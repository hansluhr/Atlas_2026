

# Tabelas Agravos de notificações Idosos 60+ -----------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/Idoso_atlas_2026_sinan_sih_sim.R")
#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop-PNADc-60+.xlsx") 
#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc_idoso <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral de idosos
  select(uf=UF,ano,pop=Pop)

#Tratando População PNADc
pop_pnadc_idoso |> 
  mutate(uf = uf |> as_factor(),
         ano = ano |> as_factor()) |> 
  #Excluindo as regiões.
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) -> pop_pnadc_idoso

#Acrescentado população Brasil
pop_pnadc_idoso |> 
  summarise(pop = sum(pop), .by = ano) |>
  mutate(uf = "Brasil" |> as_factor()) |>
  bind_rows(pop_pnadc_idoso) -> pop_pnadc_idoso


#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2014:2024;gc()

#Número de agravos de notificação de violência interpessoal de pessoas de 60 anos ou mais de idade, por UF 
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol!="Autoprovocada") |> 
  #Ano de notificação na uf de residência
  count(sg_uf, ano_not, name = "notfs") |> 
  #Excluindo UFs de residência não informadadas e rename das variáveis
  filter(!is.na(sg_uf)) |>  rename(uf=sg_uf,ano=ano_not) |> as_tibble() -> sinan_not_ufs

#### ATENÇÃO ###
#Algmas notificações não indicam a UF de residência, 
#portanto realizar o agregado nacional através do somatório das UFs de residência informa valor inferior a total de notificações nacionais.  
#É necessário contabilizar o agregado nacional e inserir na base com ufs de residência
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol!="Autoprovocada") |> 
  #Ano de notificação no Brasil
  count(ano_not, name = "notfs") |> rename(ano=ano_not) |>
  mutate(uf = "Brasil" |> as.factor()) |> tibble() -> sinan_not_br

#Bind de notificações UFs e Brasil
bind_rows(sinan_not_ufs,sinan_not_br) -> sinan_idoso
rm(sinan_not_ufs,sinan_not_br)

#Join da população pnadc idoso e notificações
left_join(pop_pnadc_idoso,sinan_idoso, by = c("ano","uf")) |>
  #Criando taxa de notificações
  mutate(tx_nots = round((notfs/pop)*100000,1)) -> sinan_idoso
rm(pop_pnadc_idoso)

#Tabela de agravos de notificação de violência interpessoal de pessoas de 60 anos ou mais de idade por UF – Brasil
sinan_idoso |> select(ano,uf,notfs) |>
  pivot_wider(names_from = ano, values_from = notfs) %>%
  #Variações 
  mutate(
    
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","), 
    
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ","),
    
    #Dez anos 
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ",") ) |>
  
  
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character), ~ str_replace_all(as.character(.x), "\\.", ",")),
         uf = uf |> as.character() ) |>
  
  #Nota de rodapé
  add_row(
    uf = glue::glue("Fonte: MS/SVSA - Sistema de Informação de Agravos de Notificação (Sinan). Elaboração: Diest/Ipea e FBSP. Notas: 1- Em alguns anos a UF de residência da vítima não
   é informada. Assim, nestes anos o somatório de notificações nas UFs é inferior ao somatório nacional. 2 - Identificação das notificações segue metodologia apresentada na seção de PCD.
   3 - Microdados do Sinan referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}") ) |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de agravos de notificação de violência interpessoal de idosos , por UF {min(year)}–{max(year)}") ) |> 
  rio::export(x = _,"base/idoso/base/n_agravos_idoso.xlsx")


#Tabela taxa de agravos de notificação de violência interpessoal idosos(60+), por UF de residência - Brasil
sinan_idoso |> select(ano,uf,tx_nots) |>
  pivot_wider(names_from = ano, values_from = tx_nots) %>%
  #Variações 
  mutate(    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","), 
    
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ","),
    
    #Dez anos 
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ",") ) |>
  
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character)|where(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ",")),
         uf = uf |> as.character() ) |>
  #Nota de rodapé
  add_row(
    uf = glue::glue("Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA - Sistema de Informação de Agravos de Notificação (Sinan). Elaboração: Diest/Ipea e FBSP. Notas: 1- Em alguns anos a UF de residência da vítima não
   é informada. Assim, nestes anos o somatório de notificações nas UFs é inferior ao somatório nacional. 2 - Identificação das notificações segue metodologia apresentada na seção de PCD.
   3 - Microdados do Sinan referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}") ) |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de agravos de notificação de violência interpessoal de idosos , por UF {min(year)}–{max(year)}") ) |> 
  rio::export(x = _,"base/idoso/base/taxa_agravos_idoso.xlsx")

rm(sinan,sinan_idoso,year)



# Tabelas lesão autoprovocadas idosos(60+)  ----------------------------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop-PNADc-60+.xlsx") 
#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc_idoso <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral de idosos
  select(uf=UF,ano,pop=Pop)


#Tratando População PNADc
pop_pnadc_idoso |> 
  mutate(uf = uf |> as_factor(),
         ano = ano |> as_factor()) |> 
  #Excluindo as regiões.
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) -> pop_pnadc_idoso

#Acrescentado população Brasil
pop_pnadc_idoso |> 
  summarise(pop = sum(pop), .by = ano) |>
  mutate(uf = "Brasil" |> as_factor()) |>
  bind_rows(pop_pnadc_idoso) -> pop_pnadc_idoso

#Removendo pnadcs, exceto pop_pnadc_idoso
rm(list = setdiff(ls(), "pop_pnadc_idoso"))


#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2014:2024;gc()

#Número de notificações de lesão autoprovocada. Usando metodologia do PCD
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol=="Autoprovocada") |> 
  #Ano de notificação na uf de residência
  count(sg_uf, ano_not, name = "notfs") |> 
  #Excluindo UFs de residência não informadadas e rename das variáveis
  filter(!is.na(sg_uf)) |> rename(uf=sg_uf,ano=ano_not) |> as_tibble() -> sinan_not_ufs

#### ATENÇÃO ###
#Algmas notificações não indicam a UF de residência, 
#portanto realizar o agregado nacional através do somatório das UFs de residência informa valor inferior a total de notificações nacionais.  
#É necessário contabilizar o agregado nacional e inserir na base com ufs de residência
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol=="Autoprovocada") |> 
  #Ano de notificação no Brasil
  count(ano_not, name = "notfs") |> rename(ano=ano_not) |>
  mutate(uf = "Brasil" |> as.factor()) |> tibble() -> sinan_not_br

bind_rows(sinan_not_ufs,sinan_not_br) -> sinan_idoso
rm(sinan_not_ufs,sinan_not_br)

#Join da população pnadc idoso e notificações
left_join(pop_pnadc_idoso,sinan_idoso, by = c("ano","uf")) |> 
  #Zero nas UF sem notificação
  mutate(notfs = replace_na(notfs,0),
         #Criando taxa de notificações
         tx_nots = round((notfs/pop)*100000,1)) -> sinan_idoso
rm(pop_pnadc_idoso)

#Tabela de notificação autoprovocada de idosos (60+)
sinan_idoso |> select(ano,uf,notfs) |>
  pivot_wider(names_from = ano, values_from = notfs) %>%
  #Variações 
  mutate(
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","), 
    
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ","),
    
    #Dez anos 
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ",") )  |>
  
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character), ~ str_replace_all(as.character(.x), "\\.", ",")),
         uf = uf |> as.character() ) |>
  #Nota de rodapé
  add_row(
    uf = glue::glue("Fonte: MS/SVSA - Sistema de Informação de Agravos de Notificação (Sinan). Elaboração: Diest/Ipea e FBSP. Notas: 1- Em alguns anos a UF de residência da vítima não
   é informada. Assim, nestes anos o somatório de notificações nas UFs é inferior ao somatório nacional. 2 - Identificação das notificações segue metodologia apresentada na seção de PCD.
   3 - Microdados do Sinan referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}") ) |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de agravos de notificação de lesão autoprovocada de idosos, por UF {min(year)}–{max(year)}") ) |> 
  rio::export(x = _,"base/idoso/base/n_autoprovocadas_idoso.xlsx")


#Tabela taxa de notificação violência autoprovocada de idosos (60+), por UF de residência - Brasil
sinan_idoso |> select(ano,uf,tx_nots) |>
  pivot_wider(names_from = ano, values_from = tx_nots) %>%
  #Variações 
  mutate(
    
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","), 
    
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ","),
    
    #Dez anos 
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ",") )  |>
  
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character)|where(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ",")),
         uf = uf |> as.character() ) |>
  #Nota de rodapé
  add_row(
    uf = glue::glue("Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA - Sistema de Informação de Agravos de Notificação (Sinan). Elaboração: Diest/Ipea e FBSP. Notas: 1- Em alguns anos a UF de residência da vítima não
   é informada. Assim, nestes anos o somatório de notificações nas UFs é inferior ao somatório nacional. 2 - Identificação das notificações segue metodologia apresentada na seção de PCD.
   3 - Microdados do Sinan referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}") ) |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de agravos de notificação de lesão autoprovocada de idosos, por UF {min(year)}–{max(year)}") ) |> 
  rio::export(x = _,"base/idoso/base/taxa_autoprovocadas_idoso.xlsx")

rm(sinan,sinan_idoso,year)





# Internações -------------------------------------------------------------
library(duckplyr)
con <- DBI::dbConnect(duckdb::duckdb(),
                      dbdir = paste0(dirname(getwd()),"/bases/sih/duckdb/sih_2008_2025.duckdb"), #Nome do database que armazena o SIH
                      read_only = FALSE)
DBI::dbListTables(con)

data <- 
  tbl(con, "sih")
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Base das internações
data |>
  
  mutate(idade = idade |> as.integer() ) |>
  
  #Mantém período de interesse, idade e raça\cor de interesse
  filter(ano_inter %in% year & idade >= 60 & def_raca_cor!= "Label não definido") |>
  
  #Elaboração de internações por agressão
  #Pegar primeira letra do diagnóstico principal. E utilizar o filtro dos caps XIX e XX
  mutate(causa_letra_dig_pri = substr(diag_princ,1,1) ) |> 
  #Mantém caps XIX e XX na variável diagnóstico principal.
  #Diag secundário com cap XIX ou XX é excluído, seguindo a descrição do manual.
  filter(causa_letra_dig_pri %in% c("S","T","V", "W", "X", "Y") & 
         ident != 5) |>
  #Criar variável CID10 Causa externa.
  mutate(cid_externa = case_when(
    #Quando diag_princ for cap XX, então cid10 da causa externa externa é o diag_princ
    causa_letra_dig_pri %in% c("V", "W", "X", "Y") ~ diag_princ, 
    #Quando diag_princ for cap XIX e o primeiro diagnóstico secundário for cap XX
    #então a cid10 da causa externa é o primeiro diagnóstico secundário.
    causa_letra_dig_pri %in% c("S","T") & substr(diagsec1,1,1) %in% c("V", "W", "X", "Y") ~ diagsec1,
    #O primeiro diagnóstico secundário não é cap XX. Então a cid10 da causa externa é o diag_princ
    .default = diag_princ) )  |>
  
  #Na variável causa externa, mantendo somente AIHs com cid10_externa no capítulo XX.
  filter(substr(cid_externa,1,1) %in% c("V", "W", "X", "Y") )  |>
  #Elimina variável não utilizada
  select(!c(causa_letra_dig_pri) ) |>
  
  collect() |>
  
  #Intenção, instrumento e local do incidente
  mutate(
    #Causa e loca do óbito.
    causa_letra = substr(cid_externa,1,1),
    causa_num = as.numeric(substr(cid_externa,2,3)),
    local_obito = as.numeric(substr(cid_externa,4,4)),
    #Intenção
    intencao = case_when(
      ###acidente(==1)
      #acidente-envenenamento
      (causa_letra  == "X" & causa_num > 39 & causa_num < 50)  |
        #acidente-enforcamento
        (causa_letra  == "W" & causa_num > 74 & causa_num < 77) | #/*Certo*/
        #acidente-afogamento
        (causa_letra  == "W" & causa_num > 64 & causa_num < 75) | #/*Certo*/
        #acidente-impacto /*Queda*/
        (causa_letra  == "W" & causa_num < 25 | 
           causa_letra  == "W" & causa_num >26 & causa_num < 32 | 
           causa_letra  == "W" & causa_num >34 & causa_num < 44 | 
           causa_letra  == "W" & causa_num == 49) |
        #acidente-fogo
        (causa_letra  == "X" & causa_num < 10) | 
        #acidente-contundente
        (causa_letra  == "W" & causa_num == 51 | causa_letra  == "W" & causa_num == 50) |
        #acidente-veiculo
        (causa_letra  == "V" &  causa_num > 0) | # /*Certo*/
        #acidente-PAF
        (causa_letra  == "W" & causa_num > 31 & causa_num < 35)  | #/*Estava em Indeterminado*/
        #Acidente - Perfurante   
        (causa_letra  == "W" & causa_num > 24 & causa_num < 27)  | #/*Estava em Indeterminado*/
        #Acidente - Desconhecido /*X:59 ? acidentes não especificados e pode contem homicídio mal registrado, visto no artigo sobre qualidade da declaração de óbito em SP.*/
        (causa_letra  == "X" & causa_num > 57 & causa_num < 60) ~ "Acidente", #/*Categoria inseriada, x58-x59 estavam em intencao outros.*/
      
      ##Sucidídio
      #suicidio-envenenamento
      (causa_letra  == "X" & causa_num > 59 & causa_num < 70) | # /*Certo*/
        #suicidio-enforcamento
        (causa_letra  == "X" & causa_num == 70) | # /*Certo*/
        #suicidio-afogamento
        (causa_letra  == "X" & causa_num == 71) | #/*Certo*/
        #suicidio-PAF
        (causa_letra  == "X" & causa_num > 71 & causa_num < 75) | # /*Certo*/
        #suicidio-impacto/veículo
        (causa_letra  == "X" & causa_num == 75 | causa_letra  == "X" & causa_num > 79 & causa_num < 83) | #/*Certo*/
        #suicidio-fogo
        (causa_letra  == "X" & causa_num > 75 & causa_num < 78) | # /*Certo*/
        #suicidio-perfurante
        (causa_letra  == "X" & causa_num ==78)  | #/*Certo*/
        #suicidio-contundente
        (causa_letra  == "X" & causa_num ==79) | #/*Certo*/
        #suicidio-desconhecido
        (causa_letra  == "X" & causa_num > 82 & causa_num < 85) ~ "Suicídio",
      
      ##homicidio (==3)
      #homicidio-envenenamento
      (causa_letra  == "X" & causa_num > 84 & causa_num < 91) | # /*Certo*/
        #homicidio-enforcamento
        (causa_letra  == "X" & causa_num == 91) | #/*Certo*/
        #homicidio-afogamento
        (causa_letra  == "X" & causa_num == 92) | #/*Certo*/
        #homicidio-PAF
        (causa_letra  == "X" & causa_num > 92 & causa_num < 96) | #/*Certo*/
        #homicidio-impacto/Veículo
        (causa_letra  == "X" & causa_num == 96 | 
           causa_letra  == "Y" & causa_num > 0 & causa_num < 04) | #/*Veículo aqui*/
        #homicidio-fogo
        (causa_letra  == "X" & causa_num > 96 & causa_num < 99) | # /*Certo*/
        #homicidio-perfurante
        (causa_letra  == "X" & causa_num == 99) | #/*Certo*/
        #homicidio-contundente
        (causa_letra  == "Y" & causa_num == 0 | 
           causa_letra  == "Y" & causa_num > 03 & causa_num < 06) | # /*Certo*/
        #homicidio-desconhecido
        (causa_letra  == "Y" & causa_num > 05 & causa_num < 10) ~ "Homicídio",
      
      #indeterminado (==4)
      #indeterminado-envenenamento
      (causa_letra  == "Y" & causa_num > 09 & causa_num < 20) | #/*Certo*/
        #indeterminado-enforcamento
        (causa_letra  == "Y" & causa_num == 20) | #/*Certo*/
        #indeterminado-afogamento
        (causa_letra  == "Y" & causa_num == 21) | #/*Certo*/
        #indeterminado-PAF
        (causa_letra  == "Y" & causa_num > 21 & causa_num < 25) | 
        #indeterminado-impacto/Ve?culo
        (causa_letra  == "Y" & causa_num == 25 | causa_letra  == "Y" & causa_num == 30 | 
           causa_letra  == "Y" & causa_num == 31 | causa_letra  == "Y" & causa_num == 32) | #/*Ve?culo aqui*/
        #indeterminado-fogo
        (causa_letra  == "Y" & causa_num > 25 & causa_num < 28) | #/*Certo*/
        #indeterminado-perfurante
        (causa_letra  == "Y" & causa_num == 28) | #/*| causa_letra  == "W" & causa_num > 24 & causa_num < 27 Foi para Acidente perfurante*/
        #indeterminado-contundente
        (causa_letra  == "Y" & causa_num == 29) | #/*Certo*/
        #indeterminado-desconhecido
        causa_letra  == "Y" & causa_num > 32 & causa_num < 35 ~ "Indeterminado", 
      
      #h_legal (==6) interven??es legais e execuexecu??eses legais. Essa categoria n?o entrar? no Mlogit, ? apenas para validar o total de ?bitos
      causa_letra  == "Y" & causa_num == 35 | causa_letra  == "Y" & causa_num == 36 ~ "h_legal",
      
      #outros (==7) Essa categoria nao entrara no Mlogit, ? apenas para validar o total de ?bitos
      #/*Penetra??o de corpo estranho no ou atrav?s de olho ou orif?cio natural + Penetra??o de corpo ou objeto estranho atrav?s da pele + Contato com agulha hipod?rmica*/
      causa_letra  == "W" & causa_num > 43 & causa_num < 47  | 
        #/* Esmagado, empurrado ou pisoteado por multid?o ou debandada em massa de pessoas + ... + Afogamento e submers?o durante banho em banheira*/
        causa_letra  == "W" & causa_num > 51 & causa_num < 66  |
        #/* Risco a respira??o devido a desmoronamento, queda de terra e de outras subst?ncias + ... + Exposi??o ? corrente el?trica, ? radia??o e ?s temperaturas e press?es extremas do ambiente*/ 
        causa_letra  == "W" & causa_num > 76  | 
        #/*Contato com uma fonte de calor ou com subst?ncias quentes + Contato com animais e plantas venenosos + Exposi??o ?s for?as da natureza*/
        causa_letra  == "X" & causa_num > 09 & causa_num < 40 | 
        #/*X:59 ? acidentes n?o especificados e pode contem homic?dio mal registrado, visto no artigo sobre qualidade da declara??o de ?bito em SP.*/
        #/*x58-x59 foi para acidente de instrumento desconhecido*/
        #/*Excesso de esfor?os, viagens e priva??es + Exposi??o acidental a outros fatores e aos n?o especificados - */
        causa_letra  == "X" & causa_num > 49 & causa_num < 60 | 
        #/*Excesso de esfor?os, viagens e priva??es*/
        causa_letra  == "X" & causa_num > 49 & causa_num < 58 | 
        #/*Complica??es de assist?ncia m?dica e cir?rgica + ... + Seq?elas de causas externas de morbidade e de mortalidade*/
        causa_letra  == "Y" & causa_num > 39 & causa_num < 90 ~ "Outros"), 
    
    
    ###Instrumento (Dicion?rio: 1=Envenenamento; 2=Enforcamento; 3=Afogamento; 4=PAF; 5=Impacto; 6=Fogo; 7=Perfurante; 8=Contundente; 9=Desconhecido; 10=veiculo)
    instrumento = case_when(
      #Envenenamento (==1) 
      causa_letra  == "X" & causa_num > 39  & causa_num < 50 | # /*Acidente envenenamento*/
        causa_letra  == "X" & causa_num > 59 & causa_num < 70 |  #/*Self harm envenenamento*/
        causa_letra  == "X" & causa_num > 84 & causa_num < 91 |  #/*Ag. envenenamento*/
        causa_letra  == "Y" & causa_num > 09 & causa_num < 20 ~ "Envenenamento",       #/*Ind. envenenamento*/
      
      #Enforcamento (==2) Tudo Certo
      causa_letra  == "W" & causa_num > 74 & causa_num < 77 |  #/*Acidente enforcamento*/
        causa_letra  == "X" & causa_num == 70 |  #/*Self harm enforcamento*/
        causa_letra  == "X" & causa_num == 91 |  #/*Ag. enforcamento*/
        causa_letra  == "Y" & causa_num == 20 ~ "Enforcamento", #/*Ind. Enforcamento*/ 
      
      
      #Afogamento (==3) Por que afogamento de intencao outros? (Categoria Sequelas?)
      causa_letra  == "W" & causa_num > 64 & causa_num < 75 | # /*Acidente afogamento*/
        causa_letra  == "X" & causa_num == 71 |  # /*Self harm afogamento*/
        causa_letra  == "X" & causa_num == 92 |  # /Ag. afogamento*/
        causa_letra  == "Y" & causa_num == 21 ~ "Afogamento", # /*Ind. afogamento*/
      
      #PAF (==4) - N?o tem acidente por arma de fogo
      causa_letra  == "W" & causa_num > 31 & causa_num < 35 | #Acidente - PAF*/
        causa_letra  == "X" & causa_num > 71 & causa_num < 75 |  #/*Self harm - PAF*/
        causa_letra  == "X" & causa_num > 92 & causa_num < 96 |  #/*ag. - PAF*/
        causa_letra  == "Y" & causa_num > 21 & causa_num < 25 |  #/*Ind. - PAF*/
        causa_letra  == "Y" & causa_num ==35 &  local_obito == 0 ~ "PAF",  #/*h_legal - PAF*/
      #causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 - Foi para instrumento fogo
      
      #Impacto (==5) - Olhar acidente impacto
      causa_letra  == "W" & causa_num < 25 | # /Acidente: Queda de altura + atingido por objeto + esmagado por objeto*/ 
        causa_letra  == "W" & causa_num >26 & causa_num < 32 |  #/*2Acidente: Contato com objetos*/ 
        causa_letra  == "W" & causa_num >34 & causa_num < 44 |  #/*3Acidente: Explos?o + fogos + Exposi??o a jato press?o, barulho e vibra??o*/ 
        causa_letra  == "W" & causa_num ==49 |  #/*Acidente: Exposi??o for?a mec indeterminada.*/ 
        causa_letra  == "X" & causa_num == 75 |  #/*Self harm: Explos?o*/ 
        causa_letra  == "X" & causa_num > 79 & causa_num < 82 |  #/*Self harm: Queda + deitar na frente de objeto movendo.*/ 
        causa_letra  == "X" & causa_num ==96 |  #/*Agress?o mat explossivo*/ 
        causa_letra  == "Y" & causa_num > 0 & causa_num < 03 |  #/*Ag. empurado de altura + colocado na frente de objeto movendo.*/ 
        causa_letra  == "Y" & causa_num == 25 |  #/*Ind. Explos?o*/ 
        causa_letra  == "Y" & causa_num == 30 |  #/*Ind. Queda altura indet*/ 
        causa_letra  == "Y" & causa_num == 31 ~ "Impacto",  #/*Ind. Queda + deitar na frente de objeto movendo indet.*/
      
      #Fogo (==6) 
      causa_letra  == "X" & causa_num < 10 |  #/*Acidente Exposi??o a fuma?a, fogo e chamas*/  
        causa_letra  == "X" & causa_num > 75 & causa_num < 78 |  #/*Self harm de fuma?a, fogo, chamas, vapor*/ 
        causa_letra  == "X" & causa_num > 96 & causa_num < 99 |  #/*Ag. de fuma?a, fogo, chamas, vapor */ 
        causa_letra  == "Y" & causa_num > 25 & causa_num < 28 |  #/*Ind. de fuma?a, fogo, chamas, vapor */ 
        causa_letra  == "Y" & causa_num == 35 & local_obito ==2 |  #/*h_legal involvendo fuma?a*/
        causa_letra  == "Y" & causa_num ==35 &  local_obito == 1 ~ "Fogo", # /*h_legal involvendo explos?o*/
      
      #Perfurante (==7) 
      causa_letra  == "X" & causa_num ==78 |  #/*self objeto afiado*/ 
        causa_letra  == "X" & causa_num ==99 |  #/*ag. objeto afiado*/ 
        causa_letra  == "Y" & causa_num ==28 |  #/*Ind. objeto afiado*/ 
        causa_letra  == "W" & causa_num > 24 & causa_num < 27 |  #/*Acidente objeto afiado. Estava indo para indeterminado*/ 
        causa_letra  == "Y" & causa_num == 35 & local_obito ==4 ~ "Perfurante", #/*h_legal objeto afiado*/
      
      #Contundente (==8) 
      causa_letra  == "W" & causa_num ==51 |  #/*Acidente - Colis?o entre duas pessoas*/ 
        causa_letra  == "X" & causa_num ==79 |  #/*self por objeto contundente*/ 
        causa_letra  == "Y" & causa_num ==0 |  #/*ag. por objeto contundente*/ 
        causa_letra  == "Y" & causa_num > 03 & causa_num < 06 |  #/*Ag. por meio de for?a corporal + Ag. sexual por meio de for?a f?sica*/ 
        causa_letra  == "W" & causa_num == 50 |  #/*Acidente - Golpe, pancada, ponta p?*/
        causa_letra  == "Y" & causa_num == 29 |  #/*Ind. Objento contundente*/ 
        causa_letra  == "Y" & causa_num == 35 & local_obito ==3 ~ "Contundente", #/*h_legal objeto contundente*/ 
      
      #Desconhecido (==9) A segunga categoria contém negligência que não é desconhecida. Cadê acidente
      causa_letra  == "X" & causa_num > 82 & causa_num < 85 |  #/*self. Outros meios especificados + self outros meios n?o especificados*/
        causa_letra  == "Y" & causa_num > 05 & causa_num < 10 |  #/*Ag. Neglig?ncia + Ag. Outros maus tratos + Ag. Outros meios especificados + Ag. outros meios n?o especificados*/
        causa_letra  == "Y" & causa_num > 32 & causa_num < 35 |  #/*Ind. Outros fatos ou eventos espcificados + fatos ou eventos n?o espcificados*/
        causa_letra  == "Y" & causa_num == 35 & local_obito ==5 |  #/*h_legal Execu??o legal - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
        causa_letra  == "Y" & causa_num == 35 & local_obito == 6 |  #/*h_legal Execu??o legal por outros meios especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
        causa_letra  == "Y" & causa_num == 35 & local_obito ==7 |  #/*h_legal Execu??o legal por meios n?o especificados - N?o ? desconhecido, mas deve ser zero. Pena de morte*/
        causa_letra  == "Y" & causa_num == 36 | # /*Opera??es de guerra*/
        causa_letra  == "X" & causa_num > 57 & causa_num < 60 ~ "Desconhecido", #/*Acidente instrumento desconhecido. Categoria inseriada, não estava na rotina.*/
      
      
      #veículo (==10) 1.Acidente 2.Homicídio (y03, impacto) , 3.Indeterminado (y32,impacto) 4.Suicídio(x82,impacto)
      causa_letra  == "V" & causa_num > 0 | causa_letra  == "Y" & causa_num == 03 | 
      causa_letra  == "Y" & causa_num == 32 | causa_letra  == "X" & causa_num == 82 ~ "Veículo"),
    
    intencao_homic = recode_values(intencao, 
                                   c("Homicídio","h_legal") ~ "Homicídio")  )  |> 
  
  #Mantém somente intenções de interesse
  filter(intencao %in% c("Homicídio","h_legal") ) -> base

DBI::dbDisconnect(con) ; gc()
rm(list = setdiff( ls(),c("base","year") ) ); gc()
beepr::beep()

methods_restore()
library(tidyverse)


#Contagem de internações
base |>
  #Alteração para negro e não negro
  mutate(def_raca_cor = replace_values(def_raca_cor,
                                       c("Amarela", "Branca", "Indígena") ~ "Não Negro",
                                       c("Parda", "Preta") ~ "Negro" ),
         sexo_raca = case_when(
           def_sexo == "Homem" & def_raca_cor == "Negro"     ~ "h_negro",
           def_sexo == "Homem" & def_raca_cor == "Não Negro" ~ "h_nao_negro",
           def_sexo == "Mulher" & def_raca_cor == "Negro"    ~ "m_negra",
           def_sexo == "Mulher" & def_raca_cor == "Não Negro"~ "m_nao_negra") )  |>
  count(def_uf_resd, ano_inter, sexo_raca, name = "n_inter") -> inter

#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop-PNADc-60+.xlsx") 

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Neste caso, tail informa o último ano
  .x = tail(readxl::excel_sheets(excel_pnadc), 1),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população homem negro, homem não negro, mulher negro e mulher não negro
  select(uf=UF,ano, h_negro = H.Negro, h_nao_negro = H.Não_Negra, m_negra = M.Negro, m_nao_negra = M.Não_Negra) |>
  
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
  
  mutate(
    ano = ano |> as_factor()) |> 
  
  #Formato Long para facilitar o join
  pivot_longer(cols = !c(uf,ano), names_to = "def_racacor", names_transform = list(def_racacor = as.factor),
               values_to = "pop_pnadc") |>
  
  #População Brasil
  summarise(pop_pnadc = sum(pop_pnadc), .by = c(ano,uf, def_racacor) )  |>
  #Mantém o último ano
  filter(ano %in% year) 
rm(excel_pnadc)


  
pop_pnadc |>
  left_join(x = _, y = inter, 
            by = join_by("ano" == "ano_inter","uf" == "def_uf_resd", 
                         "def_racacor" == "sexo_raca" ) ) |>
  select(!c(ano) ) %>%

    #Acrescentando total Brasil e criando taxa e proporção
    bind_rows(. |>
            summarise(uf ="Brasil",
                      pop_pnadc = sum(pop_pnadc),
                      n_inter = sum(n_inter, na.rm = TRUE), .by = c(def_racacor) ) ) |> 
  
  #Taxa de internações por agressão, por uf de residência
  mutate(n_inter = n_inter |> replace_na(0),
         tx_inter = round( (n_inter/pop_pnadc)*100000,1) ) |> 
  
  #Formato wide
  pivot_wider(id_cols = uf,
              names_from = def_racacor,
              values_from = c(n_inter, tx_inter) ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf) )  |>
  
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  
  #Ordenação das colunas
  select(uf,n_inter_h_negro, tx_inter_h_negro,n_inter_h_nao_negro, tx_inter_h_nao_negro,
         
         n_inter_m_negra, tx_inter_m_negra, n_inter_m_nao_negra, tx_inter_m_nao_negra ) |>
  #Nota de rodapé
  add_row(
    uf = glue::glue("Fonte: Ministério da Saúde - Sistema de Informações Hospitalares do SUS (SIH/SUS) e IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc). Elaboração: Diest/Ipea
    e FBSP. Nota: O número de internações por agressão na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, agressões, intervenção legal e operações de guerra.") ) |> 
  #Título da Tabela
  janitor::adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número e taxa de internações de idosos por agressões por 100 mil habitantes, por sexo e raça/cor {(year)}") ) |> 
  rio::export(x = _,"base/idoso/base/intenacoes_agressao_idoso.xlsx")

  
rm(list = ls() );gc()



# Mortes por causa externa - Homicídio, Quedas e Acidentes de Tran --------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Idoso_atlas_2026_sinan_sih_sim.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#Anos de interesse. Últimos dez anos.
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 10, as.integer(format(Sys.Date(), "%Y")) - 2)

#Homicídio idoso raça\cor
sim_doext |> 
  #Refazendo raçacor e acrescentando racacor negra e não negra nos microdado do SIM 
  mutate(def_racacor = replace_values(def_racacor, 
                                      c("Parda","Preta") ~ "Negro",
                                      c("Amarela","Branca","Indigena") ~ "Não Negro") |> as_factor() ) |> 
  #Filtro das características desejadas
  filter(idade>=60 & intencao_homic == "Homicídio") |> 
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,def_sexo,def_racacor, name = "Homicídio", .drop = FALSE ) |> 
  #Filtro para eliminar anos, sexo e racacor não investigada.
  filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |> 
  #Deixando racacor igual ao informando em pop_pnadc_idoso
  mutate(def_racacor =  case_when(def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
                                  def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
                                  def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
                                  def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,def_racacor,Homicídio) -> homic

#Quedas raça cor
#cid10 de quedas é w00 a w19
sim_doext |> 
  #Refazendo raçacor e acrescentando def_racacor negra e não negra nos microdado so SIM 
  mutate(def_racacor = case_match(def_racacor,
                                  c("Parda","Preta") ~ "Negro",
                                  c("Amarela","Branca","Indigena") ~ "Não Negro",.default = def_racacor) |> as_factor()) |> 
  #Filtro das características desejadas
  filter(idade>=60 & (causa_letra == "W" & causa_num %in% c(0:19))) |> #cid10 de quedas é w00 a w19
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,def_sexo,def_racacor, name = "Queda", .drop = FALSE ) |> 
  #Filtro para eliminar anos, def_sexo e def_racacor não investigada. 
  filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |>
  #Deixando def_racacor igual ao informando em pop_pnadc_idoso
  mutate(def_racacor =  case_when(
    def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
    def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
    def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
    def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,def_racacor,Queda) -> queda


#Acidente de trânsito
#Cid 10 de sinistro de trânsito é V01 a V99
sim_doext |>
  #Refazendo raçacor e acrescentando def_racacor negra e não negra nos microdado do SIM 
  mutate(def_racacor = case_match(def_racacor, c("Parda","Preta") ~ "Negro",
                                  c("Amarela","Branca","Indigena") ~ "Não Negro",.default = def_racacor) |> as_factor()) |> 
  #Filtro das características desejadas
  filter(idade>=60 & def_racacor != "Ignorado" & (causa_letra == "V" & causa_num %in% c(0:99))) |> #cid10 de acidente transporte é V01 a V99
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,def_sexo,def_racacor, name = "trans", .drop = FALSE ) |> 
  #Filtro para eliminar anos, def_sexo e def_racacor não investigada. 
  filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |> 
  #Deixando def_racacor igual ao informando em pop_pnadc_idoso
  mutate(def_racacor =  case_when(def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
                                  def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
                                  def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
                                  def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,def_racacor,trans) -> trans

#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop-PNADc-60+.xlsx") 

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população homem negro, homem não negro, mulher negro e mulher não negro
  select(uf=UF,ano, h_negro = H.Negro, h_nao_negro = H.Não_Negra, m_negra = M.Negro, m_nao_negra = M.Não_Negra) |>
  
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
  
  mutate(
    ano = ano |> as_factor()) |> 
  
  #Formato Long para facilitar o join
  pivot_longer(cols = !c(uf,ano), names_to = "def_racacor", names_transform = list(def_racacor = as.factor),
               values_to = "pop_pnadc") |>
  
  #População Brasil
  summarise(pop_pnadc = sum(pop_pnadc), .by = c(ano,def_racacor) )  
rm(excel_pnadc)


#Join de população, homicídio, Quedas e acidetne de transporte
list(pop_pnadc,homic,queda,trans) %>% reduce(left_join, by = c("ano","def_racacor")) |>
  #Taxas de homicídio, Acidente e Transporte IDOSO. Padrão Atlas, somente um casa decimal.
  mutate(across(c(Homicídio, Queda, trans), ~ round((./pop_pnadc)*100000,1),.names="tx_{col}")) |>
  #Mantém variáveis utilizadas
  select(ano,def_racacor,starts_with("tx")) -> base
rm(homic,queda,trans,pop_pnadc)

#Exportação das taxas de interesse
base |>
  mutate(def_racacor = def_racacor |> fct_relevel("h_negro","h_nao_negro","m_negra","m_nao_negra")) |>
  arrange(def_racacor) |> 
  pivot_longer(cols = starts_with("tx"), names_to = "taxas") |>
  pivot_wider(names_from = ano, values_from = "value") |>
  arrange(taxas) 





  rio::export(x = _, "base/idoso/base/tx_homic_queda_acide_idoso.xlsx")



