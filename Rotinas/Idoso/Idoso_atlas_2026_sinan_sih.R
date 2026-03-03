

# Tabelas Agravos de notificações Idosos 60+ -----------------------------------------
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
