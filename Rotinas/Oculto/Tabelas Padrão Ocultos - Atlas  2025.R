# Homicídio Oculto e projetado, por UF e Brasil--------------------------------------------------------
library(tidyverse)
library(janitor)
library(glue)

#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)
ano <- paste(min(year), max(year), sep = "-")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd,intencao_homic, name = "homicidio") |>
  #Mantém variáveis utlizadas
  select(!c(intencao_homic) ) -> homic

#Contagem de homicídios ocultos, por ano e UF
homic_preds |> 
  #Filtrando homicídios ocultos
  filter(.pred_class == "homic" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
  #Mantém variáveis utlizadas
  select(!c(.pred_class)) -> ocult

#Juntando base homicídio registrado a homicídio coulto
base <- left_join(homic,ocult, by = c("ano","uf_resd")) |> 
  #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
  mutate(across(where(is.numeric),~replace_na(.,0)),
         #Homicídios projetados
         homicidio_proj = homicidio + ppb2_homicidio)
rm(homic,ocult)

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |> 
  
  #Selecionando população geral
  select(UF,ano,pop = Pop)
rm(excel_pnadc)

#Tratamento base com população PNADc
pop_pnadc |> rename(uf_resd = UF) |>
  #Excluindo as regiões. Não utilizado
  filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf_resd, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .before=uf_resd,
         #Transofrmando em factor variáveis desejadas
         ano = ano |> as_factor(),
         uf_resd = uf_resd |> as_factor() ) -> pop_pnadc

#Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
left_join(pop_pnadc,base, by = c("ano","uf_resd"))  -> base
rm(pop_pnadc)

#Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
base |>
  summarise(uf_resd="Brasil" |> as.factor(),
            pop = sum(pop),
            homicidio = sum(homicidio),
            ppb2_homicidio = sum(ppb2_homicidio),
            homicidio_proj = sum(homicidio_proj),.by=ano) |>
  #Bind_row de Brasil a base com UFs.
 bind_rows(base) -> base

#Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
base %>%
  mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop)*100000,.names="tx_{col}"),
         #Padrão Atlas. Taxas com uma casa decimal
         across(starts_with("tx_"),~round(.,digits = 1))) |> 
  rename(tx_oculto = tx_ppb2_homicidio) -> base

### Tabelas no formato atlas da violência
#Homicídio oculto - Valor absoluto
base |> select(ano,uf_resd,oculto = ppb2_homicidio) |>
  
  pivot_wider(names_from = ano,values_from = oculto) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top", col_name = glue::glue("Número de homicídios ocultos, por UF – Brasil {ano}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_oculto_uf_br.xlsx")

#Taxa de Homicídio oculto 
base |> select(ano,uf_resd,tx_oculto) |>
  
  pivot_wider(names_from = ano,values_from = tx_oculto) %>%
  
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = glue::glue("Taxa de homicídios ocultos, por UF – Brasil {ano}") ) %>% 
  rio::export(.,"taxa_homicidio_oculto_uf_br.xlsx")


#Homicídio estimado - Valor absoluto
base |> select(ano,uf_resd,homicidio_proj) |>
  
  pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
  
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = glue("Número de homicídios projetados, por UF – Brasil {ano}") ) %>% 
  rio::export(.,"n_homicidio_estimados_uf_br.xlsx")

#Taxa de homicídio estimado
base |> select(ano,uf_resd,tx_homicidio_proj) |>
  
  pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = glue("Taxa de homicídios projetados, por UF – Brasil {ano}") ) %>% 
  
  rio::export(.,"taxa_homicidio_estimado_uf_br.xlsx")

rm(base,homic_preds,sim_doext,ano,year)


# Homicídio projetado jovens 15 a 29 anos. --------------------------------
library(tidyverse)
library(janitor)
library(glue)
#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)
ano <- paste(min(year), max(year), sep = "-")

#Contagem de homicídios registrados de jovens, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & idade %in% c(15:29) & ano %in% year) |> droplevels() |>
  count(ano,cod_uf_resd,uf_resd, name = "homicidio") -> homic

#Contagem de homicídios ocultos, por ano e UF
homic_preds |> 
  #Filtrando homicídios ocultos
  filter(.pred_class == "homic" & idade %in% c(15:29) & ano %in% year) |> droplevels() |>
  count(ano,uf_resd, name = "ppb2_homicidio")  -> ocult

#Juntando base homicídio registrado a homicídio coulto
base <- left_join(homic,ocult, by = c("ano","uf_resd")) |> 
  #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
  mutate(across(where(is.numeric),~replace_na(.,0)),
         #Homicídios projetados
         homicidio_proj = homicidio + ppb2_homicidio) 
rm(homic,ocult)

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Jovens_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral
  select(UF,ano,Pop)
rm(excel_pnadc)

#Tratamento base com população PNADc
pop_pnadc |> rename(uf = UF) |>
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .before=uf,
         #Transofrmando em factor variáveis desejadas
         ano = ano |> as_factor(),
         uf = uf |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = base, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio),
                        ppb2_homicidio = sum(ppb2_homicidio),
                        homicidio_proj = sum(homicidio_proj),.by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj), ~ round( (./Pop)*100000, 1) ,.names="tx_{col}") ) -> base
  

### Tabelas no formato atlas da violência
#Homicídio Projetado jovem - Valor absoluto
base |> select(ano,uf_resd,homicidio_proj) |>
    pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
    #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = glue("Número de homicídios estimados de jovens de 15 a 29 anos, por UF – Brasil {ano}") ) %>% 
  
  rio::export(.,"n_homicidio_jovem_estimado_uf_br.xlsx")

#Taxa de homicídio estimado de jovens
base |> select(ano,uf_resd,tx_homicidio_proj) |>
  pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = glue("Taxa de homicídios projetados, por UF – Brasil {ano}") ) %>% 
  rio::export(.,"taxa_homicidio_jovem_projetado_uf_br.xlsx")
rm(list = ls())

# Homicídio estimado mulher ----------------------------------------------
library(tidyverse)
library(janitor)
library(glue)
#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)
ano <- paste(min(year), max(year), sep = "-")

#Contagem de homicídios registrados de mulheres, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & sexo == "Mulher" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd, cod_uf_resd, name = "homicidio")  -> homic

#Contagem de homicídios ocultos mulheres, por ano e UF
homic_preds |> 
  #Filtrando homicídios ocultos
  filter(.pred_class == "homic" & sexo == "Mulher" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd, cod_uf_resd, name = "ppb2_homicidio") -> ocult

#Juntando base homicídio registrado a homicídio oculto
base <- left_join(homic,ocult, by = c("ano","uf_resd","cod_uf_resd")) |> 
  #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
  mutate(across(where(is.numeric),~replace_na(.,0)),
         #Homicídios projetados
         homicidio_proj = homicidio + ppb2_homicidio) 
rm(homic,ocult)

##Importando população de indígenas
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população mulher
  select(uf = UF,ano, pop = PoP.Mulher)
rm(excel_pnadc)

#Tratamento base com população PNADc
pop_pnadc |> 
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .before=uf,
         #Transofrmando em factor variáveis desejadas
         ano = ano |> as_factor(),
         uf = uf |> as_factor() ) -> pop_pnadc

#Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
left_join(x = pop_pnadc, y =  base, by = join_by("ano", "uf" == "uf_resd", "cod_ibge" == "cod_uf_resd")  )  |> rename(uf_resd = uf) |>
  select(!c(cod_ibge) ) -> base
rm(pop_pnadc)


#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio),
                        ppb2_homicidio = sum(ppb2_homicidio),
                        homicidio_proj = sum(homicidio_proj), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj), ~ round( (./pop)*100000, 1) ,.names="tx_{col}") ) -> base


### Tabelas no formato atlas da violência
#Taxa de homicídio estimado - mulheres
base |> select(ano,uf_resd,homicidio_proj) |>
  pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  
  adorn_title(placement = "top",col_name = "Número de homicídios estimados de Mulher, por UF – Brasil {ano}") %>% 
  
  rio::export(.,"n_homicidio_mulher_estimados_uf_br.xlsx")

#Taxa de homicídio Projetado Mulher
base |> select(ano,uf_resd,tx_homicidio_proj) |>
  pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de Mulher, por UF – Brasil {ano}") %>% 
  
  rio::export(.,"taxa_homicidio_estimado_mulher_uf_br.xlsx")
rm(list = ls() )


# Homicídio estimado indígena ---------------------------------------------
library(tidyverse)
library(janitor)
library(glue)

##Importando população de indígenas
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  .f = ~ readxl::read_xlsx(path = excel_pnadc, sheet = .x) |> 
    
    #Seleciona Variáveis de interesse.  
    select(uf=UF, ano, pop = Pop.Indígena) ) |>
  
  #Tratamento base com população PNADc
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
  
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .before=uf,
         #Transofrmando em factor variáveis desejadas
         ano = ano |> as_factor(),
         uf = uf |> as_factor() ) 
rm(excel_pnadc)

#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)
ano <- paste(min(year), max(year), sep = "-")

#Homicídios registrado - Indígenas
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & racacor == "Indigena" & ano %in% year) |> 
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic
 
#Homicídio oculto - Indígenas
homic_preds |>
  #Filtrando homicídios ocultos
  filter(.pred_class == "homic" & racacor == "Indigena" & ano %in% year) |>  
  count(ano, cod_uf_resd, uf_resd,  name = "ppb2_homicidio") -> ocult

#Join das bases de população, registrados e ocultos.
reduce(.x =
       #Bases de interesse 
       list(pop_pnadc |> rename(cod_uf_resd = cod_ibge, uf_resd = uf),
        homic, ocult),
       .f = left_join, 
       by = c("ano","cod_uf_resd","uf_resd") ) |> 
    #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto.  
    mutate(across(where(is.numeric),~replace_na(.,0)),
       #Homicídios projetados
       homicidio_proj = homicidio + ppb2_homicidio) |>
  #Mantém variáveis de interesse
  select(!c(cod_uf_resd) ) %>% 
  
#Adicionando total Brasil
 bind_rows(. |>
            summarise(uf_resd="Brasil" |> as.factor(),
                      pop = sum(pop),
                      homicidio = sum(homicidio),
                      ppb2_homicidio = sum(ppb2_homicidio),
                      homicidio_proj = sum(homicidio_proj), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj), ~ round( (./pop)*100000, 1) ,.names="tx_{col}") ) -> base
rm(homic,ocult,pop_pnadc)

### Tabelas no formato atlas da violência
#Taxa de homicídio estimado
base |> select(ano,uf_resd,homicidio_proj) |>
  pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  
  adorn_title(placement = "top",col_name = "Número de homicídios estimados de indíegnas, por UF – Brasil {ano}") %>% 
  
  rio::export(.,"n_homicidio_indígenas_estimados_uf_br.xlsx")

#Taxa de homicídio Projetado
base |> select(ano,uf_resd,tx_homicidio_proj) |>
  pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
  #Variações
  mutate(
    #Dez anos
    "{names(.)[2]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
    #Anual
    "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
      format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
    #Cinco anos
    "{names(.)[7]} a {names(.)[ncol(.)]} " := 
      format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  
  adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de indíegnas, por UF – Brasil {ano}") %>% 
  
  rio::export(.,"taxa_homicidio_estimado_indígenas_uf_br.xlsx")
rm(list = ls() )










# 
# # Homicído Projetado HOMEM 15 a 29 anos -----------------------------------
# library(tidyverse)
# library(janitor)
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Contagem de homicídios registrados de jovens, por ano e UF
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & sexo == "Homem" & idade %in% c(15:29) & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,intencao_homic, name = "homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos, por ano e UF
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & sexo == "Homem" & idade %in% c(15:29) & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base homicídio registrado a homicídio coulto
# base <- left_join(homic,ocult, by = c("ano","uf_resd")) |> 
#   #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) -> base
# 
# rm(homic,ocult)
# 
# #Importando população jovens PNADc
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 2, to = 23, by = 2), 
#                               #Indicar as sheets desejadas.
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc_homem_jovem <- do.call(bind_rows, 
#                            mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc_homem_jovem
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc_homem_jovem")))
# 
# #População PNADc Homem Jovem
# pop_pnadc_homem_jovem |> 
#   #Selecionando variáveis e população desejada, neste caso, população geral
#   select(uf_resd=UF,ano,
#          #Seleciona população desejada
#          pop_homem=PoP.Homem) |> 
#   #Excluindo as regiões.
#   filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
#   #Transofrmando em factor variáveis desejadas
#   mutate(ano = ano |> as_factor(),
#          uf_resd = uf_resd |> as_factor()) -> pop_pnadc_homem_jovem
# 
# #Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
# left_join(pop_pnadc_homem_jovem,base, by = c("ano","uf_resd"))  -> base
# rm(pop_pnadc_homem_jovem)
# 
# #Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
# base |>
#   summarise(uf_resd="Brasil" |> as.factor(),
#             pop_homem = sum(pop_homem),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj),.by=ano) |>
#   #Bind_row de Brasil a base com UFs.
#   bind_rows(base) -> base
# 
# #Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
# base %>%
#   mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop_homem)*100000,.names="tx_{col}"),
#          #Padrão Atlas. Taxas com uma casa decimal
#          across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# ### Tabelas no formato atlas da violência
# #Homicídio Projetado Homem jovem - Valor absoluto
# base |> select(ano,uf_resd,homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Número de homicídios projetados de jovens de 15 a 29 anos por UF (2012 a 2022)") %>% 
#   rio::export(.,"n_homicidio_homem_jovem_projetado_uf_br.xlsx")
# 
# #Taxa de homicídio Projetado Homem Jovem
# base |> select(ano,uf_resd,tx_homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character)|(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados, por UF – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_homem_jovem_projetado_uf_br.xlsx")
# rm(base)
# 
# 
# 
# 
# 
# # Homicídio Projetado Mulher Negra ----------------------------------------
# library(tidyverse)
# library(janitor)
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Contagem de homicídios registrados de mulheres negras, por ano e UF
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & sexo == "Mulher" & racacor %in% c("Preta","Parda") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,intencao_homic, name = "homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos mulheres, por ano e UF
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & sexo == "Mulher" & racacor %in% c("Preta","Parda") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base homicídio registrado a homicídio coulto
# base <- left_join(homic,ocult, by = c("ano","uf_resd")) |> 
#   #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) 
# 
# rm(homic,ocult)
# 
# #Importando população mulher negra
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc_mulher_negra <- do.call(bind_rows, 
#                             mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc_mulher_negra
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc_mulher_negra")))
# 
# #População PNADc Mulher
# pop_pnadc_mulher_negra |> 
#   #Selecionando variáveis e população desejada, neste caso, população mulher negra
#   select(uf_resd=UF,ano,
#          #Seleciona população desejada
#          pop_mulher_negra=M.Negro) |> 
#   #Excluindo as regiões.
#   filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
#   #Transofrmando em factor variáveis desejadas
#   mutate(ano = ano |> as_factor(),
#          uf_resd = uf_resd |> as_factor()) -> pop_pnadc_mulher_negra
# 
# #Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
# left_join(pop_pnadc_mulher_negra,base, by = c("ano","uf_resd"))  -> base
# rm(pop_pnadc_mulher_negra)
# 
# #Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
# base |>
#   summarise(uf_resd="Brasil" |> as.factor(),
#             pop_mulher_negra = sum(pop_mulher_negra),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj),.by=ano) |>
#   #Bind_row de Brasil a base com UFs.
#   bind_rows(base) -> base
# 
# #Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
# base %>%
#   mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop_mulher_negra)*100000,.names="tx_{col}"),
#          #Padrão Atlas. Taxas com uma casa decimal
#          across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# ### Tabelas no formato atlas da violência
# #Homicídio Projetado mulher negra - Valor absoluto
# base |> select(ano,uf_resd,homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Número de homicídios projetados de Mulher Negra por UF (2012 a 2022)") %>% 
#   rio::export(.,"n_homicidio_mulher_negra_projetado_uf_br.xlsx")
# 
# #Taxa de homicídio Projetado Mulher Negra
# base |> select(ano,uf_resd,tx_homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character)|(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de Mulher Negra, por UF – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_projetado_mulher_negra_uf_br.xlsx")
# rm(base)
# 
# # Homicído Projetado Mulher NÃO NEGRA -------------------------------------
# library(tidyverse)
# library(janitor)
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Painel com as UFs 
# siconfir::get_info() |> filter(esfera %in% c("E","D")) |>
#   select(uf_resd = ente) |>
#   #Colocando anos investigados
#   crossing(data.frame(ano = year)) |>
#   mutate(uf_resd = uf_resd |> as_factor(),
#          ano = ano |> as_factor()) -> painel_ufs
# 
# 
# #Contagem de homicídios registrados de mulheres não negras, por ano e UF
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & sexo == "Mulher" & racacor %in% c("Amarela","Branca","Indigena") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,intencao_homic, name = "homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos mulheres não negras, por ano e UF
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & sexo == "Mulher" & racacor %in% c("Amarela","Branca","Indigena") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base painel de ufs, homicídio registrado e homicídio coulto
# base <- list(painel_ufs, homic, ocult) %>% reduce(left_join, by = c("ano","uf_resd")) |>
#   #Colocando zeros em UFs sem homicídio registrado ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) 
# 
# rm(homic,ocult,painel_ufs)
# 
# #Importando população mulher não negra
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc_mulher_nao_negra <- do.call(bind_rows, 
#                                   mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc_mulher_nao_negra
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc_mulher_nao_negra")))
# 
# #População PNADc Mulher não negra
# pop_pnadc_mulher_nao_negra |> 
#   #Selecionando variáveis e população desejada, neste caso, população mulher nao negra
#   select(uf_resd=UF,ano,
#          #Seleciona população desejada
#          pop_mulher_nao_negra=M.Não_Negra) |> 
#   #Excluindo as regiões.
#   filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
#   #Transofrmando em factor variáveis desejadas
#   mutate(ano = ano |> as_factor(),
#          uf_resd = uf_resd |> as_factor()) -> pop_pnadc_mulher_nao_negra
# 
# #Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
# left_join(pop_pnadc_mulher_nao_negra,base, by = c("ano","uf_resd")) -> base
# rm(pop_pnadc_mulher_nao_negra)
# 
# #Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
# base |>
#   summarise(uf_resd="Brasil" |> as.factor(),
#             pop_mulher_nao_negra = sum(pop_mulher_nao_negra),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj),.by=ano) |> 
#   #Bind_row de Brasil a base com UFs.
#   bind_rows(base) -> base
# 
# #Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
# base %>%
#   mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop_mulher_nao_negra)*100000,.names="tx_{col}"),
#          #Padrão Atlas. Taxas com uma casa decimal
#          across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# ### Tabelas no formato atlas da violência
# #Homicídio Projetado mulher não negra - Valor absoluto
# base |> select(ano,uf_resd,homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Número de homicídios projetados de Mulher Não Negra por UF (2012 a 2022)") %>% 
#   rio::export(.,"n_homicidio_mulher_nao_negra_projetado_uf_br.xlsx")
# 
# #Taxa de homicídio Projetado Mulher não Negra
# base |> select(ano,uf_resd,tx_homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character)|(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de Mulher não Negra, por UF – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_projetado_mulher_nao_negra_uf_br.xlsx")
# rm(base)
# 
# 
# # Homicídio projetado Mulher residência\domicílio ------------------------
# #Importando população
# library(tidyverse)
# library(janitor)
# #Importando população mulher
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# #Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
# pop_fem_br <- do.call(bind_rows,
#                       mget(ls(pattern = "^[^.]+$"))) |> 
#   #Selecionando pop mulher
#   select(UF,cuf,ano,PoP.Mulher)
# 
# #Removendo pnadcs, exceto pop_fem_br
# rm(list = setdiff(ls(), "pop_fem_br"))
# 
# #População feminina BR por ano
# pop_fem_br |> 
#   #Excluindo regiões 
#   filter(!(UF %in% c("Nordeste","Norte","Sudeste","Centro Oeste","Sul"))) |> 
#   #Somatório população mulher
#   summarise(pop_mulher = sum(PoP.Mulher), .by = ano) |>
#   mutate(ano = ano |> as_factor()) -> pop_fem_br
# 
# #Importando base de homicídios registrados e ocultos
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# ###Contagem de homicídios registrados de mulheres na residência ou fora, Brasil
# #Homicídio feminino, local do acidente = residencial. Aqui poderia entrar Habitação coletiva
# sim_doext |> 
#   filter(ano %in% year & intencao_homic == "Homicídio" & sexo == "Mulher")  |> droplevels() |>
#   tabyl(ano,local_obito) |> 
#   #Homicídios fora da residência. Somatório de todos os locais do incidente, exceto Residencial. Entendo que Hab Coletiva é residencia.
#   mutate(homic_out = rowSums(across(!c(ano,Residencial)))) |> 
#   rename(homic_resd = Residencial) |> select(ano,homic_out,homic_resd) -> homic_fem
# 
# #Contagem de homicídios ocultos de mulheres na residência ou fora, Brasil
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(ano %in% year & .pred_class == "homic" & sexo == "Mulher") |> droplevels() |>
#   tabyl(ano,local_obito) |>
#   #Homicídios fora da residência. Somatório de todos os locais do incidente, exceto Residencial. Entendo que Hab Coletiva é residencia.
#   mutate(homic_ocult_out = rowSums(across(!c(ano,Residencial)))) |> 
#   rename(homic_ocult_resd = Residencial) |> select(ano,homic_ocult_out,homic_ocult_resd) -> ocult
# 
# #Juntando base painel de ufs, homicídio registrado e homicídio coulto
# list(pop_fem_br, homic_fem, ocult) %>% reduce(left_join, by = c("ano")) |>
#     #Criando homicídio projetado residência e fora.
#   mutate(homic_proj_resd = homic_resd + homic_ocult_resd,
#          homic_proj_out = homic_out + homic_ocult_out, 
#          #Taxa de Homicídio projetado dentro e fora da residência
#          tx_homic_proj_resd = round((homic_proj_resd/pop_mulher)*100000,1),
#          tx_homic_proj_out = round((homic_proj_out/pop_mulher)*100000,1)) |>
#   #Selecionando variáveis utilizadas
#   select(ano,starts_with("tx")) |>
#   #Exportando
#   rio::export(x=_,"tx_homic_projetado_fem_resd_out.xlsx")
#   
#  #Join população feminina e homicídio feminio residencial.
# left_join(pop_fem_br,homic_fem, by = c("ano")) |>
#   #Criando taxas homicídio residência e fora da residência
#   mutate(tx_homic_resd = round((homic_resd/pop_mulher)*100000,2),
#          tx_homic_out = round((homic_out/pop_mulher)*100000,2)) |> 
#   select(ano,starts_with("tx")) |> 
#   #Exportando.
#   rio::export(x=_,"tx_homic_projetado_fem_resd_out.xlsx")
# 
# rm(base,homic_fem,pop_fem_br,sim_doext,year,ocult,homic_preds)
# 
# # Homicídio Projetado Negros ----------------------------------------------
# library(tidyverse)
# library(janitor)
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Painel com as UFs 
# siconfir::get_info() |> filter(esfera %in% c("E","D")) |>
#   select(uf_resd = ente) |>
#   #Colocando anos investigados
#   crossing(data.frame(ano = year)) |>
#   mutate(uf_resd = uf_resd |> as_factor(),
#          ano = ano |> as_factor()) -> painel_ufs
# 
# 
# #Contagem de homicídios registrados de negros, por ano e UF
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & racacor %in% c("Parda","Preta") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,intencao_homic, name = "homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos mulheres não negras, por ano e UF
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & racacor %in% c("Parda","Preta") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base painel de ufs, homicídio registrado e homicídio coulto
# base <- list(painel_ufs, homic, ocult) %>% reduce(left_join, by = c("ano","uf_resd")) |>
#   #Colocando zeros em UFs sem homicídio registrado ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) 
# 
# rm(homic,ocult,painel_ufs)
# 
# #Importando população negros
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc_negros <- do.call(bind_rows, 
#                                       mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc_negros
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc_negros")))
# 
# #População PNADc Negros
# pop_pnadc_negros |> 
#   #Selecionando variáveis e população desejada, neste caso, população negro
#   select(uf_resd=UF,ano,
#          #Seleciona população desejada
#          pop_negro=Pop.Negro) |> 
#   #Excluindo as regiões.
#   filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
#   #Transofrmando em factor variáveis desejadas
#   mutate(ano = ano |> as_factor(),
#          uf_resd = uf_resd |> as_factor()) -> pop_pnadc_negros
# 
# #Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
# left_join(pop_pnadc_negros,base, by = c("ano","uf_resd")) -> base
# rm(pop_pnadc_negros)
# 
# #Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
# base |>
#   summarise(uf_resd="Brasil" |> as.factor(),
#             pop_negro = sum(pop_negro),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj),.by=ano) |> 
#   #Bind_row de Brasil a base com UFs.
#   bind_rows(base) -> base
# 
# #Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
# base %>%
#   mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop_negro)*100000,.names="tx_{col}"),
#          #Padrão Atlas. Taxas com uma casa decimal
#          across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# ### Tabelas no formato atlas da violência
# #Homicídio Projetado Negro - Valor absoluto
# base |> select(ano,uf_resd,homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Número de homicídios projetados de Negros, por UF (2012 a 2022)") %>% 
#   rio::export(.,"n_homicidio_negros_projetado_uf_br.xlsx")
# 
# #Taxa de homicídio Projetado Negros
# base |> select(ano,uf_resd,tx_homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character)|(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de Negros, por UF – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_projetado_negros_uf_br.xlsx")
# rm(base)
# 
# 
# # Homicídios Projetados Não Negros ----------------------------------------
# library(tidyverse)
# library(janitor)
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Painel com as UFs 
# siconfir::get_info() |> filter(esfera %in% c("E","D")) |>
#   select(uf_resd = ente) |>
#   #Colocando anos investigados
#   crossing(data.frame(ano = year)) |>
#   mutate(uf_resd = uf_resd |> as_factor(),
#          ano = ano |> as_factor()) -> painel_ufs
# 
# #Contagem de homicídios registrados de não negros, por ano e UF
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & racacor %in% c("Branca","Amarela","Indigena") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,intencao_homic, name = "homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos de não negros, por ano e UF
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & racacor %in% c("Branca","Amarela","Indigena") & ano %in% year) |> droplevels() |>
#   count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base painel de ufs, homicídio registrado e homicídio coulto
# base <- list(painel_ufs, homic, ocult) %>% reduce(left_join, by = c("ano","uf_resd")) |>
#   #Colocando zeros em UFs sem homicídio registrado ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) 
# 
# rm(homic,ocult,painel_ufs)
# 
# #Importando população negros
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc_nao_negros <- do.call(bind_rows, 
#                             mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc_nao_negros
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc_nao_negros")))
# 
# #População PNADc Negros
# pop_pnadc_nao_negros |> 
#   #Selecionando variáveis e população desejada, neste caso, população negro
#   select(uf_resd=UF,ano,
#          #Seleciona população desejada
#          pop_nao_negro=Pop.Não_Negra) |> 
#   #Excluindo as regiões.
#   filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
#   #Transofrmando em factor variáveis desejadas
#   mutate(ano = ano |> as_factor(),
#          uf_resd = uf_resd |> as_factor()) -> pop_pnadc_nao_negros
# 
# #Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
# left_join(pop_pnadc_nao_negros,base, by = c("ano","uf_resd")) -> base
# rm(pop_pnadc_nao_negros)
# 
# #Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
# base |>
#   summarise(uf_resd="Brasil" |> as.factor(),
#             pop_nao_negro = sum(pop_nao_negro),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj),.by=ano) |> 
#   #Bind_row de Brasil a base com UFs.
#   bind_rows(base) -> base
# 
# #Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
# base %>%
#   mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop_nao_negro)*100000,.names="tx_{col}"),
#          #Padrão Atlas. Taxas com uma casa decimal
#          across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# ### Tabelas no formato atlas da violência
# #Homicídio Projetado Não Negro - Valor absoluto
# base |> select(ano,uf_resd,homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Número de homicídios projetados de Não Negros, por UF (2012 a 2022)") %>% 
#   rio::export(.,"n_homicidio_nao_negro_projetado_uf_br.xlsx")
# 
# #Taxa de homicídio Projetado Não Negros
# base |> select(ano,uf_resd,tx_homicidio_proj) |>
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Variações 
#   mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
#          z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
#          y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
#   #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
#   #Colocando vírgula no lugar de ponto.
#   mutate(across(where(is.character)|(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
#   #Ordenando a linha de população seguindo a ordem do atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados de Não Negros, por UF – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_projetado_nao_negros_uf_br.xlsx")
# 
# rm(base)
# 
# 
# 
# # Homicídio Projetado por Região de residência --------------------------------------
# library(tidyverse)
# library(janitor)
# #Importação da base de homicídios ocultos e homicídios registrados
# load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
# #Período analisado
# year <- c(2012:2022)
# 
# #Contagem de homicídios registrados, por ano e Região de Residência
# sim_doext |> 
#   #Filtro das intenções de interesse.
#   filter(intencao_homic  == "Homicídio" & ano %in% year) |> droplevels() |>
#   count(ano,reg_resd,intencao_homic, name = "homicidio") |>
#   #Mantém variáveis utlizadas
#   select(!c(intencao_homic)) -> homic
# 
# #Contagem de homicídios ocultos, por ano e Região de Residência
# homic_preds |> 
#   #Filtrando homicídios ocultos
#   filter(.pred_class == "homic" & ano %in% year) |> droplevels() |>
#   count(ano,reg_resd,.pred_class, name = "ppb2_homicidio") |> 
#   #Mantém variáveis utlizadas
#   select(!c(.pred_class)) -> ocult
# 
# #Juntando base homicídio registrado a homicídio coulto
# base <- left_join(homic,ocult, by = c("ano","reg_resd")) |> 
#   #Colocando zeros em regiões sem homicídio registrado (kek) ou sem homicídio oculto   
#   mutate(across(where(is.numeric),~replace_na(.,0)),
#          #Homicídios projetados
#          homicidio_proj = homicidio + ppb2_homicidio) 
# 
# rm(homic,ocult)
# 
# #Importando população PNADc das regiões
# data_list <- rio::import_list("C:/Users/b224552695/Dropbox/Ipea/Atlas/Pop-PNADc.xlsx", 
#                               which = seq(from = 1, to = 22, by = 2), 
#                               #Indicar as sheets desejadas. 
#                               setclass = "tbl")
# 
# # Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
# for(i in seq_along(data_list)) {
#   assign(names(data_list)[i], data_list[[i]])
# }
# rm(data_list,i)
# 
# # Aplicar bind_rows a todos os dataframes no ambiente global
# pop_pnadc <- do.call(bind_rows, 
#                      mget(setdiff(ls(pattern = "^[^.]+$"),c("base","homic_preds","sim_doext","year"))))
# 
# #Removendo pnadcs, exceto bases utilizadas e pop_pnadc 
# rm(list = setdiff(ls(), c("base","homic_preds","sim_doext","year","pop_pnadc")))
# 
# #População PNADc
# pop_pnadc |> 
#   select(reg_resd = UF, ano,
#          #Selecionando População de interesse. População geral
#          pop = Pop) |>
#   #Selecionando as regiões de residência
#   filter(reg_resd %in% c("Centro Oeste","Norte","Nordeste","Sul","Sudeste")) |>
#   #Alteração das classes.
#   mutate(reg_resd = reg_resd |> as_factor(),
#          ano = ano |> as_factor()) -> pop_pnadc
# 
# #Join da População Geral (PNADc) na base de homicídios registrados, ocultos e projetados, por região de residência.
# left_join(pop_pnadc,base, by = c("ano","reg_resd")) -> base
# rm(pop_pnadc)
# #Acrescentando o agregado nacional a tabela com os homicídios por UF e ano.
# base |>
#   group_by(ano) |>
#   summarise(reg_resd = "Brasil" |> as.factor(),
#             pop = sum(pop),
#             homicidio = sum(homicidio),
#             ppb2_homicidio = sum(ppb2_homicidio),
#             homicidio_proj = sum(homicidio_proj)) |>
#   #Bind_row do total Brasil 
#   bind_rows(base) -> base
# 
# #Criação das taxas de homicídio oculto, registrado e projetado
# base |>  mutate(across(c(homicidio,ppb2_homicidio,homicidio_proj),~(./pop)*100000,.names="tx_{col}"),
#                 #Padrão Atlas. Taxas com uma casa decimal
#                 across(starts_with("tx_"),~round(.,digits = 1))) |> 
#   rename(tx_oculto = tx_ppb2_homicidio) -> base
# 
# #Tabela formato wide do valor absoluto de homicídios
# base |> select(ano,reg_resd,tx_homicidio_proj) |> 
#   pivot_wider(names_from = ano,values_from = tx_homicidio_proj) %>%
#   #Colocando vírgula no lugar de ponto, numérico
#   mutate(across(where(is.numeric), ~ format(., decimal.mark = ","))) %>% 
#   #Ordenando as linhas da tabela
#   slice(match(c("Norte", "Nordeste", "Centro Oeste", "Sudeste", "Sul", "Brasil"),reg_resd)) |>
#   adorn_title(placement = "top",col_name = "Taxa de homicídios projetados por Região de Residência – Brasil (2012-2022)") %>% 
#   rio::export(.,"taxa_homicidio_projetado_reg_resd_br.xlsx")
# 
# rm(base,homic_preds,sim_doext,year)
