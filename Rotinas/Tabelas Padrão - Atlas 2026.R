                                       #### Tabelas Atlas da Violência 2026 ####

# Número de Homicídio Geral ---------------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)


#Número de Homicídios - Geral
sim_doext |>
  filter(intencao_homic == "Homicídio") |>
  tabyl(def_uf_resd,ano) |> adorn_totals(name = "Brasil") %>%

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
                "Sergipe","Tocantins"), def_uf_resd) ) |> 
  #Necessário para colocar o título
  as_tibble() |>
  janitor::adorn_title(row_name = "UF",
                       placement = "top", 
                       col_name = glue::glue("Número de homicídios, por UF {min(year)}–{max(year)}") )  |>
  #Exportando tabela.
  rio::export(x = _,"base/n_homicidio_uf_br.xlsx")


# #Taxa de homicídios - Geral ---------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio") |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano,cod_uf_resd,def_uf_resd, name = "homicidio") |>
  #Atlas 2026, elaborado através de duckdb. Números como caracter
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- here::here("populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide da taxa de homicídios
base |> select(ano,def_uf_resd,tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) %>%
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
                "Sergipe","Tocantins"), def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  #as_tibble() |>
  #Nota de rodapé
  add_row(
  def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade (SIM). Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de Homicídios, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/tx_homicidio_uf_br.xlsx")


# Homicídios de Jovens ------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & idade %in% c(15:29) ) |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #Base sim extraida de arquivo duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.double() ) -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Jovens_UFs_PNADc.xlsx") 

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,def_uf_resd, homicidio) |>
  pivot_wider(names_from = ano, values_from =  homicidio) %>%
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  as_tibble() |>
  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade (SIM). Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue(" Homicídios de Jovens, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/n_homicidio_jovem_uf_br.xlsx")


### Tabela com taxa de homicídios Jovens


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
  
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%

  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade (SIM). Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de Homicídios de Jovens, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/taxa_homicidio_jovem_uf_br.xlsx")
rm(base)


# Homicídio de Homens Jovens ----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)


#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_sexo == "Homem" & idade %in% c(15:29) ) |> 
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #Base sim extraida de arquivo duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.double() ) -> homic

##Importando população.
#Caminho do excel com pnadc de jovens
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Jovens_UFs_PNADc.xlsx")

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população homem jovem
  select(UF,ano,Pop = PoP.Homem)
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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,def_uf_resd, homicidio) |>
  pivot_wider(names_from = ano, values_from =  homicidio) %>%
  
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos foi obtido pela soma de indivíduos homens de 15 a 29 anos.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de Homicídios Homens Jovens, por UF {min(year)}–{max(year)}") ) |>
  
  #Exportando tabela.
  rio::export(x= _ ,"base/n_homicidio_homem_jovem_uf_br.xlsx")

###Taxa de homicídio homem jovem ###


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),def_uf_resd) ) |>
  
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Necessário para colocar o título
  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos e da população foi obtido pela soma de indivíduos homens de 15 e 29 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top",
              col_name = glue::glue("Taxa de Homicídios de Homens Jovens, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/taxa_homicidio_homem_jovem_uf_br.xlsx")
rm(base)



#Homicídios de crianças de 0 a 4 anos por UF -----------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)


#Carregando base populacional. Microdado RIPSA.
load( paste0(dirname(getwd()),"/bases/populacao/ripsa/pop_ripsa.Rdata") ) 
pop <- pop |> 
  #Código do município com seis dígitos
  mutate(cod_mun = substr(cod_mun, 1, 2),
         idade = idade |> as.character() |> as.integer(),
         def_uf_resd = recode_values(as.character(cod_mun),
                                       "11" ~ "Rondônia",
                                       "12" ~ "Acre",
                                       "13" ~ "Amazonas",
                                       "14" ~ "Roraima",
                                       "15" ~ "Pará",
                                       "16" ~ "Amapá",
                                       "17" ~ "Tocantins",
                                       "21" ~ "Maranhão",
                                       "22" ~ "Piauí",
                                       "23" ~ "Ceará",
                                       "24" ~ "Rio Grande do Norte",
                                       "25" ~ "Paraíba",
                                       "26" ~ "Pernambuco",
                                       "27" ~ "Alagoas",
                                       "28" ~ "Sergipe",
                                       "29" ~ "Bahia",
                                       "31" ~ "Minas Gerais",
                                       "32" ~ "Espírito Santo",
                                       "33" ~ "Rio de Janeiro",
                                       "35" ~ "São Paulo",
                                       "41" ~ "Paraná",
                                       "42" ~ "Santa Catarina",
                                       "43" ~ "Rio Grande do Sul",
                                       "50" ~ "Mato Grosso do Sul",
                                       "51" ~ "Mato Grosso",
                                       "52" ~ "Goiás",
                                       "53" ~ "Distrito Federal",
                                       default = NA_character_) ) |>
  #Nome padrão.
  rename(cod_uf_resd = cod_mun) |>
  #Matém idade e anos de interesse
  filter(between(idade,0,4) & ano %in% year) |>
  
  #Variáveis não utilizadas
  select(!c(sexo,idade) ) |>
  #Somatório das idades para encontra a faixa etária desejada.
  summarise(pop_inf04 = sum(pop), .by = c(cod_uf_resd,def_uf_resd,ano) )
  

#Tabela com homicídios infatil 0 a 4. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% c(0:4)) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(def_uf_resd, cod_uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf04


#Join entre população infatil e homicídio infantil
left_join(pop,homic_inf04, by = join_by("cod_uf_resd","def_uf_resd","ano") ) -> base
rm(homic_inf04, pop)  

#Acrescentando total Brasil e criando taxa
base |>
  #Adiciona zero, quando não ocorre homicídio no ano, uf e idade de interesse
  mutate(homic = replace_na(homic,0) ) %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" ,
                        pop_inf04 = sum(pop_inf04),
                        homic = sum(homic, na.rm = TRUE), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf04)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de infantes 0 a 4 anos.
base |> select(ano,def_uf_resd, homic) |>
  pivot_wider(names_from = ano, values_from =  homic) %>%
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos foi obtido pela soma de indivíduos de 0 a 4 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de homicídios registrados de infantes (0 a 4 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/n_homicidio_infatil04_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de infantes 0 a 4 anos.
base |> select(ano,def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  
  #Ordenando a linha da tabela seguindo o padrão Atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"), def_uf_resd) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Nota de rodapé
  add_row(
    def_uf_resd = "Fonte: IBGE - Projeções da População do Brasil e Unidades da Federação por sexo e idade: 2010-2060 e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos e da população foi obtido pela soma de indivíduos de 0 a 4 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios registrados de infantes (0 a 4 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/taxa_homicidio_infantil04_uf_br.xlsx")



  