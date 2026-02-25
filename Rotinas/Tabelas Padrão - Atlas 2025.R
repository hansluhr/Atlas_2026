                                       #### Tabelas Atlas da Violência 2025 ####

# Número de Homicídio Geral ---------------------------------------------------------
library(tidyverse)
library(janitor)
                                       
#Importando bases
load("C:/Users/P224552695/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Número de Homicídios - Geral
sim_doext |>
  filter(intencao_homic == "Homicídio") |>
  tabyl(uf_resd,ano) |> adorn_totals(name = "Brasil") %>%
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
  
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf_resd) ) |> 
  #Necessário para colocar o título
  as_tibble() |>
  adorn_title(placement = "top",col_name = "Número de homicídios, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _,"n_homicidio_uf_br.xlsx")


# #Taxa de homicídios - Geral ---------------------------------------------
library(tidyverse)
library(janitor)
#Importando bases
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio") |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano,cod_uf_resd,uf_resd, name = "homicidio") -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
  summarise(uf_resd="Brasil" |> as.factor(),
            Pop = sum(Pop),
            homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide da taxa de homicídios
base |> select(ano,uf_resd,tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de Homicídios, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"tx_homicidio_uf_br.xlsx")
rm(base)



# Homicídios de Jovens ------------------------------------------
library(tidyverse)
library(janitor)
#Criando tabela com número e taxa de homicídios de jovens. 

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & idade %in% c(15:29) ) |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,uf_resd, homicidio) |>
  pivot_wider(names_from = ano, values_from =  homicidio) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de Homicídios de Jovens, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_jovem_uf_br.xlsx")


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de Homicídios de Jovens, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_jovem_uf_br.xlsx")
rm(base)


# Homicídio de Homens Jovens ----------------------------------------------
library(tidyverse)
library(janitor)

#Criando tabela com número e taxa de homicídios de jovens. 

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & sexo == "Homem" & idade %in% c(15:29) ) |> 
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar brasil ao juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic

##Importando população.
#Caminho do excel com pnadc de jovens
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Jovens_UFs_PNADc.xlsx"

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        Pop = sum(Pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/Pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,uf_resd, homicidio) |>
  pivot_wider(names_from = ano, values_from =  homicidio) %>%
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
  adorn_title(placement = "top",col_name = "Número de Homicídios Homens Jovens, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_homem_jovem_uf_br.xlsx")


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de Homicídios de Homens Jovens, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_homem_jovem_uf_br.xlsx")
rm(base)



#Homicídios de crianças de 0 a 4 anos por UF -----------------
library(tidyverse)
library(janitor)

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Importando população de crianças idade 0 a 4 por UF.
#Fonte: PROJEÇÃO DA POPULAÇÃO, retirado do DATASUS
pop_inf04 <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2025/base/pop_inf04.xlsx") |>
  pivot_longer(cols = !c(cod_uf_resd, uf_resd), names_to = "ano", names_transform = list(ano = as.factor), values_to = "pop_inf04") |>
  mutate(uf_resd = uf_resd |> as_factor() ) 

#Tabela com homicídios infatil 0 a 4. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% c(0:4)) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(uf = uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf04

#Join entre população infatil e homicídio infantil
left_join(pop_inf04,homic_inf04, by = join_by("uf_resd" == "uf","ano") ) -> base
rm(homic_inf04, pop_inf04)  
  
#Acrescentando total Brasil e criando taxa
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop_inf04 = sum(pop_inf04),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf04)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de homicídios de infantes 0 a 4 anos.
base |> select(ano,uf_resd, homic) |>
  pivot_wider(names_from = ano, values_from =  homic) %>%
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
  adorn_title(placement = "top",col_name = "Número de homicídios de crianças de 0 a 4 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_infatil04_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de infantes 0 a 4 anos.
base |> select(ano,uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de crianças de 0 a 4 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"taxa_homicidio_infantil04_uf_br.xlsx")



# Homicídio de criança 5 a 14 por ano e UF ---------------------------------------------
library(tidyverse)
library(janitor)

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Importando população de crianças idade 5 a 14 anos por UF.
#Fonte: PROJEÇÃO DA POPULAÇÃO, retirado do DATASUS
pop_inf514 <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2025/base/pop_inf514.xlsx") |>
  pivot_longer(cols = !c(cod_uf_resd, uf_resd), names_to = "ano", names_transform = list(ano = as.factor), values_to = "pop_inf514") |>
  mutate(uf_resd = uf_resd |> as_factor() ) 

#Tabela com homicídios infatil 5 a 14 anos. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% c(5:14)) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(uf = uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf514

#Join entre população infatil e homicídio infantil
left_join(pop_inf514,homic_inf514, by = join_by("uf_resd" == "uf","ano") ) -> base
rm(homic_inf514, pop_inf514)  

#Acrescentando total Brasil e criando taxa
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop_inf514 = sum(pop_inf514),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf514)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de infantes 0 a 4 anos.
base |> select(ano,uf_resd, homic) |>
  pivot_wider(names_from = ano, values_from =  homic) %>%
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
  adorn_title(placement = "top",col_name = "Número de homicídios de crianças de 0 a 4 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_infatil514_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de infantes 0 a 4 anos.
base |> select(ano,uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de crianças de 0 a 4 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"taxa_homicidio_infantil514_uf_br.xlsx")




# Homicídio adolescente 15 a 19 -------------------------------------------
library(tidyverse)
library(janitor)

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Importando população de crianças idade 5 a 14 anos por UF.
#Fonte: PROJEÇÃO DA POPULAÇÃO, retirado do DATASUS
pop_inf1519 <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2025/base/pop_inf1519.xlsx") |>
  pivot_longer(cols = !c(cod_uf_resd, uf_resd), names_to = "ano", names_transform = list(ano = as.factor), values_to = "pop_inf1519") |>
  mutate(uf_resd = uf_resd |> as_factor() ) 

#Tabela com homicídios infatil 5 a 14 anos. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% c(15:19) ) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(uf = uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf1519

#Join entre população infatil e homicídio infantil
left_join(pop_inf1519,homic_inf1519, by = join_by("uf_resd" == "uf","ano") ) -> base
rm(homic_inf1519, pop_inf1519)  

#Acrescentando total Brasil e criando taxa
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop_inf1519 = sum(pop_inf1519),
                        homic = sum(homic), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf1519)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de adolescentes 15 a 19 anos.
base |> select(ano,uf_resd, homic) |>
  pivot_wider(names_from = ano, values_from =  homic) %>%
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
  adorn_title(placement = "top",col_name = "Número de homicídios de adolescentes de 15 a 19 anos por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_infatil1519_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de infantes 0 a 4 anos.
base |> select(ano,uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from =  tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de crianças de 15 a 19 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"taxa_homicidio_infantil1519_uf_br.xlsx")


# Instrumento de óbito de infantes, crianças e adolescentes ---------------
library(tidyverse)
library(janitor)

#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

sim_doext |> 
  #Adiciona faixa etária de interesse
  mutate(fxet = case_when(
                between(idade, 0, 4) ~ "Infantes (0 a 4 anos)", 
                between(idade, 5, 14) ~ "Crianças (5 a 14 anos)",
                between(idade, 15, 19) ~ "Adolescentes (15 a 19 anos)", .default = "Restante") |> as_factor() |>
           
  #Ordem dos Levels da faixa etária
  fct_relevel("Infantes (0 a 4 anos)", 
              "Crianças (5 a 14 anos)",
              "Adolescentes (15 a 19 anos)") ) |> 
  #Ordem dos levels de instrumento
  mutate(instrumento = instrumento |> 
  fct_relevel("PAF", "Perfurante", "Desconhecido", "Contundente", "Enforcamento",
              "Fogo", "Afogamento", "Veículo", "Envenenamento", "Impacto" ) ) |>
  #Mantém somente homicídios e exclui faixa etária sem interesse.
  filter(fxet != "Restante" & intencao_homic == "Homicídio") |> droplevels() |>
  
  tabyl(instrumento,fxet) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |>
  rio::export(x = _, "instrumento_eca.xlsx")



# Suicídios de infantes 10 a 4 anos ---------------------------------------------------
#Pedido do Daniel para subseção.
library(tidyverse)
library(janitor)
#Importando bases de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Importando população de jovens 10 a 19 anos por UF. Fonte PNADC
pop <- readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas 2025/base/pop_10a19.xlsx",
                        sheet = "Planilha1") |>
   pivot_longer(cols = !c(cod_ibge, uf_resd), names_to = "ano", 
                names_transform = list(ano = as.factor), values_to = "pop") |>
   mutate(uf_resd = uf_resd |> as_factor() ) 


#Tabela com suicídio jovens 10 a 19 anos 
sim_doext |> filter(intencao_homic == "Suicídio" & idade %in% c(10:19)) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(uf = uf_resd, cod_uf_resd, ano, .drop = FALSE, name = "n_suic") -> suic


#Join entre população infatil e homicídio infantil
left_join(pop,suic, by = join_by("uf_resd" == "uf","cod_ibge" == "cod_uf_resd", "ano") ) -> base
rm(suic, pop)  

#Acrescentando total Brasil e criando taxa
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        n_suic = sum(n_suic), .by=ano) ) |>
  #Taxa de Suicídio Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_suic = format(round((n_suic/pop)*100,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide do número de suicídios entre 10 a 19 anos
base |> select(ano,uf_resd, n_suic) |>
  pivot_wider(names_from = ano, values_from =  n_suic) %>%
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
  adorn_title(placement = "top",col_name = "Número de suicídios entre 10 a 19 anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_suic_jovens10a19_uf_br.xlsx")


#Tabela formato wide da taxa de suicídios jovens 10 a 19 anos
base |> select(ano,uf_resd, tx_suic) |>
  pivot_wider(names_from = ano, values_from =  tx_suic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de suicídios de jovens anos por UF (2013 a 2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"taxa_suic_10a19_uf_br.xlsx")


# Homicídio Mulheres ------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & sexo == "Mulher") |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano,cod_uf_resd,uf_resd, name = "homicidio") -> homic

##Importando população.
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de mulheres, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"n_homicidio_mulher_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de mulheres.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de mulheres, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_mulher_uf_br.xlsx")



# Gráfico com as as três UFs de maior taxa + taxa Brasil
base |>
  select(ano,uf_resd,tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, names_prefix = "ano_") |>
  #Ordenando linhas para maiores taxas em 2023
  arrange(desc(ano_2023)) |>
  #Filtro das três UFs de maior taxa em 2023 + a taxa Brasil
  filter(uf_resd %in% c(uf_resd[1:4]) | uf_resd == "Brasil") |>
  #Exportando ranking das UFs mais violêntas, mulher
  rio::export(x = _, "ranking_tx_homic_mulher.xlsx")
  
 


# Homicídio Mulher residência\domicílio ---------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Homicídio feminino, local do acidente = residencial. Aqui poderia entrar Habitação coletiva
sim_doext |> 
  filter(intencao_homic == "Homicídio" & sexo == "Mulher")  |> droplevels() |>
  tabyl(ano,local_obito) |>
  #Homicídios fora da residência. Somatório de todos os locais do incidente, exceto Residencial. Entendo que Hab Coletiva é residencia.
  mutate(homic_out = rowSums(across(!c(ano,Residencial)))) |> 
  rename(homic_resd = Residencial) |> select(ano,homic_out,homic_resd) -> homic_fem


##Importando população feminina
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
  filter( !(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) )  |>
  
  #Somatório população mulher. Pop Mulher Brasil
  summarise(pop = sum(pop), .by = ano) |>
  
  mutate(ano = ano |> as_factor() ) -> pop_pnadc

#Join população feminina e homicídio feminio residencial.
left_join(pop_pnadc, homic_fem, by = join_by("ano") ) |>
  mutate(tx_homic_resd = round((homic_resd/pop)*100000,2),
         tx_homic_out = round((homic_out/pop)*100000,2)) |> 
  select(ano,starts_with("tx") ) |> 
  
  pivot_longer(cols = starts_with("tx_homic"), 
               names_to = "tipo_homicidio", 
               values_to = "taxa") %>%
  
  pivot_wider(names_from = ano, values_from = taxa) |> 
  #Exportando.
  rio::export(x=_,"tx_homic_fem_resd_out.xlsx")

rm(homic_fem,pop_pnadc,sim_doext)



# Homicídios mulheres negras ----------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & sexo == "Mulher" & racacor %in% c("Parda","Preta") ) |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano,cod_uf_resd,uf_resd, name = "homicidio") -> homic

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população mulher
  select(uf = UF,ano, pop = M.Negro)
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de mulheres negras, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"n_homicidio_mulher_negra_uf_br.xlsx")

#Tabela formato wide da taxa de homicídios de mulheres.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de mulheres negras, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_mulher_negra_uf_br.xlsx")
rm(base, sim_doext)



# Homicídio Mulheres Não Negras -------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
 # mutate(cod_uf_resd = cod_uf_resd |> as_factor() ) |>
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & sexo == "Mulher" & racacor %in% c("Amarela","Branca","Indigena") ) |>
  
  
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic
  #Com .drop = FALSE, o count faz a interação de todos os códigos com a uf_resd. 


##Importando população de mulher não negra.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população mulher
  select(uf = UF,ano, pop = M.Não_Negra)
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres não negras
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de mulheres não negras, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"n_homicidio_mulher_nao_negra_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de mulheres não negra.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Taxa de homicídios de mulheres negras, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_mulher_nao_negra_uf_br.xlsx")
rm(base, sim_doext)




# Homicídio de negros -----------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor %in% c("Parda","Preta") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic
  

##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população negra
  select(uf = UF,ano, pop = Pop.Negro)
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres não negras
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de negros, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"n_homicidio_negro_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de negros.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de homicídios negros, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_negro_uf_br.xlsx")
rm(base, sim_doext)




# Homicídio de Negros - Capitais ----------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Adicionar município de residência
library(geobr)
#Informações sobre os municípios
munic_code <- 
  read_municipality(year = 2022) |> as_tibble() |>
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6 dígitos. 
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6), 
         across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) |> select(!c(code_region))

#Adicionando município Ignorado ou exterior. 
#A saúde utiliza código de município ingorado. Esses municípios não aparecem em outras bases.
munics_ign <- tribble(~code_muni,~name_muni,~code_state,~abbrev_state,~name_state, ~name_region,
                      "000000", "Ignorado ou exterior",    "00", "IGN", "Ignorado ou exterior", "Ignorado ou exterior",
                      "110000", "Município ignorado - RO", "11",  "RO", "Rondônia", "Norte",
                      "130000", "Município ignorado - AM", "13",  "AM", "Amazonas", "Norte",
                      "150000", "Município ignorado - PA", "15",  "PA", "Pará", "Norte",
                      "210000", "Município ignorado - MA", "21",  "MA", "Maranhão", "Nordeste",
                      "170000", "Município ignorado - TO", "17",  "TO", "Tocantins", "Norte",
                      "240000", "Município ignorado - RN", "24",  "RN", "Rio Grande do Norte", "Nordeste",
                      "260000" ,"Município ignorado - PE", "26",  "PE", "Pernambuco", "Nordeste",
                      "280000", "Município ignorado - SE", "28",  "SE", "Sergipe", "Nordeste",
                      "310000", "Município ignorado - MG", "31",  "MG", "Minas Gerais", "Sudeste",
                      "330000", "Município ignorado - RJ", "33",  "RJ", "Rio de Janeiro", "Sudeste",
                      "410000", "Município ignorado - PR", "41",  "PR", "Paraná", "Sul",
                      "430000", "Município ignorado - RS", "43",  "RS", "Rio Grande do Sul", "Sul",
                      "510000", "Município ignorado - MT", "51",  "MT", "Mato Grosso", "Centro Oeste",
                      "520000", "Município ignorado - GO", "52",  "GO", "Goiás", "Centro Oeste",
                      "120000", "Município ignorado - AC", "12",  "AC", "Acre", "Norte",      
                      "140000", "Município ignorado - RR", "14",  "RR", "Roraima", "Norte",
                      "160000", "Município ignorado - AP", "16",  "AP", "Amapá",  "Norte",
                      "220000", "Município ignorado - PI", "22",  "PI", "Piauí", "Nordeste",
                      "230000", "Município ignorado - CE", "23",  "CE", "Ceará",  "Nordeste",
                      "250000", "Município ignorado - PB", "25",  "PB", "Paraíba","Nordeste",
                      "270000", "Município ignorado - AL", "27",  "AL", "Alagoas", "Nordeste",
                      "290000", "Município ignorado - BA", "29",  "BA", "Bahia", "Nordeste",
                      "320000", "Município ignorado - ES", "32",  "ES", "Espírito Santo", "Sul",
                      "350000", "Município ignorado - SP", "35",  "SP", "São Paulo", "Sudeste", 
                      "420000", "Município ignorado - SC", "42",  "SC", "Santa Catarina", "Sul",
                      "500000", "Município ignorado - MS", "50",  "MS", "Mato Grosso do Sul", "Sul")

#Não estão incluídios os municípios que são bairros presentes no início da série.
#Por exemplo, Rio de Janeiro.

#Bind de municípios conhecidos e ignorados.
bind_rows(munic_code,munics_ign) -> base_munics
rm(munics_ign)

#Join do nome dos municípios a base de homicídios
sim_doext |>
  left_join(x = _, y = munic_code, by = join_by("codmunres" == "code_muni" )) |> as_tibble() |>
  rename(name_munic_resd = name_muni) -> sim_doext
rm(base_munics,munic_code)

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor %in% c("Parda","Preta") & ano == 2023 & 
        #Mantém somente capitias   
         codmunres %in% c( "120040", "270430", "160030", "130260", "292740", "230440", "530010", "320530", "520870", "211130",
                           "510340", "500270", "310620", "150140", "250750", "410690", "261160", "221100", "330455", "240810",
                           "431490", "110020", "140010", "420540", "355030", "280030", "172100") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, codmunres, name_munic_resd, name = "homic") -> homic


#Importando população negros nas capitais
pop_pnadc <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_Cidades_PNADc.xlsx", sheet = "PNADc2023") |>
  #Mantém pop de negros
  select(code_muni, name_munic_resd = Capital,name_state, pop = Pop.Negro)


#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("codmunres" == "code_muni","name_munic_resd") ) |>
  select(ano,name_munic_resd,homic,pop) -> base
rm(homic,pop_pnadc)

#Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
base |>
   mutate(tx_homic = format(round((homic/pop)*100000,digits = 1) ) |> as.double() ) |>
  select(name_munic_resd,tx_homic) |> arrange(desc(tx_homic) ) |>
  #Exportando
  rio::export(x = _, "tx_homic_negros_capitais.xlsx")

rm(list = ls() )



# Homicídio de não negros -------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor %in% c("Amarela","Branca","Indigena") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic


##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população negra
  select(uf = UF,ano, pop = Pop.Não_Negra)
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de não negros
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de não negros, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"n_homicidio_nao_negro_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de não negros.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de homicídios de não negros, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_nao_negro_uf_br.xlsx")
rm(base, sim_doext)




# Homicídio Indígena ------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/p224552695/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

grupo <- "Indigena"

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor == "Indigena" ) |> 
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") -> homic


##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população negra
  select(uf = UF,ano, pop = "Pop.Indígena")
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

#Join homicídios e população.  
left_join(x = pop_pnadc, y = homic , by = join_by("ano", "cod_ibge" == "cod_uf_resd", "uf" == "uf_resd" ) ) |>
  #Colocando zeros nas UFs com zeros homicídios.
  mutate(homicidio = homicidio |> replace_na(0) ) |>
  #O código da UF não é mais necessário
  select(!c(cod_ibge) ) -> base


rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(
    tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de indígenas
base |> select(ano,uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios de indígenas, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ , glue::glue( "n_homicidio_{grupo}_uf_br.xlsx") )


#Tabela formato wide da taxa de homicídios de indígenas.
base |> select(ano, uf_resd = uf, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = glue::glue("Taxa de homicídios {grupo}, por UF – Brasil (2013-2023)") ) |>
  #Exportando tabela.
  rio::export(x= _ , glue::glue("taxa_homicidio_{grupo}_uf_br.xlsx") ) 
rm(base, sim_doext, grupo)



# Taxa Nacional (sem indígenas) x Taxa Indígenas --------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

grupo <- "Indigena"
#Contagem de homicídios registrados exceto indígenas, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor!= "Indigena") |> 
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "homicidio") |>
  
  #Join da contagem de homicídios registrados indígenas.
  left_join(x = _, y = sim_doext |> 
              
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & racacor == "Indigena") |> 
    
  #Cotagem de homicídios indígenas
  count(ano, cod_uf_resd, uf_resd, name = "homicidio_ind"), 
  #Join de homicídio indígena e geral sem indígena
  by = join_by("ano", "cod_uf_resd", "uf_resd") ) |>
  #Preenchimento de zeros em UFs sem homicídios de indígenas 
  mutate(homicidio_ind = homicidio_ind |> replace_na(0) ) -> homic


##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- "D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população de interesse
  select(uf = UF, ano, pop = Pop, pop_indg = "Pop.Indígena") |>
  
  #Redução da pop geral da pop indígena
  mutate(pop = pop - pop_indg)
rm(excel_pnadc)

#Tratamento base com população PNADc
pop_pnadc |> 
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
         uf = uf |> as_factor() ) -> pop_pnadc

#Join homicídios e população.  
left_join(x = pop_pnadc, y = homic ,
          by = join_by("ano", "cod_ibge" == "cod_uf_resd", "uf" == "uf_resd" ) ) |>
  #O código da UF não é mais necessário
  select(!c(cod_ibge) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
               summarise(uf="Brasil" |> as.factor(),
                        pop = sum(pop), 
                        pop_indg = sum(pop_indg),
                        homicidio = sum(homicidio), 
                        homicidio_ind = sum(homicidio_ind), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(
    tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double(), 
    tx_homic_indg = format(round((homicidio_ind/pop_indg)*100000,digits = 1) ) |> as.double() ) -> base


#Gráfico Taxa de homicídios indígenas e taxa Brasil excluindo indígenas.
base |> 
 filter(uf == "Brasil") |> select(ano, "Tx. H. Geral" = tx_homic, "Tx. H.  Indígena" = tx_homic_indg) |>
 pivot_longer(cols = !c(ano), names_to = "taxas", values_to = "value") |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano = ano |> fct_drop() |> as.character() |> as.numeric(),
         ano = make_date(ano) ) |> 
 
 ggplot() + 
 #Linha  
 geom_line(aes(x = ano, y = value, color = taxas, group = taxas), linewidth = 1) + 
 geom_point(aes(x = ano, y = value, color = taxas) ) + 
 #Label dos valores das taxas 
 ggrepel::geom_text_repel(aes(x = ano, y = value, label = value, color = taxas), size = 4, show.legend = FALSE) +
 #Label do eixo x como 1 year
 scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
 scale_y_continuous(breaks = seq(0,60,5) ) +
  guides(color = guide_legend(position = "inside", nrow = 1) ) +
  theme(legend.position.inside = c(0.6, 0.90),
        axis.text.x=element_text(size=8), axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8), axis.title.y=element_text(size=10),
        legend.text = element_text(size = 12),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) + 
 labs(x="", y = "Taxas de Homicídio", color = "")
ggsave(filename ="tx_homic_geralxindg.bmp",width = 8,height = 5,device='bmp', dpi=150)



# #Tabela formato wide do número de homicídios de indígenas
# base |> select(ano,uf_resd,homicidio) |>
#   pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
#   #Variações
#   mutate(
#     #Dez anos
#     "{names(.)[2]} a {names(.)[ncol(.)]}" := 
#       format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
#     #Anual
#     "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
#       format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
#     #Cinco anos
#     "{names(.)[7]} a {names(.)[ncol(.)]} " := 
#       format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
#   #Ordenando a linha da tabela seguindo o padrão Atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd) ) |>
#   # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
#   #Assim fica mais fácil converter para numérico no excel.
#   mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
#   #Necessário para colocar o título
#   as_tibble() |>
#   adorn_title(placement = "top",col_name = "Homicídios de indígenas, por UF – Brasil (2013-2023)") |>
#   #Exportando tabela.
#   rio::export(x = _ , glue::glue( "n_homicidio_{grupo}_uf_br.xlsx") )
# 
# 
# #Tabela formato wide da taxa de homicídios de indígenas.
# base |> select(ano, uf_resd = uf, tx_homic) |>
#   pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
#   #Variações
#   mutate(
#     #Dez anos
#     "{names(.)[2]} a {names(.)[ncol(.)]}" := 
#       format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ","),
#     #Anual
#     "{names(.)[ncol(.)-1]} a {names(.)[ncol(.)]}" := 
#       format(round((.[[ncol(.)]] - .[[ncol(.)-1]]) / .[[ncol(.)-1]] * 100, 1), decimal.mark = ","),
#     #Cinco anos
#     "{names(.)[7]} a {names(.)[ncol(.)]} " := 
#       format(round((.[[ncol(.)]] - .[[7]]) / .[[7]] * 100, 1), decimal.mark = ",") ) |>
#   #Ordenando a linha da tabela seguindo o padrão Atlas.
#   slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
#                 "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
#                 "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
#                 "Sergipe","Tocantins"),uf_resd) ) |>
#   # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
#   #Assim fica mais fácil converter para numérico no excel.
#   mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
#   #Necessário para colocar o título
#   as_tibble() |>
#   adorn_title(placement = "top", col_name = glue::glue("Taxa de homicídios {grupo}, por UF – Brasil (2013-2023)") ) |>
#   #Exportando tabela.
#   rio::export(x= _ , glue::glue("taxa_homicidio_{grupo}_uf_br.xlsx") ) 
# rm(base, sim_doext, grupo)




# Homicídio em Terras indígenas -------------------------------------------









# Suicídio Indígena -------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/p224552695/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de suicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & racacor %in% c("Indigena") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, name = "suic") -> suic


##Importando população de indígenas
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população indígena
  select(uf = UF,ano, pop = Pop.Indígena)
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

#Join homicídios e população. 
#Fiz o full, pois algumas UFs apresentam contagem de suicídio zero e não aparecem no count.
full_join(x = suic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) |> 
  #Colocando zero nas UFs sem suicídio indígena no ano
  mutate(suic = replace_na(suic,0) ) -> base
rm(suic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        suic = sum(suic), .by=ano) ) |> 
  #Taxa de suicídio de indígenas Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_suic = format(round((suic/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de não negros
base |> select(ano,uf_resd,suic) |>
  pivot_wider(names_from = ano, values_from = suic, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Suicídio de indígenas, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"n_ind_suic_uf_br.xlsx")


#Tabela formato wide da taxa de suicídios de indígenas
base |> select(ano, uf_resd, tx_suic) |>
  pivot_wider(names_from = ano, values_from = tx_suic, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de suicídio de indígenas, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"tx_ind_suic_uf_br.xlsx")
rm(base, sim_doext)


# Suicídio indígena e Geral -----------------------------------------------
library(tidyverse)
library(janitor)

#Importando base SIM
load("C:/Users/p224552695/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de suicídios registrados indígenas, por ano
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & racacor == "Indigena") |>
  #Contagem de suicídio de indígena
  count(ano, name = "ind_suic") -> suic_ind
  
#Contagem de suicídios de Brancos, Pardos, Pretos e Amarelos, por ano
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & racacor %in% c("Parda","Branca","Preta","Amarela") ) |>
  #Contagem de suicídio de indígena
  count(ano, name = "br_suic") -> suic_br

#Join suicídio Brasil e Suicídio Indígena
left_join(x = suic_ind, y = suic_br, by = join_by("ano") ) -> suic
rm(suic_br,suic_ind)


##Importando população de indígenas e geral
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Criando população de interesse
  mutate(pop_geral = Pop.Branca + Pop.Preta + Pop.Amarela + Pop.Parda) |>
  
  #Selecionando população indígena
  select(uf = UF, ano,
         pop_ind = Pop.Indígena,
         pop_geral) |>
  
  #Excluindo as regiões. Não utilizado
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
  #Total Brasil da pop geral e pop indígena 
  summarise(pop_geral = sum(pop_geral),
            pop_ind = sum(pop_ind), .by = ano) |>
  mutate(ano = ano |> as_factor() )
rm(excel_pnadc)


#Join de suicídio e população
left_join(x = pop_pnadc, y = suic, by = join_by("ano") ) |>
  #Taxa de suicídio de indígenas Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_suic_br = format(round((br_suic/pop_geral)*100000,digits = 1) ) |> as.double(),
         
         tx_suic_ind = format(round((ind_suic/pop_ind)*100000,digits = 1) ) |> as.double() ) |>
 #Mantém séries de intersse
 select(ano, br_suic, ind_suic, tx_suic_br, tx_suic_ind) |>
  rio::export(x = _, "suicído_geral e indígena.xlsx")

rm(list = ls())
  



# Seção Idosos SIM --------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base SIM
load("C:/Users/p224552695/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Homicídio idoso raça\cor
sim_doext |> 
  #Refazendo raçacor e acrescentando racacor negra e não negra nos microdado do SIM 
  mutate(racacor = case_match(racacor, c("Parda","Preta") ~ "Negro",
                              c("Amarela","Branca","Indigena") ~ "Não Negro", .default = racacor) |> as_factor() ) |> 
  #Filtro das características desejadas
  filter(idade>=60 & intencao_homic == "Homicídio") |> 
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,sexo,racacor, name = "Homicídio", .drop = FALSE ) |> 
  #Filtro para eliminar anos, sexo e racacor não investigada.
  filter(racacor != "Ignorado" & sexo != "Ignorado") |> 
  #Deixando racacor igual ao informando em pop_pnadc_idoso
  mutate(racacor =  case_when(sexo == "Homem" & racacor == "Não Negro" ~ "h_nao_negro",
                              sexo == "Homem" & racacor == "Negro" ~ "h_negro",
                              sexo == "Mulher"& racacor == "Não Negro" ~ "m_nao_negra",
                              sexo == "Mulher" & racacor == "Negro" ~ "m_negra",.default = racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,racacor,Homicídio) -> homic


#Quedas raça cor
#cid10 de quedas é w00 a w19
sim_doext |> 
  #Refazendo raçacor e acrescentando racacor negra e não negra nos microdado so SIM 
  mutate(racacor = case_match(racacor, c("Parda","Preta") ~ "Negro",
                              c("Amarela","Branca","Indigena") ~ "Não Negro",.default = racacor) |> as_factor()) |> 
  #Filtro das características desejadas
  filter(idade>=60 & (causa_letra == "W" & causa_num %in% c(0:19))) |> #cid10 de quedas é w00 a w19
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,sexo,racacor, name = "Queda", .drop = FALSE ) |> 
  #Filtro para eliminar anos, sexo e racacor não investigada. 
  filter(racacor != "Ignorado" & sexo != "Ignorado") |>
  #Deixando racacor igual ao informando em pop_pnadc_idoso
  mutate(racacor =  case_when(sexo == "Homem" & racacor == "Não Negro" ~ "h_nao_negro",
                              sexo == "Homem" & racacor == "Negro" ~ "h_negro",
                              sexo == "Mulher"& racacor == "Não Negro" ~ "m_nao_negra",
                              sexo == "Mulher" & racacor == "Negro" ~ "m_negra",.default = racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,racacor,Queda) -> queda


#Acidente de trânsito
#Cid 10 de sinistro de trânsito é V01 a V99
sim_doext |>
  #Refazendo raçacor e acrescentando racacor negra e não negra nos microdado do SIM 
  mutate(racacor = case_match(racacor, c("Parda","Preta") ~ "Negro",
                              c("Amarela","Branca","Indigena") ~ "Não Negro",.default = racacor) |> as_factor()) |> 
  #Filtro das características desejadas
  filter(idade>=60 & racacor != "Ignorado" & (causa_letra == "V" & causa_num %in% c(0:99))) |> #cid10 de acidente transporte é V01 a V99
  #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
  count(ano,sexo,racacor, name = "trans", .drop = FALSE ) |> 
  #Filtro para eliminar anos, sexo e racacor não investigada. 
  filter(racacor != "Ignorado" & sexo != "Ignorado") |> 
  #Deixando racacor igual ao informando em pop_pnadc_idoso
  mutate(racacor =  case_when(sexo == "Homem" & racacor == "Não Negro" ~ "h_nao_negro",
                              sexo == "Homem" & racacor == "Negro" ~ "h_negro",
                              sexo == "Mulher"& racacor == "Não Negro" ~ "m_nao_negra",
                              sexo == "Mulher" & racacor == "Negro" ~ "m_negra",.default = racacor) |> as_factor()) |>
  #Mantém variáveis utilizadas
  select(ano,racacor,trans) -> trans

#Importando população de idosos.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/p224552695/Dropbox/Ipea/Atlas/Pop-PNADc-60+.xlsx"

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
  pivot_longer(cols = !c(uf,ano), names_to = "racacor", names_transform = list(racacor = as.factor),
               values_to = "pop_pnadc") |>
  
  #População Brasil
  summarise(pop_pnadc = sum(pop_pnadc), .by = c(ano,racacor) )  
rm(excel_pnadc)


#Join de população, homicídio, Quedas e acidetne de transporte
list(pop_pnadc,homic,queda,trans) %>% reduce(left_join, by = c("ano","racacor")) |>
  #Taxas de homicídio, Acidente e Transporte IDOSO. Padrão Atlas, somente um casa decimal.
  mutate(across(c(Homicídio, Queda, trans), ~ round((./pop_pnadc)*100000,1),.names="tx_{col}")) |>
  #Mantém variáveis utilizadas
  select(ano,racacor,starts_with("tx")) -> base
rm(homic,queda,trans,pop_pnadc)

#Exportação das taxas de interesse
base |>
  mutate(racacor = racacor |> fct_relevel("h_negro","h_nao_negro","m_negra","m_nao_negra")) |>
  arrange(racacor) |> 
  pivot_longer(cols = starts_with("tx"), names_to = "taxas") |>
  pivot_wider(names_from = ano, values_from = "value") |>
  arrange(taxas) |>
  rio::export(x = _, "tx_homic_queda_acide_idoso.xlsx")
  




# Homicídio por PAF -------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_13_23.RData")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio") |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, uf_resd, instrumento, name = "homic_int") -> homic


##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "C:/Users/gabli/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral
  select(uf = UF,ano, pop = Pop)
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

#Join homicídios e população.  
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homic_int = sum(homic_int), .by = c(instrumento,ano) ) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic_int/pop)*100000,digits = 1) ) |> as.double(),
         prop = round(homic_int/sum(homic_int)*100,1), .by = c(uf_resd,ano) ) |>
  #Mantém somente instrumento PAF
  filter(instrumento == "PAF") -> base

#Tabela formato wide do número de homicídios por arma de fogo.
base |> select(ano,uf_resd,homic_int) |>
  pivot_wider(names_from = ano, values_from = homic_int, values_fill = 0) %>%
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
  adorn_title(placement = "top",col_name = "Homicídios por arma de fogo, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x = _ ,"homic_paf_uf_resd.xlsx")


#Tabela formato wide da taxa de homicídios por Arma de fogo.
base |> select(ano, uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de homicídios por arma de fogo, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_homicidio_PAF_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios por Arma de fogo.
base |> select(ano, uf_resd, prop) |>
  pivot_wider(names_from = ano, values_from = prop, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Proporção de homicídios por arma de fogo, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"prop_homicidio_PAF_uf_br.xlsx")

rm(base, sim_doext)




# Acidente de transito ----------------------------------------------------
library(tidyverse)
library(janitor)


#Importando base. Peguei no datasus
acid <- 
  readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas 2025/base/Transito/acidente_transporte.xlsx") |>
  pivot_longer(cols = !c(cod_ibge,uf_resd), names_to = "ano", values_to = "n_acid", 
               names_transform = list(ano = as.factor) )


##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral
  select(uf = UF,ano, pop = Pop)
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

#Join homicídios e população.  
left_join(x = acid, y = pop_pnadc, by = join_by("ano", "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_ibge) ) -> base
rm(acid,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        n_acid = sum(n_acid), .by = c(ano) ) ) |> 
  #Taxa de acidentes de transporte. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_acid = format(round((n_acid/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide da taxa de acidentes de transito.
base |> select(ano, uf_resd, tx_acid) |>
  pivot_wider(names_from = ano, values_from = tx_acid, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de acidentes de transito, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_acid_terrestre_uf_br.xlsx")





# Acidente de motocicleta -------------------------------------------------
library(tidyverse)
library(janitor)


#Importando base. Peguei no datasus
acid <- 
  readxl::read_excel("D:/Dropbox/Ipea/Atlas/Atlas 2025/base/Transito/acidente_moto.xlsx") |>
  pivot_longer(cols = !c(cod_ibge,uf_resd), names_to = "ano", values_to = "n_acid", 
               names_transform = list(ano = as.factor) )


##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população geral
  select(uf = UF,ano, pop = Pop)
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

#Join homicídios e população.  
left_join(x = acid, y = pop_pnadc, by = join_by("ano", "cod_ibge","uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_ibge) ) -> base
rm(acid,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        n_acid = sum(n_acid), .by = c(ano) ) ) |> 
  #Taxa de acidentes de transporte. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_acid = format(round((n_acid/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato wide da taxa de acidentes de moto
base |> select(ano, uf_resd, tx_acid) |>
  pivot_wider(names_from = ano, values_from = tx_acid, values_fill = 0) %>%
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
  adorn_title(placement = "top", col_name = "Taxa de acidentes de moto, por UF – Brasil (2013-2023)") |>
  #Exportando tabela.
  rio::export(x= _ ,"taxa_acid_moto_uf_br.xlsx")






# Tabela estatísticas descritivas -----------------------------------------
library(gtsummary)
library(kableExtra)
library(tidyverse)
library(janitor)
load("C:/Users/gabli/Desktop/r/SIM/Negros/sim_doext_12_22.RData")


theme_gtsummary_language(language  = "pt", big.mark = ".",decimal.mark=",")
sim_doext |> filter(intencao_homic == "Homicídio") |>
  #Acrescentando raçacor Negra nos microdado so SIM 
  mutate(racacor = case_match(racacor,
                              c("Parda","Preta") ~ "Negra",
                              c("Indigena","Branca","Amarela") ~ "Não Negra",
                              .default = racacor) |> fct_relevel("Negra", "Não Negra", "Ignorado") ) |> 
  select(c(racacor,idade,esc,estciv,local_obito, instrumento, sexo) ) |>
  #Declarando a tabela
  tbl_summary(
              by = racacor, 
              
              percent = "column",
              
              #missing_text = "(Missing)",
              missing = "no",
              
              #Alterando o label das variáveis 
              label = list(idade ~ "Idade", sexo ~ "Sexo", estciv ~ "Estado Civil", esc ~ "Escolaridade",
                           local_obito ~ "Local do incidente",instrumento ~ "Instrumento"),
              
              type = all_continuous() ~ "continuous2",
              #Alterando estatísticas utilizadas
              statistic = all_continuous() ~ c(
                "{N_nonmiss} ({p_nonmiss}%)",
                "{N_miss} ({p_miss}%)",
                "{mean} ({sd})",
                "{median} ({p25}, {p75})",
                "{min} - {max}"),
              
              #Alterando número de dígitos em cada linha das estatísticas de idade.
              digits = list(idade ~ c(0,1,0,1,1,1,1,1,0,0) ) ) |>
  
  #Adicionando coluna com valores totais 
  add_overall(col_label = "Homicídios - Geral",last = TRUE) |>
  
  #Alterando label das estatísticas de idade.
  add_stat_label(label = idade ~ c("N(%)", "N faltante (%)", "Média (SD)", "Mediana (IQR)", "Min - Max")) |>
  
  #Alterando header da tabela
  modify_spanning_header(c("stat_1","stat_2","stat_3") ~ "**Homicídio, por raça\\cor**") %>%
  #
  modify_header(label = "**Características**",   # update the column header
                #Alterando as estatísticas dos totais apresentados no topo da tabela.
                all_stat_cols() ~ "**{level}** N = {scales::number(n)} ({style_percent(p,digits = 1)}%)") |> bold_labels() |>
  as_hux_xlsx("example_gtsummary2.xlsx")


  as_flex_table() %>% save_as_docx(path = "test.docx")
