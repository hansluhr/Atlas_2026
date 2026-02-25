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
                       col_name = glue::glue("Número de Autorizações de Internações Hospitalares (AIH), por UF {min(year)}–{max(year)}") )  
  #Exportando tabela.
  rio::export(x = _,"Tabelas/n_aih.xlsx")