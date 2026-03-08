                                       #### Tabelas Atlas da Violência 2026 ####

# Número de Homicídio Geral ---------------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)


#Número de Homicídios - Geral
sim_doext |>
  filter(intencao_homic == "Homicídio" & ano %in% year) |>
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
                "Sergipe","Tocantins"), def_uf_resd) )  |>
  #Necessário para colocar o título
  as_tibble() |>
  #Nota de rodapé
  add_row(
  def_uf_resd = "MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade (SIM). Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>

  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de homicídios, por UF {min(year)}–{max(year)}") ) |> 
 
  #Exportando tabela.
  rio::export(x = _,"base/homic/base/n_homicidio_uf_br.xlsx")


# #Taxa de homicídios - Geral ---------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)

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
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx") 

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
  rio::export(x= _ ,"base/homic/base/tx_homicidio_uf_br.xlsx")


# Homicídios de Jovens ------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)

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
    def_uf_resd = "MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade (SIM). Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios de Jovens (15 a 29 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/n_homicidio_jovem_uf_br.xlsx")


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
              col_name = glue::glue("Taxa de Homicídios de Jovens (15 a 29 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/taxa_homicidio_jovem_uf_br.xlsx")
rm(base)



# Anos Potenciais de vida Perdidos ----------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)


#Método Romeder e McWhinnie.
sim_doext |> slice_sample(n=1000) |>
  #SIM duckdb
  mutate(idade = idade |> as.integer() ) |>
  
  filter(ano %in% year & idade %in% c(1:69) & !intencao %in% c("Indeterminado") ) |> 
  #Quero o número de homicídios por idade
  group_by(intencao) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP)
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Gráfico em barras apresentando o APVP
  ggplot() +
  geom_col(aes(x=idade,y=apvp, fill = intencao), show.legend = FALSE) +
  facet_wrap(vars(intencao), scales = "free") + 
  scale_x_continuous(breaks = seq(1,69,3) ) + 
  #scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3) ) +
  #Ajuste eixo y
  ggh4x::facetted_pos_scales(y = list(
    intencao == "Homicídio" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                                 breaks = seq(0, 1250000, 250000) ), 
    intencao == "Acidente" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                                breaks = seq(0,600000,100000) ) )  )  +
  theme(strip.text = element_text(size=10, face="bold"),
        axis.title.y = element_text(size = 10) ) + labs(fill = "", x = "", y = "APVP (Mil anos)")
ggsave(filename = "APVP.bmp", width = 13,height = 8,dpi=200)



#Anos potenciais de vida perdidos - Intencionalidades no mesmo gráfico
sim_doext |> #slice_sample(n=1000) |>
  mutate(intencao = recode(intencao,"h_legal" = "Homicídio")) |>
  filter(ano %in% year & idade %in% c(1:69) & !intencao %in% c("Indeterminado") ) |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP)
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Gráfico em barras apresentando o APVP
  ggplot() +
  geom_col(aes(x=idade,y=apvp, fill = intencao),alpha=0.5, position="identity") +
  scale_x_continuous(breaks = seq(1,69,2) ) + 
  scale_y_continuous(breaks = seq(0, 1250000, 250000),
                     labels = scales::label_number(scale = 1 / 1e3) ) +
  guides(fill = guide_legend(position = "inside") ) +
  theme(axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size=10, face = "bold"),
        legend.position.inside = c(0.7, 0.85),legend.direction = "horizontal",
        legend.text = element_text(size = 17, face="bold"),
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  labs(fill = "", x = "", y = "APVP (Mil anos)")
ggsave(filename = "APVP.bmp", width = 13,height = 8,dpi=200)



#Anos potenciais de vida perdidos - - Polígonos vazados nas outras idades e por instrumento
sim_doext |> #slice_sample(n=1000) |>
  #Sim duckdb 
  mutate(idade = idade |> as.integer() ) |>
  filter(ano %in% year & idade %in% c(0:69) & intencao_homic == "Homicídio" ) |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao_homic,instrumento) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP)
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Gráfico em barras apresentando o APVP
  ggplot() +
  geom_col(aes(x=idade,y=apvp, fill = instrumento),position="stack") +
  annotate('rect', xmin=30, xmax=70, ymin=0, ymax=Inf, alpha=.7, fill='white') +
  annotate('rect', xmin=-1, xmax=14, ymin=0, ymax=Inf, alpha=.7, fill='white') +
  scale_x_continuous(breaks = seq(0,69,2) ) + 
  scale_y_continuous(breaks = seq(0, 1250000, 250000), 
                     labels = scales::label_number(scale = 1 / 1e3) ) +
  guides(fill = guide_legend(position = "inside", nrow = 4,title.position = "top") ) +
  theme(legend.title = element_text(size = 13, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size=9, face = "bold"),
        legend.position.inside = c(0.7, 0.85),legend.direction = "horizontal",
        legend.text = element_text(size = 11.5, face="bold"),
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  labs(fill = "Instrumento da causa básica do óbito", x = "", y = "APVP (Mil anos)")

ggsave(filename = "base/eca/figura/APVP.bmp", width = 13,height = 10,dpi=250)
ggsave(filename ="base/eca/figura/APVP.eps",width = 15,height = 10,device=cairo_ps, dpi=350)



#Tabelas - APVP por idade
sim_doext |> 
  
  mutate(idade = idade |> as.integer() ) |>
  
  filter(ano %in% year & idade %in% c(15:29) & intencao_homic == "Homicídio") |> droplevels() |>
  #Quero o número de homicídios por idade
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP) - Por idade
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/eca/base/apvp_eca.xlsx")


#Tabelas - APVP intencao
sim_doext |> 
  #sim duckdb
  mutate(idade = idade |> as.integer() ) |>
  
  filter(ano %in% year & idade %in% c(0:69)) |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP) - Por idade
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Valor total de APVP de jovens
  filter(idade %in% c(15:29) ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/eca/base/apvp_eca_intencao.xlsx")



#Tabelas - APVP instrumento
sim_doext |> 
  
  #Sim duckdb
  mutate(idade = idade |> as.integer() ) |>
  
  filter(ano %in% year & idade %in% c(0:69) & intencao_homic == "Homicídio") |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao_homic,instrumento) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP) - Por idade
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Valor total de APVP de jovens
  filter(idade %in% c(15:29) ) |> summarise(apvp = sum(apvp) ) |>
  mutate(total = sum(apvp),
         prop = (apvp/total)*100) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/eca/base/apvp_eca_homic_instrumento.xlsx")




# Suicídios de Jovens -----------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & idade %in% c(10:19) ) |>
  
  count(ano, cod_uf_resd, def_uf_resd, name = "suicidio") %>%
  
  bind_rows(. |>
          summarise(
            cod_uf_resd = "1",
            def_uf_resd="Brasil",
                      suicidio = sum(suicidio), .by=ano) ) -> suic
  
#Join com a base populacional.
left_join(x = 
readxl::read_excel("base/pop_10a19.xlsx", 
                        sheet = "Plan1") |>
  pivot_longer(cols = !c(cod_uf_resd,def_uf_resd), names_to = "ano", values_to = "pop")  |>
  
  mutate(cod_uf_resd = cod_uf_resd |> as.character() ) ,y = suic, by = join_by(cod_uf_resd, def_uf_resd, ano) ) |>
  
  #Taxa padrão atlas.
  mutate(tx_suic = format(round((suicidio/pop)*100,digits = 1) ) |> as.double() ) -> base


rm(suic)


#Tabela formato wide do número de suicídios de jovens.
base |> select(ano,def_uf_resd, suicidio) |>
  pivot_wider(names_from = ano, values_from =  suicidio) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de suicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X60-X84, ou seja, lesões autoprovocadas voluntariamente. O número de óbitos foi obtido pela soma de indivíduos de 10 a 19 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Suicídios de Jovens (15 a 29 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/eca/base/n_suicdio_jovem_uf_br.xlsx")


### Tabela com taxa de homicídios Jovens


#Tabela formato wide do número de homicídios de jovens.
base |> select(ano,def_uf_resd, tx_suic) |>
  pivot_wider(names_from = ano, values_from =  tx_suic) %>%
  
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de suicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X60-X84, ou seja, lesões autoprovocadas voluntariamente. O número de óbitos e da população foi obtido pela soma de indivíduos de 10 a 19 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de Suicídios de Jovens (15 a 29 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/eca/base/tx_suicd_jovem_uf_br.xlsx")
rm(base)


# Homicídio de Homens Jovens ----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)


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
  rio::export(x= _ ,"base/homic/base/n_homicidio_homem_jovem_uf_br.xlsx")

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
  rio::export(x= _ ,"base/homic/base/tx_homicidio_homem_jovem_uf_br.xlsx")
rm(base)



#Homicídios de crianças de 0 a 4 anos por UF -----------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)



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
  rio::export(x= _ ,"base/homic/base/n_homicidio_infatil04_uf_br.xlsx")


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
    def_uf_resd = "Fonte: IBGE - Projeções da População do Brasil e Unidades da Federação por sexo e idade: 2000-2070 e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos e da população foi obtido pela soma de indivíduos de 0 a 4 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios registrados de infantes (0 a 4 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/homic/base/tx_homicidio_infantil04_uf_br.xlsx")




# Homicídio de criança 5 a 14 por ano e UF ---------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)
fxet <- c(5:14)

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
  filter(idade %in% fxet & ano %in% year) |>
  
  #Variáveis não utilizadas
  select(!c(sexo,idade) ) |>
  #Somatório das idades para encontra a faixa etária desejada.
  summarise(pop_inf5_14 = sum(pop), .by = c(cod_uf_resd,def_uf_resd,ano) )


#Tabela com homicídios infatil 0 a 4. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% fxet) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(def_uf_resd, cod_uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf5_14


#Join entre população infatil e homicídio infantil
left_join(pop,homic_inf5_14, by = join_by("cod_uf_resd","def_uf_resd","ano") ) -> base
rm(homic_inf5_14, pop)  

#Acrescentando total Brasil e criando taxa
base |>
  #Adiciona zero, quando não ocorre homicídio no ano, uf e idade de interesse
  mutate(homic = replace_na(homic,0) ) %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" ,
                        pop_inf5_14 = sum(pop_inf5_14),
                        homic = sum(homic, na.rm = TRUE), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf5_14)*100000,digits = 1) ) |> as.double() ) -> base


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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos foi obtido pela soma de indivíduos de 5 a 14 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de homicídios registrados de infantes (5 a 14 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/n_homicidio_infatil5_14_uf_br.xlsx")


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
    def_uf_resd = "Fonte: IBGE - Projeções da População do Brasil e Unidades da Federação por sexo e idade: 2000-2070 e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos e da população foi obtido pela soma de indivíduos de 5 a 14 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios registrados de infantes (5 a 14 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/homic/base/tx_homicidio_infantil5_14_uf_br.xlsx")


rm(list = base); gc()


# Homicídio adolescente 15 a 19 -------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)
fxet <- c(15:19)

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
  filter(idade %in% fxet & ano %in% year) |>
  
  #Variáveis não utilizadas
  select(!c(sexo,idade) ) |>
  #Somatório das idades para encontra a faixa etária desejada.
  summarise(pop_inf15_19 = sum(pop), .by = c(cod_uf_resd,def_uf_resd,ano) )


#Tabela com homicídios infatil 0 a 4. 
sim_doext |> filter(intencao_homic == "Homicídio" & idade %in% fxet) |>
  #Contagem pela UF DE RESIDÊNCIA
  count(def_uf_resd, cod_uf_resd, ano, .drop = FALSE, name = "homic") -> homic_inf15_19


#Join entre população infatil e homicídio infantil
left_join(pop,homic_inf15_19, by = join_by("cod_uf_resd","def_uf_resd","ano") ) -> base
rm(homic_inf15_19, pop)  

#Acrescentando total Brasil e criando taxa
base |>
  #Adiciona zero, quando não ocorre homicídio no ano, uf e idade de interesse
  mutate(homic = replace_na(homic,0) ) %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" ,
                        pop_inf15_19 = sum(pop_inf15_19),
                        homic = sum(homic, na.rm = TRUE), .by=ano) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic/pop_inf15_19)*100000,digits = 1) ) |> as.double() ) -> base


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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos foi obtido pela soma de indivíduos de 15 a 19 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de homicídios registrados de infantes (15 a 19 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/n_homicidio_infatil15_19_uf_br.xlsx")


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
    def_uf_resd = "Fonte: IBGE - Projeções da População do Brasil e Unidades da Federação por sexo e idade: 2000-2070 e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de óbitos e da população foi obtido pela soma de indivíduos de 15 a 19 anos de idade.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios registrados de infantes (15 a 19 anos), por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/homic/base/tx_homicidio_infantil15_19_uf_br.xlsx")

rm(list = ls()); gc()

                                              

# Instrumento de óbito de infantes, crianças e adolescentes ---------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2)

#Tabela
sim_doext |> 
  
  filter(ano %in% year) |>
  
  #Base sim extraida do duckdb
  mutate(idade = idade |> as.integer() ) |>
  
  
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
  
  tabyl(instrumento,fxet) %>% adorn_totals(where = c("row","col")) |>
  #Substituir valores absolutos no excel
  rio::export(x = _, "base/eca/base/n_homic_instrumento_eca.xlsx")

  # adorn_percentages(denominator  = "col") %>% adorn_pct_formatting(digits = 1) %>%
  # adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) 

 


# Homicídio Mulheres ------------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & def_sexo == "Mulher") |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano,cod_uf_resd,def_uf_resd, name = "homicidio") |>
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic

##Importando população.
#Caminho do excel com pnadc geral
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres
base |> select(ano,def_uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio) %>%
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
  add_row(
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios de mulheres, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/mulheres/base/n_homicidio_mulher_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de mulheres.
base |> select(ano, def_uf_resd, tx_homic) |>
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios de mulheres, por UF {min(year)}–{max(year)}") )  |>
  #Exportando tabela.
  rio::export(x= _ ,"base/mulheres/base/tx_homicidio_mulher_uf_br.xlsx")



# Gráfico com as as três UFs de maior taxa + taxa Brasil
base |>
  select(ano,def_uf_resd,tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, names_prefix = "ano_") |>
  #Ordenando linhas para maiores taxas em 2024
  arrange(desc(ano_2024)) |>
  #Filtro das três UFs de maior taxa em 2023 + a taxa Brasil
  filter(def_uf_resd %in% c(def_uf_resd[1:4]) | def_uf_resd == "Brasil") |>
  #Exportando ranking das UFs mais violêntas, mulher
  rio::export(x = _, "base/mulheres/base/ranking_tx_homic_mulher.xlsx")

rm(list = ls())

# Homicídio Mulher residência\domicílio ---------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Homicídio feminino, local do acidente = residencial. Aqui poderia entrar Habitação coletiva
sim_doext |> 
  
  filter(ano %in% year & intencao_homic == "Homicídio" & def_sexo == "Mulher")  |> droplevels() |>
  
  tabyl(ano,local_incd) |>
  #Homicídios fora da residência. Somatório de todos os locais do incidente, exceto Residencial. Entendo que Hab Coletiva é residencia.
  mutate(homic_out = rowSums(across(!c(ano,Residencial)))) |> 
  rename(homic_resd = Residencial) |> select(ano,homic_out,homic_resd) -> homic_fem


##Importando população feminina
#Caminho do excel com pnadc geral
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
  rio::export(x=_,"base/mulheres/base/tx_homic_fem_resd_out.xlsx")

rm(list = ls() )


# Homicídios mulheres negras ----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & def_sexo == "Mulher" & def_racacor %in% c("Parda","Preta") ) |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano,cod_uf_resd,def_uf_resd, name = "homicidio") |>
  #Sim oriundo do duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic

##Importando população.
#Caminho do excel com pnadc geral
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres
base |> select(ano,def_uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio) %>%
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
  def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negras é a soma de pretas e pardas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
  col_name = glue::glue("Homicídios de mulheres negras, por UF {min(year)}–{max(year)}") ) |>                               
  #Exportando tabela.
  rio::export(x = _ ,"base/mulheres/base/n_homicidio_mulher_negra_uf_br.xlsx")

   ### Taxa de homicídios de mulheres negras ###

                                    
#Tabela formato wide da taxa de homicídios de mulheres.
base |> select(ano, def_uf_resd, tx_homic) |>
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
  def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negras é a soma de pretas e pardas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios de mulheres negras, por UF {min(year)}–{max(year)}") ) |> 
   #Exportando tabela.
  rio::export(x= _ ,"base/mulheres/base/tx_homicidio_mulher_negra_uf_br.xlsx")



rm(list = ls() )



# Homicídio Mulheres Não Negras -------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_sexo == "Mulher" & def_racacor %in% c("Amarela","Branca","Indigena") ) |>
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #base sim extraida do duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic

##Importando população de mulher não negra.
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres não negras
base |> select(ano,def_uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de não negras foi obtido pela soma de brancas, amarelas e indígenas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios de mulheres não negras, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/mulheres/base/n_homicidio_mulher_nao_negra_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de mulheres não negra.
base |> select(ano, def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
  
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVS/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios de mulheres na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de não negras foi obtido pela soma de brancas, amarelas e indígenas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios de mulheres negras, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/mulheres/base/tx_homicidio_mulher_nao_negra_uf_br.xlsx")

rm(list = ls() )



# Homicídio de negros -----------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_racacor %in% c("Parda","Preta") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #Sim extraido do duckdb.
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic


##Importando população de negros.
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de mulheres não negras
base |> select(ano,def_uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
  
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negros foi obtido pela soma de pardos e pretos.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios de negros, por UF {min(year)}–{max(year)}") ) |> 
   #Exportando tabela.
  rio::export(x = _ ,"base/homic/base/n_homicidio_negro_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de negros.
base |> select(ano, def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negros foi obtido pela soma de pardos e pretos.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios negros, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/tx_homicidio_negro_uf_br.xlsx")


rm(list = ls() )


# Homicídio de Negros - Capitais ----------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Paniel das capitais
base_munic <- 
  geobr::read_capitals() |> sf::st_drop_geometry() |> 
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6 dígitos. 
  mutate(code_muni = code_muni |> str_sub(start = 1, end = 6), 
         across( c(code_muni), ~ as_factor(.x) ),
         name_muni = replace_values(x = name_muni,
                        "Brasília" ~ "Distrito Federal") ) |> 
  #Exclusão de variáveis não utilizadas  
  select(!c(code_state,code_region,name_region,year)) |> 
  #Painel de municípios
  crossing(ano = year)  


#Contagem de homicídios registrados, por ano nas capitais.
sim_doext |> 
  
  #Base sim extraída do duckdb
  mutate(ano = ano |> as.integer() ) |>
  
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_racacor %in% c("Parda","Preta") & 
           #Mantém somente capitias   
           codmunresd %in% c( "120040", "270430", "160030", "130260", "292740", "230440", "530010", "320530", "520870", "211130",
                              "510340", "500270", "310620", "150140", "250750", "410690", "261160", "221100", "330455", "240810",
                              "431490", "110020", "140010", "420540", "355030", "280030", "172100") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, codmunresd, name = "homicidio") -> homic

#Join das capitais com homicídios nas capitais
homic <- base_munic |> 
  left_join(x = _, y = homic, 
            join_by("ano","code_muni"== "codmunresd") ) 
rm(base_munic)

#Importando população de capitais Negros
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_Cidades_PNADc.xlsx")

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
  
  #Selecionando população negros
  select(code_muni, ano, pop = Pop.Negro)
rm(excel_pnadc)

#Join de painel capitais com homicídios e pnadc capitais
homic |>
  left_join(x = _, y = pop_pnadc, 
            by = join_by("ano","code_muni" == "code_muni")) %>%
  #Adiciona somatório de todas as capitais do Brasil
  bind_rows(. |>
              summarise(name_muni = "Capitais" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base
rm(homic,pop_pnadc)



#Tabela formato wide nº de homicídios
base |> select(ano,name_muni,homicidio) |>
  
  pivot_wider(names_from = ano, values_from = homicidio) %>%  
  
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
  
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Capitais", "Porto Velho", "Rio Branco", "Manaus", "Boa Vista", "Belém", "Macapá", "Palmas",
                "São Luís", "Teresina", "Fortaleza", "Natal", "João Pessoa", "Recife", "Maceió", "Aracaju", "Salvador",
                "Cuiabá", "Campo Grande", "Goiânia", "Distrito Federal",
                "Belo Horizonte", "Vitória", "Rio De Janeiro", "São Paulo",
                "Curitiba", "Florianópolis", "Porto Alegre"), name_muni) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) |>
  #Necessário para colocar o título
  #as_tibble() |>
  #Nota de rodapé
  add_row(
    name_muni = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na capital de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negros foi obtido pela soma de pardos e pretos.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de Homicídios registrados de negros, por Capitais {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x = _, "base/homic/base/n_homicidio_negro_capitais.xlsx")


#Taxa de homicídio negros nas capitais
base |> select(ano,name_muni,tx_homic) |>
  
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
      format(round((.[[ncol(.)]] - .[[2]]) / .[[2]] * 100, 1), decimal.mark = ",") ) |>
  
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Capitais", "Porto Velho", "Rio Branco", "Manaus", "Boa Vista", "Belém", "Macapá", "Palmas",
                "São Luís", "Teresina", "Fortaleza", "Natal", "João Pessoa", "Recife", "Maceió", "Aracaju", "Salvador",
                "Cuiabá", "Campo Grande", "Goiânia", "Distrito Federal",
                "Belo Horizonte", "Vitória", "Rio De Janeiro", "São Paulo",
                "Curitiba", "Florianópolis", "Porto Alegre"), name_muni) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) |>
  #Necessário para colocar o título
  #as_tibble() |>
  #Nota de rodapé
  add_row(
    name_muni = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na capital de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de negros foi obtido pela soma de pardos e pretos.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de Homicídios registrados de negros, por Capitais {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x = _, "base/homic/base/tx_homicidio_negro_capitais.xlsx")

rm(list = ls() )



# Homicídio de não negros -------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_racacor %in% c("Amarela","Branca","Indigena") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #Sim extraido duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic


##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homicidio = sum(homicidio), .by=ano) ) |>
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homicidio/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de não negros
base |> select(ano,def_uf_resd,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de não negros foi obtido pela soma de brancos, amarelos e indígenas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios de não negros, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/homic/base/n_homicidio_nao_negro_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de não negros.
base |> select(ano, def_uf_resd, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  
  add_row(
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de não negros foi obtido pela soma de brancos, amarelos e indígenas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios de não negros, por UF {min(year)}–{max(year)}") ) |>
     #Exportando tabela.
  rio::export(x= _ ,"base/homic/base/tx_homicidio_nao_negro_uf_br.xlsx")
rm(base, sim_doext)




# Homicídio Indígena ------------------------------------------------------
library(tidyverse)
library(janitor)

#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_racacor == "Indigena" ) |> 
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #Sim duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic


##Importando população de indígenas
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = pop_pnadc, y = homic , by = join_by("ano", "cod_ibge" == "cod_uf_resd", "uf" == "def_uf_resd" ) ) |>
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
base |> select(ano,uf,homicidio) |>
  pivot_wider(names_from = ano, values_from = homicidio, values_fill = 0) %>%
  
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
                "Sergipe","Tocantins"),uf) ) |>
  # Converte as colunas numéricas para caracteres e substitui pontos por vírgulas.
  #Assim fica mais fácil converter para numérico no excel.
  mutate(across(where(is.numeric), ~ str_replace_all(as.character(.), "\\.", ","))) %>%
  #Nota de rodapé
  add_row(
    uf = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de homicídios de indígenas, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ , "base/homic/base/n_homicidio_indigena_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios de indígenas.
base |> select(ano, def_uf_resd = uf, tx_homic) |>
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
  add_row(
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios de indígenas, por UF {min(year)}–{max(year)}") ) |>
    #Exportando tabela.
  rio::export(x= _ , "base/homic/base/tx_homicidio_indigena_uf_br.xlsx") 
rm(list = ls())




# Taxa Nacional (sem indígenas) x Taxa Indígenas --------------------------
library(tidyverse)
library(janitor)

#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Contagem de homicídios registrados exceto indígenas, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Homicídio" & def_racacor!= "Indigena") |> 
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "homicidio") |>
  #SIm duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) |>
  
  #Join da contagem de homicídios registrados indígenas.
  left_join(x = _, y = sim_doext |> 
              
              #Filtro das intenções de interesse.
              filter(intencao_homic == "Homicídio" & def_racacor == "Indigena") |> 
              
              #Cotagem de homicídios indígenas
              count(ano, cod_uf_resd, def_uf_resd, name = "homicidio_ind") |>
            
            mutate(cod_uf_resd = cod_uf_resd |> as.integer() ),
            
            #Join de homicídio indígena e geral sem indígena
            by = join_by("ano", "cod_uf_resd", "def_uf_resd") ) |>
  #Preenchimento de zeros em UFs sem homicídios de indígenas 
  mutate(homicidio_ind = homicidio_ind |> replace_na(0) ) -> homic


##Importando população de negros.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
          by = join_by("ano", "cod_ibge" == "cod_uf_resd", "uf" == "def_uf_resd" ) ) |>
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
ggsave(filename ="Figuras/tx_homic_geralxindg.bmp",width = 8,height = 5,device='bmp', dpi=150)

rm(list = ls() )


# Suicídio Indígena -------------------------------------------------------
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()

#Contagem de suicídios registrados, por ano e UF
sim_doext |> 
  
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & def_racacor %in% c("Indigena") ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "suic") |>
  #SIM duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> suic


##Importando população de indígenas
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
full_join(x = suic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) |> 
  #Colocando zero nas UFs sem suicídio indígena no ano
  mutate(suic = replace_na(suic,0) ) -> base
rm(suic,pop_pnadc)

#Acrescentando total Brasil e criando taxas
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        suic = sum(suic), .by=ano) ) |> 
  #Taxa de suicídio de indígenas Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_suic = format(round((suic/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios de não negros
base |> select(ano,def_uf_resd,suic) |>
  pivot_wider(names_from = ano, values_from = suic, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de suicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X60-X84, ou seja, lesões autoprovocadas voluntariamente.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Suicídio de indígenas, por UF {min(year)}–{max(year)}") ) |> 
    #Exportando tabela.
  rio::export(x = _ ,"base/indio/base/n_ind_suic_uf_br.xlsx")


#Tabela formato wide da taxa de suicídios de indígenas
base |> select(ano, def_uf_resd, tx_suic) |>
  pivot_wider(names_from = ano, values_from = tx_suic, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de suicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X60-X84, ou seja, lesões autoprovocadas voluntariamente.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de suicídio de indígenas, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/indio/base/tx_ind_suic_uf_br.xlsx")


rm(list = ls())





# Suicídio indígena e Geral -----------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Contagem de suicídios registrados indígenas, por ano
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & def_racacor == "Indigena") |>
  #Contagem de suicídio de indígena
  count(ano, name = "ind_suic") -> suic_ind

#Contagem de suicídios de Brancos, Pardos, Pretos e Amarelos, por ano
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic == "Suicídio" & def_racacor %in% c("Parda","Branca","Preta","Amarela") ) |>
  #Contagem de suicídio de indígena
  count(ano, name = "br_suic") -> suic_br

#Join suicídio Brasil e Suicídio Indígena
left_join(x = suic_ind, y = suic_br, by = join_by("ano") ) -> suic
rm(suic_br,suic_ind)


##Importando população de indígenas e geral
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
  rio::export(x = _, "base/indio/base/suicído_geral e indígena.xlsx")

rm(list = ls())



# Seção Idosos SIM --------------------------------------------------------
# library(tidyverse)
# library(janitor)
# #Pasta raiz
# here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
# #Importação base de interesse
# load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
# year <- c(2014:2024)
# 
# 
# #Homicídio idoso raça\cor
# sim_doext |> 
#   #Refazendo raçacor e acrescentando racacor negra e não negra nos microdado do SIM 
#   mutate(def_racacor = replace_values(def_racacor, 
#                               c("Parda","Preta") ~ "Negro",
#                               c("Amarela","Branca","Indigena") ~ "Não Negro") |> as_factor() ) |> 
#   #Filtro das características desejadas
#   filter(idade>=60 & intencao_homic == "Homicídio") |> 
#   #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
#   count(ano,def_sexo,def_racacor, name = "Homicídio", .drop = FALSE ) |> 
#   #Filtro para eliminar anos, sexo e racacor não investigada.
#   filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |> 
#   #Deixando racacor igual ao informando em pop_pnadc_idoso
#   mutate(def_racacor =  case_when(def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
#                               def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
#                               def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
#                               def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
#   #Mantém variáveis utilizadas
#   select(ano,def_racacor,Homicídio) -> homic
# 
# 
# #Quedas raça cor
# #cid10 de quedas é w00 a w19
# sim_doext |> 
#   #Refazendo raçacor e acrescentando def_racacor negra e não negra nos microdado so SIM 
#   mutate(def_racacor = case_match(def_racacor,
#                               c("Parda","Preta") ~ "Negro",
#                               c("Amarela","Branca","Indigena") ~ "Não Negro",.default = def_racacor) |> as_factor()) |> 
#   #Filtro das características desejadas
#   filter(idade>=60 & (causa_letra == "W" & causa_num %in% c(0:19))) |> #cid10 de quedas é w00 a w19
#   #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
#   count(ano,def_sexo,def_racacor, name = "Queda", .drop = FALSE ) |> 
#   #Filtro para eliminar anos, def_sexo e def_racacor não investigada. 
#   filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |>
#   #Deixando def_racacor igual ao informando em pop_pnadc_idoso
#   mutate(def_racacor =  case_when(
#                               def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
#                               def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
#                               def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
#                               def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
#   #Mantém variáveis utilizadas
#   select(ano,def_racacor,Queda) -> queda
# 
# 
# #Acidente de trânsito
# #Cid 10 de sinistro de trânsito é V01 a V99
# sim_doext |>
#   #Refazendo raçacor e acrescentando def_racacor negra e não negra nos microdado do SIM 
#   mutate(def_racacor = case_match(def_racacor, c("Parda","Preta") ~ "Negro",
#                               c("Amarela","Branca","Indigena") ~ "Não Negro",.default = def_racacor) |> as_factor()) |> 
#   #Filtro das características desejadas
#   filter(idade>=60 & def_racacor != "Ignorado" & (causa_letra == "V" & causa_num %in% c(0:99))) |> #cid10 de acidente transporte é V01 a V99
#   #Contagem das características desejadas. .drop = FALSE mantém contagens zero na tabela.
#   count(ano,def_sexo,def_racacor, name = "trans", .drop = FALSE ) |> 
#   #Filtro para eliminar anos, def_sexo e def_racacor não investigada. 
#   filter(def_racacor != "Ignorado" & def_sexo != "Ignorado") |> 
#   #Deixando def_racacor igual ao informando em pop_pnadc_idoso
#   mutate(def_racacor =  case_when(def_sexo == "Homem" & def_racacor == "Não Negro" ~ "h_nao_negro",
#                               def_sexo == "Homem" & def_racacor == "Negro" ~ "h_negro",
#                               def_sexo == "Mulher"& def_racacor == "Não Negro" ~ "m_nao_negra",
#                               def_sexo == "Mulher" & def_racacor == "Negro" ~ "m_negra",.default = def_racacor) |> as_factor()) |>
#   #Mantém variáveis utilizadas
#   select(ano,def_racacor,trans) -> trans
# 
# #Importando população de idosos.
# #Caminho do excel com pnadc
# excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop-PNADc-60+.xlsx") 
# 
# #Importação e empilhando os últimos dez anos da PNADc
# pop_pnadc <- map_dfr(
#   #Tail informa que desejamos os últimos dez anos da pnadc
#   .x = tail(readxl::excel_sheets(excel_pnadc), 11),
#   
#   ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |>
#   
#   #Selecionando população homem negro, homem não negro, mulher negro e mulher não negro
#   select(uf=UF,ano, h_negro = H.Negro, h_nao_negro = H.Não_Negra, m_negra = M.Negro, m_nao_negra = M.Não_Negra) |>
#   
#   #Excluindo as regiões. Não utilizado
#   filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul") ) ) |>
#   
#   mutate(
#     ano = ano |> as_factor()) |> 
#   
#   #Formato Long para facilitar o join
#   pivot_longer(cols = !c(uf,ano), names_to = "def_racacor", names_transform = list(def_racacor = as.factor),
#                values_to = "pop_pnadc") |>
#   
#   #População Brasil
#   summarise(pop_pnadc = sum(pop_pnadc), .by = c(ano,def_racacor) )  
# rm(excel_pnadc)
# 
# 
# #Join de população, homicídio, Quedas e acidetne de transporte
# list(pop_pnadc,homic,queda,trans) %>% reduce(left_join, by = c("ano","def_racacor")) |>
#   #Taxas de homicídio, Acidente e Transporte IDOSO. Padrão Atlas, somente um casa decimal.
#   mutate(across(c(Homicídio, Queda, trans), ~ round((./pop_pnadc)*100000,1),.names="tx_{col}")) |>
#   #Mantém variáveis utilizadas
#   select(ano,def_racacor,starts_with("tx")) -> base
# rm(homic,queda,trans,pop_pnadc)
# 
# #Exportação das taxas de interesse
# base |>
#   mutate(def_racacor = def_racacor |> fct_relevel("h_negro","h_nao_negro","m_negra","m_nao_negra")) |>
#   arrange(def_racacor) |> 
#   pivot_longer(cols = starts_with("tx"), names_to = "taxas") |>
#   pivot_wider(names_from = ano, values_from = "value") |>
#   arrange(taxas) |>
#   rio::export(x = _, "base/tx_homic_queda_acide_idoso.xlsx")


# Homicídio por PAF -------------------------------------------------------
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
  filter(intencao_homic == "Homicídio") |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, instrumento, name = "homic_int") |>
  #SIM duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> homic


##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = homic, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(homic,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        homic_int = sum(homic_int), .by = c(instrumento,ano) ) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_homic = format(round((homic_int/pop)*100000,digits = 1) ) |> as.double(),
         prop = round(homic_int/sum(homic_int)*100,1), .by = c(def_uf_resd,ano) ) |>
  #Mantém somente instrumento PAF
  filter(instrumento == "PAF") -> base

#Tabela formato wide do número de homicídios por arma de fogo.
base |> select(ano,def_uf_resd,homic_int) |>
  
  pivot_wider(names_from = ano, values_from = homic_int, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios po arma de fogo na UF de residência foi obtido pela soma das seguintes CIDs 10: X93-X95 e Y35.0, ou seja, óbitos causados por agressão por disparo de arma de fogo e intervenções legais causadas por arma de fogo.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Homicídios por arma de fogo, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x = _ ,"base/n_homic_paf_uf_resd.xlsx")


#Tabela formato wide da taxa de homicídios por Arma de fogo.
base |> select(ano, def_uf_resd, tx_homic) |>
  
  pivot_wider(names_from = ano, values_from = tx_homic, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF por arma de fogo de residência foi obtido pela soma das seguintes CIDs 10: X93-X95 e Y35.0, ou seja, óbitos causados por agressão por disparo de arma de fogo e intervenções legais causadas por arma de fogo.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de homicídios por arma de fogo, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/taxa_homicidio_PAF_uf_br.xlsx")


#Tabela formato wide da taxa de homicídios por Arma de fogo.
base |> select(ano, def_uf_resd, prop) |>
  pivot_wider(names_from = ano, values_from = prop, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de homicídios na UF de residência foi obtido pela soma das seguintes CIDs 10: X85-Y09 e Y35 - Y36, ou seja, óbitos causados por agressão, intervenção legal e operações de guerra. O número de homicídios por arma de fogo na UF de residência foi obtido pela soma das seguintes CIDs 10: X93-X95 e Y35.0, ou seja, óbitos causados por agressão por disparo de arma de fogo e intervenções legais causadas por arma de fogo.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Proporção de homicídios por arma de fogo, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x= _ ,"base/prop_homicidio_PAF_uf_br.xlsx")

rm(base, sim_doext)




# Acidente de Transporte terrestre ----------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)


#Contagem de sinistros no transporte terrestre
sim_doext |>
  filter(
    #V01-V09 Pedestre traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(0:9) ) |
    #V10-V19 Ciclista traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(10:19) ) |
    #V20-V29 Motociclista traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(20:29) ) |
    #V30-V39 Ocupante de triciclo motorizado traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(30:39) ) |
    #V40-V49 Ocupante de um automóvel traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(40:49) ) |
    #V50-V59 Ocupante de uma caminhonete traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(50:59) ) |
    #V60-V69 Ocupante de um veículo de transporte pesado traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(60:69) ) |
    #V70-V79 Ocupante de um ônibus traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(70:79) ) |
    #V80-V89 Outros acidentes de transporte terrestre
    (causa_letra == "V" & causa_num %in% c(80:89) ) ) |>

  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "acid_terres") |> 
  #SIM duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> acid_terres

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = acid_terres, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(acid_terres,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        acid_terres = sum(acid_terres), .by = c(ano) ) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_acid_terres = format(round((acid_terres/pop)*100000,digits = 1) ) |> as.double() ) -> base

#Tabela formato wide do número de homicídios por arma de fogo.
base |> select(ano,def_uf_resd,acid_terres) |>
  
  pivot_wider(names_from = ano, values_from = acid_terres, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de acidentes na UF de residência foi obtido pela soma das seguintes CIDs 10: V01-V89, ou seja, sinistros envolvendo pedestres, ciclistas, motociclitas, ocupantes de triciclo, automóvel, caminhonete,  veículo de transporte pesado, ônibus ou outros veículos terrestres não especificados.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Mortes no transporte terrestre, por UF {min(year)}–{max(year)}") ) |> 
  #Exportando tabela.
  rio::export(x = _ ,"base/n_transp_terres_uf_resd.xlsx")


             #Figura Número de Mortes no transporte terrestre#
base |>
 
  filter(def_uf_resd == "Brasil") |>
  
  mutate(ano = ano |> make_date() ) |>
  #Gráfico
  ggplot() +
  
  geom_line(aes(x = ano, y = acid_terres), linetype = "longdash", 
            lineend = "round", linejoin = "round", linewidth = 0.5) + 
  
  geom_point(aes(x = ano, y = acid_terres) ) +
  
  ggrepel::geom_text_repel(aes(x = ano, y = acid_terres,
                           label = scales::number(acid_terres, big.mark = ".", decimal.mark = ",") ), 
                           size = 3.5, show.legend = FALSE) +
  
  scale_y_continuous(
  breaks = seq(32000,44000,2000),  
    
  labels = scales::label_number(big.mark = ".", decimal.mark = ",") ) +
  
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  
  theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8) ) +
  
  labs(x = "Ano", y = "Número de mortes no transporte terrestre")
ggsave(filename ="Figuras/n_acid_terres.bmp",width = 8,height = 5,device='bmp', dpi=150)





#Tabela formato wide da taxa de homicídios por Arma de fogo.
base |> select(ano, def_uf_resd, tx_acid_terres) |>
  
  pivot_wider(names_from = ano, values_from = tx_acid_terres, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de acidentes na UF de residência foi obtido pela soma das seguintes CIDs 10: V01-V89, ou seja, sinistros envolvendo pedestres, ciclistas, motociclitas, ocupantes de triciclo,automóvel, caminhonete,  veículo de transporte pesado, ônibus ou outros veículos terrestres não especificados.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de acidentes no transporte terrestre, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x= _ ,"base/taxa_transp_terres_uf_br.xlsx")


           #Figura Taxa de mortes no transporte terrestre#
base |>
  
  filter(def_uf_resd == "Brasil") |>
  
  mutate(ano = ano |> make_date() ) |>
  #Gráfico
  ggplot() +
  
  geom_line(aes(x = ano, y = tx_acid_terres), linetype = "longdash", 
            lineend = "round", linejoin = "round", linewidth = 0.5) + 
  
  geom_point(aes(x = ano, y = tx_acid_terres) ) +
  
  ggrepel::geom_text_repel(aes(x = ano, y = tx_acid_terres,
                               label = scales::number(tx_acid_terres, big.mark = ".", decimal.mark = ",") ), 
                           size = 3.5, show.legend = FALSE) +
  
  # scale_y_continuous(
  #   breaks = seq(32000,44000,2000),  
  #   
  #   labels = scales::label_number(big.mark = ".", decimal.mark = ",") ) +
  
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  
  theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8) ) +
  
  labs(x = "Ano", y = "Taxa de mortes no transporte terrestre")
ggsave(filename ="Figuras/taxa_acid_terres.bmp",width = 8,height = 5,device='bmp', dpi=150)



#Tabela número e taxa de óbitos no transporte terrestre
base |>
  
  filter(def_uf_resd == "Brasil") |> 
  
  select(ano,acid_terres,tx_acid_terres) |> 
  
  pivot_longer(
    cols = c(acid_terres, tx_acid_terres), 
    names_to = "metrica", 
    values_to = "valor") |>
 #Ano como coluna
  pivot_wider(
    names_from = ano, 
    values_from = valor) |>
  rio::export(x = _, "base/transp_terres.xlsx")
  




# Sinistro de Motocicleta -------------------------------------------------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)

sim_doext |>
  #V20-V29 Motociclista traumatizado em um acidente de transporte
  filter(causa_letra == "V" & causa_num %in% c(20:29) ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "acid_moto") |> 
  #SIM duckdb
  mutate(cod_uf_resd = cod_uf_resd |> as.integer() ) -> acid_moto

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- paste0(dirname(getwd()),"/bases/populacao/Pop_Geral_UFs_PNADc.xlsx")

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
left_join(x = acid_moto, y = pop_pnadc, by = join_by("ano","cod_uf_resd" == "cod_ibge","def_uf_resd" == "uf") ) |>
  #O código da UF não é mais necessário
  select(!c(cod_uf_resd) ) -> base
rm(acid_moto,pop_pnadc)

#Acrescentando total Brasil e criando taxa e proporção
base %>%
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        pop = sum(pop),
                        acid_moto = sum(acid_moto), .by = c(ano) ) ) |> 
  #Taxa de homicídio. Padrão Atlas. A taxa somente com uma casa decimal.
  mutate(tx_acid_moto = format(round((acid_moto/pop)*100000,digits = 1) ) |> as.double() ) -> base


#Tabela formato atlas. Número de sinistros de moto
base |> select(ano,def_uf_resd,acid_moto) |>
  
  pivot_wider(names_from = ano, values_from = acid_moto, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de acidentes na UF de residência foi obtido pela soma das seguintes CIDs 10: V20-V29, ou seja, sinistros envolvendo motociclitas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Número de acidentes de motocicleta, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/n_acid_moto_uf_resd.xlsx")


#Tabela Atlas. Taxa de sinistros de moto
base |> select(ano,def_uf_resd,tx_acid_moto) |>
  
  pivot_wider(names_from = ano, values_from = tx_acid_moto, values_fill = 0) %>%
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
    def_uf_resd = "Fonte: IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua (PNADc) e MS/SVSA/CGIAE - Sistema de Informações sobre Mortalidade - SIM. Elaboração: Diest/Ipea e FBSP. Nota: O número de acidentes na UF de residência foi obtido pela soma das seguintes CIDs 10: V20-V29, ou seja, sinistros envolvendo motociclitas.") |>
  #Título da Tabela
  adorn_title(placement = "top", row_name = "",
              col_name = glue::glue("Taxa de acidentes de motocicleta, por UF {min(year)}–{max(year)}") ) |>
  #Exportando tabela.
  rio::export(x = _ ,"base/tx_acid_moto_uf_resd.xlsx")




# Percentual de óbitos envolvendo acidentes com motocicletas em relação ao acidentes terrestres --------
library(tidyverse)
library(janitor)
#Pasta raiz
here::i_am("Rotinas/Tabelas Padrão - Atlas 2026.R") 
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/sim/RData/sim_doext_14_24.Rdata"))
year <- c(2014:2024)


#Contagem de sinistros no transporte terrestre
sim_doext |>
  filter(
    #V01-V09 Pedestre traumatizado em um acidente de transporte
    (causa_letra == "V" & causa_num %in% c(0:9) ) |
      #V10-V19 Ciclista traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(10:19) ) |
      #V20-V29 Motociclista traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(20:29) ) |
      #V30-V39 Ocupante de triciclo motorizado traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(30:39) ) |
      #V40-V49 Ocupante de um automóvel traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(40:49) ) |
      #V50-V59 Ocupante de uma caminhonete traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(50:59) ) |
      #V60-V69 Ocupante de um veículo de transporte pesado traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(60:69) ) |
      #V70-V79 Ocupante de um ônibus traumatizado em um acidente de transporte
      (causa_letra == "V" & causa_num %in% c(70:79) ) |
      #V80-V89 Outros acidentes de transporte terrestre
      (causa_letra == "V" & causa_num %in% c(80:89) ) ) |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, cod_uf_resd, def_uf_resd, name = "n_acid_terres") |>   
  
  select(ano,def_uf_resd, n_acid_terres) %>%
  
  #Acrescenta Total Brasil
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        n_acid_terres = sum(n_acid_terres), .by = c(ano) ) ) -> acid_terres

#Acidentes de motocicleta  
sim_doext |>
  
  filter(
    
      #V20-V29 Motociclista traumatizado em um acidente de transporte
      causa_letra == "V" & causa_num %in% c(20:29) )   |>
  
  #Poderia adcionar total brasil. Mas precisa adicionar brasil na pop. Vou acrescentar Brasil após juntas as bases.
  count(ano, def_uf_resd, name = "n_acid_moto") %>%
  
  #Acrescenta Total Brasil
  bind_rows(. |>
              summarise(def_uf_resd="Brasil" |> as.factor(),
                        n_acid_moto = sum(n_acid_moto), .by = c(ano) ) ) |>

  #Join com o total de acidentes terrestres
  left_join(x = _, 
            y = acid_terres, by = join_by("ano","def_uf_resd") ) |>
  
  #Proporção de acidentes de moto
  mutate(p_acid_moto = round( (n_acid_moto/n_acid_terres)*100, 1),
         ufs = case_when(def_uf_resd == "Brasil" ~ "Brasil",
                         .default = "Outros") ) |>
    filter(ano == 2024) -> base

#Gráfico da proporção de mortes por acidente de motocicleta no total de acidentes de terrestres.
  filter(ano == 2023) -> base

base |>
  
  ggplot() +
  
  geom_col(aes(x = p_acid_moto, y = fct_reorder(.f = def_uf_resd, .x = p_acid_moto),
               fill = ufs), show.legend = FALSE ) +
  
  geom_text(aes(x = p_acid_moto, y = def_uf_resd, 
                label = paste0(scales::number(p_acid_moto, big.mark = ".", decimal.mark = ","), "%"),
                hjust = -0.3), size = 3 ) +
  
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, decimal.mark = ",", accuracy = 0.1) ) +

  labs(x = "", y = "") + 
  
  theme(axis.text.y = element_text(face = "bold") )

  labs(x = "", y = "")


ggsave(filename ="Figuras/p_acid_moto.bmp",width = 11,height = 8,device='bmp', dpi=150)






ggrepel::geom_text_repel(




# Sinistros em outros meios de transporte ---------------------------------


