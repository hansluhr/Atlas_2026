library(tidyverse)
library(janitor)

# Importando a base -------------------------------------------------------
load("C:/Users/P224552695/Desktop/r/SIH/sih_13_23.RData")
gc()
#Ano a ser excluido das contagens.
year <- 2012

# Internações por raça_cor ------------------------------------------------
sih |> 
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  tabyl(ano_inter, raca_cor, show_missing_levels = FALSE) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  adorn_title(col_name = "Internações por homicídio - Raça\\Cor") |>
  rio::export(x = _, "sih_raca_cor.xlsx")

# #Homicídio de indígenas, por UF de residência e ano de internação -------
sih |> 
  filter(intencao_homic == "Homicídio" & raca_cor == "Indígena" & ano_inter!= year) |> 
  tabyl(uf_resd, ano_inter, show_missing_levels = FALSE) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  adorn_title(col_name = "Internações por homicídio - UF de Residência") |>
  rio::export(x = _, "sih_raca_uf_resd.xlsx")


# #Homicídio por ano de internação e etnia --------------------------------
sih |>
  filter(intencao_homic == "Homicídio" & raca_cor == "Indígena" & ano_inter!= year) |> 
  tabyl(etnia, ano_inter, show_missing_levels = FALSE) |> adorn_totals(where = c("row","col") ) |>
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  #adorn_title(col_name = "Internações por homicídio - etinia") |>
  #Exclusão de etnia sem internações
  filter(Total > 0) |>
  rio::export(x = _, "sih_etnia.xlsx")




# #Homicídio por ano de internação e UF de residência ---------------------
sih |>
  filter(intencao_homic == "Homicídio" & raca_cor == "Indígena" & ano_inter!= year) |> 
  count(ano_inter,uf_resd,etnia) |> arrange(uf_resd) |> 
  #Pivot. Quero deixar no formato de tabela
  pivot_wider(names_from = "ano_inter", values_from = "n", values_fill = 0) |> 
  #Adiciona somatória de colunas e linhas
  adorn_totals(where = c("col","row") ) |>
  #Exportando
  rio::export(x = _, "sih_uf_resd_ano_etnia.xlsx")


# #Homicídio por ano de internação e Município de residência --------------
sih |>
  filter(intencao_homic == "Homicídio" & raca_cor == "Indígena" & ano_inter!= year) |> 
  count(ano_inter,uf_resd,munic_resd,etnia) |> arrange(uf_resd) |> 
  #Pivot. Quero deixar no formato de tabela
  pivot_wider(names_from = "ano_inter", values_from = "n", values_fill = 0) |> 
  #Adiciona somatória de colunas e linhas
  adorn_totals(where = c("col","row") ) |>
  #Exportando
  rio::export(x = _, "sih_uf_resd_munic_resd_ano_etnia.xlsx")

# Instrumento -------------------------------------------------------------
sih |>
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  tabyl(instrumento,raca_cor) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  adorn_title("combined", row_name = "Instrumento", col_name = "Raça\\cor") |>
  #Exportando
  rio::export(x = _, "sih_instrumento.xlsx")


# Local do Incidente ------------------------------------------------------
sih |>
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  tabyl(local_incd,raca_cor) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  adorn_title("combined", row_name = "Local do Incidente", col_name = "Raça\\cor") |>
  #Exportando
  rio::export(x = _, "sih_local_incidente.xlsx")


# Sexo da Vítima ----------------------------------------------------------
sih |>
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  tabyl(raca_cor,sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front",  format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  adorn_title("combined", row_name = "Sexo", col_name = "Raça\\cor") |>
  #Exportando
  rio::export(x = _, "sih_sexo.xlsx")



# Houve óbito -------------------------------------------------------------
sih |>
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  tabyl(raca_cor,morte) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")


# Acumulado de idade ------------------------------------------------------
#Idade - empirical cumulative distribution - label doo eixo x indicando as medianas das idades.
#Histograma de idade
sih |>
  filter(intencao_homic == "Homicídio" & ano_inter!= year) |> 
  ggplot(aes(x=idade)) +
  stat_ecdf(aes(color = raca_cor), geom = "point",  pad = FALSE,na.rm=TRUE) + geom_hline(yintercept=0.5,linetype = "dotted") +
  labs(x = "", y="",color = "") +
  scale_x_continuous(breaks = seq(0, 110, 5)) + scale_y_continuous(breaks = seq(0, 10, 0.1), labels = scales::percent) + 
  theme(legend.position = c(0.75,0.2),legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_blank(),legend.direction = "horizontal") 
ggsave(filename ="sih_acum_idade.bmp",width = 8,height = 5,device='bmp', dpi=150)


sih %>%
  filter(intencao_homic == "Homicídio" & ano_inter!= year & !is.na(idade)) |> 
  group_by(raca_cor, idade) %>% 
  summarise(freq = n(), .groups = "drop") %>%
  arrange(raca_cor, idade) %>%
  group_by(raca_cor) %>%
  mutate(
    freq_acumulada = cumsum(freq),  # Soma acumulada
    perc_acumulada = round(freq_acumulada / sum(freq) * 100, 2)  # Percentual acumulado
  ) %>%
  ungroup() |>
  rio::export(x = _, "sih_idade_acumulada.xlsx")

