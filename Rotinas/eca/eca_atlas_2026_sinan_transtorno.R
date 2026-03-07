# Tabelas violência contra crianças e jovens ---------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
#últimos dez anos
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year) |>
  mutate(fxet_eca = as.factor(case_when(
    nu_idade_n<=4004 ~ "0 a 4",
    nu_idade_n>4004 & nu_idade_n<=4014 ~ "5 a 14",
    nu_idade_n>4014 & nu_idade_n<=4019 ~ "15 a 19",
    TRUE ~ NA_character_) ) ) |>
  tabyl(fxet_eca, show_na = F) |> adorn_totals(where = c("row") ) |>
  adorn_percentages("col") %>% adorn_pct_formatting(digits = 1)








#Tabela 4.5 Brasil: Distribuição da violência contra crianças e adolescentes por local da violência e faixa etária da vítima (2011-2021)*
#Criando  Faixa crianças e jovens
sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year) |>
  mutate(fxet_eca = as.factor(case_when(
  nu_idade_n<=4004 ~ "0 a 4",
  nu_idade_n>4004 & nu_idade_n<=4014 ~ "5 a 14",
  nu_idade_n>4014 & nu_idade_n<=4019 ~ "15 a 19",
  TRUE ~ NA_character_)),
  #Ordem de local da violência na tabela.
  local_ocor = local_ocor |> fct_relevel(c("Residência","Via pública",
                                           "Ignorado", "Outro",
                                           "Bar ou similar", "Escola",
                                           "Comércio/Serviços", "Habitação coletiva",
                                           "Local de prática esportiva","Indústrias/Construção") ) ) |>
  tabyl(local_ocor,fxet_eca,show_na = F) |> adorn_totals(where = c("row")) |>
  adorn_percentages("col") %>% adorn_pct_formatting(digits = 1) |>
  #Ordem das colunas na tabela.
  select("Local da Violência" = local_ocor,"0 a 4","5 a 14","15 a 19") |>
  rio::export(x=_,"eca_local_fxet.xlsx")


#Tabela 4.12 Brasil: Distribuição da violência contra crianças e adolescentes por autor da violência
#Grupo autor da violência
sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year ) |>
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras")),
    grupo_viol = grupo_viol |> fct_relevel("Doméstica", "Comunitária",
                                           "Institucional","Misto") ) |>
  tabyl(grupo_viol,fxet_eca,show_na = T, show_missing_levels = T) |>
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\,", "."))) |>
  #Ordem das colunas na tabela.
  select("Autoria" = grupo_viol,"0 a 4","5 a 14","15 a 19") |>
  rio::export(x=_,"eca_grupo_viol_fxet.xlsx")

#Número de violência física contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Física" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
   #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_fisc_fxet.xlsx")


#Número de violência sexual contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Sexual" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_sex_fxet.xlsx")


#Número de violência psicológica contra crianças e adolescentes
sinan |>
  filter(t_viol == "V.Psico" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_psic_fxet.xlsx")


#Número de negligência/abandono de crianças e adolescentes por faixa etária
sinan |>
  filter(t_viol == "Negligência" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_negli_fxet.xlsx")



#Gráfico com quadros das quatro violências
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras") ) |> fct_relevel("0 a 4","5 a 14","15 a 19"),
    t_viol = fct_recode(t_viol,"V. Psicológica" = "V.Psico") ) |>
  filter(t_viol %in% c("Negligência","V.Física","V. Psicológica","V.Sexual") & fxet_eca != "Outras" &
           grupo_viol!="Autoprovocada" & ano_not %in% year ) |> droplevels() |>
  #contagem de notificações
  count(ano_not,fxet_eca,t_viol) |>
  ggplot(aes(x = ano_not, y = n, color = fxet_eca, group = fxet_eca) ) +
  geom_line() + geom_point() +

  ggrepel::geom_text_repel(aes(x= ano_not, y = n, label = scales::comma(n, big.mark = "."), color = fxet_eca), size = 3.15, show.legend = FALSE ) +

  facet_wrap(vars(t_viol), scales = "free") +

  guides(color = guide_legend(position = "inside",title.position = "top") ) +
  theme(strip.text = element_text(size=10, face="bold"),legend.title = element_text(face="bold"),
        legend.text = element_text(face = "bold",size = 10),
        legend.position.inside = c(0.1, 0.4),legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x="",y="",color = "Faixa Etária")
ggsave(filename = "V. não letal_eca.bmp", width = 15,height = 10,dpi=250)
ggsave(filename = "V. não letal_eca.eps", width = 15,height = 10,dpi=350)


#tabela de proporção por sexo.
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  filter(grupo_viol!="Autoprovocada" & fxet_eca != "Outras" & ano_not %in% year &
           t_viol %in% c("Negligência","V.Física","V.Psico","V.Sexual") ) |> droplevels() |>
  tabyl(t_viol,cs_sexo) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")



#tabela de proporção faixa etária
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  filter(grupo_viol!="Autoprovocada" & fxet_eca != "Outras" & ano_not %in% year &
           t_viol %in% c("Negligência","V.Física","V.Psico","V.Sexual") ) |> droplevels() |>
  tabyl(fxet_eca,t_viol) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")