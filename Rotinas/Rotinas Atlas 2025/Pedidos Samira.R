library(tidyverse)
library(janitor)
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
year <- 2013:2023


#Número de violência física contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Física" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_fisc_fxet.xlsx")


#Número de violência sexual contra crianças e adolescentes – Brasil 
sinan |>
  filter(t_viol == "V.Sexual" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_sex_fxet.xlsx")


#Número de violência psicológica contra crianças e adolescentes
sinan |>
  filter(t_viol == "V.Psico" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_psic_fxet.xlsx")


#Número de negligência/abandono de crianças e adolescentes por faixa etária
sinan |>
  filter(t_viol == "Negligência" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"),fxet_eca)) |> rio::export(x=_,"eca_viol_negli_fxet.xlsx")



#Gráfico com quadros das quatro violências
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras") ) |> fct_relevel("0 a 4", "5 a 9", "10 a 14", "15 a 19"),
    t_viol = fct_recode(t_viol,"V. Psicológica" = "V.Psico") ) |>
  filter(t_viol %in% c("Negligência","V.Física","V. Psicológica","V.Sexual") & fxet_eca != "Outras" &
           grupo_viol!="Autoprovocada" & ano_not %in% year ) |> droplevels() |>
  #contagem de notificações
  count(ano_not,fxet_eca,t_viol) |> 
  ggplot(aes(x = ano_not, y = n, color = fxet_eca, group = fxet_eca) ) +
  geom_line() + geom_point() +
  
  ggrepel::geom_text_repel(aes(x= ano_not, y = n, 
                           label = scales::comma(n, big.mark = "."), color = fxet_eca), 
                           size = 3, show.legend = FALSE, force = 2 ) + 
  
  facet_wrap(vars(t_viol), scales = "free") +
  
  guides(color = guide_legend(position = "inside",title.position = "top") ) +
  theme(strip.text = element_text(size=10, face="bold"),legend.title = element_text(face="bold"),
        legend.text = element_text(face = "bold",size = 10),
        legend.position.inside = c(0.12, 0.4),legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  labs(x="",y="",color = "Faixa Etária")
ggsave(filename = "V. não letal_eca.bmp", width = 15,height = 10,dpi=180)
ggsave(filename = "V. não letal_eca.eps", width = 20,height = 10,dpi=350)
