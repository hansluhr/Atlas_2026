# Anos Potenciais de vida Perdidos ----------------------------------------
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/homic_preds.RData")
year <- c(2013:2023)

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
  mutate(intencao = recode(intencao,"h_legal" = "Homicídio")) |>
  filter(ano %in% year & idade %in% c(0:69) & intencao == "Homicídio" ) |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao,instrumento) |>
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
ggsave(filename = "APVP.bmp", width = 13,height = 10,dpi=250)
ggsave(filename ="APVP.eps",width = 15,height = 10,device=cairo_ps, dpi=350)



#Tabelas - APVP por idade
sim_doext |> 
  filter(ano %in% year & idade %in% c(15:29) & intencao_homic == "Homicídio") |> droplevels() |>
  #Quero o número de homicídios por idade
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP) - Por idade
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> view()
summarise(apvp = sum(apvp) )



#Tabelas - APVP intencao
sim_doext |> 
  mutate(intencao = recode(intencao,"h_legal" = "Homicídio")) |>
  filter(ano %in% year & idade %in% c(0:69)) |> droplevels() |>
  #Quero o número de homicídios por idade
  group_by(intencao) |>
  count(idade, name = "n") |> 
  #Anos Potenciais de Vida Perdidos (APVP) - Por idade
  mutate(anos_restantes = 70 - (idade + 0.5),
         apvp = anos_restantes * n) |> 
  #Valor total de APVP de jovens
  filter(idade %in% c(15:29) ) |> summarise(apvp = sum(apvp) )

#Tabelas - APVP instrumento
sim_doext |> 
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
         prop = (apvp/total)*100)


