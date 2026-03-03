# Histograma de idade por instrumento -------------------------------------
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/homic_preds.RData")
year <- c(2012:2022)

#Todas as idades
sim_doext |> filter(!is.na(idade) & idade <= 90 & ano %in% year & intencao_homic == "Homicídio" & 
                      sexo != "Ignorado") |> droplevels() |> 
  ggplot() +
  geom_histogram(aes(x=idade,fill = instrumento), binwidth  = 3,color = "white") +
  #Idade mediana por sexo
  geom_vline(data = sim_doext |> 
               filter(!is.na(idade) & ano %in% year & intencao_homic == "Homicídio" & sexo != "Ignorado") |> 
              summarise(idade = median(idade), .by = c(sexo) ),
             aes(xintercept = idade), color = "red",linewidth = 1, linetype="dashed")  +
  facet_wrap(vars (sexo), scales = "free") +
  #Ajuste eixo x
  ggh4x::facetted_pos_scales(x = list(
    sexo == "Homem" ~ scale_x_continuous(breaks = seq(0, 90, 3)),
    sexo == "Mulher" ~ scale_x_continuous(breaks = seq(0, 90, 3) )  ) ) +
  #Ajuste eixo y
  ggh4x::facetted_pos_scales(y = list(
    sexo == "Homem" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                         breaks = seq(0, 75000, 5000) ),
    sexo == "Mulher" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                          breaks = seq(0, 4000, 500) ) ) ) +
  
  labs(x="",y = "Frequência (Em Mil)",fill = "") + 
  theme(legend.position = "bottom",legend.text=element_text(size=13),axis.text.x = element_text(angle = 45),
        axis.title.y = element_text(size = 10, face = "bold"),
        strip.text = element_text(size=12, face="bold")) + guides(fill = guide_legend(nrow = 1) )
ggsave(filename ="hist_idade_sexo_instrumento.bmp",width = 13,height = 8,device='bmp', dpi=200)


#Importando a base
load("C:/Users/gabli/Desktop/r/homic_preds.RData")
year <- c(2012:2022)

#Somente Jovens
sim_doext |> filter(idade %in% c(15:29) & ano %in% year & intencao_homic == "Homicídio" & 
                      sexo != "Ignorado") |> droplevels() |>
  ggplot() +
  geom_histogram(aes(x=idade,fill = instrumento), binwidth  = 1,color = "white") +
  #Idade mediana por sexo
  geom_vline(data = sim_doext |> 
               filter(!is.na(idade) & ano %in% year & intencao_homic == "Homicídio" & sexo != "Ignorado") |> 
               summarise(idade = median(idade), .by = c(sexo) ),
             aes(xintercept = mean(idade)), color = "red",linewidth = 1, linetype="dashed")  +
  scale_x_continuous(breaks = seq(15,29,1) ) + 
  facet_wrap(vars (sexo), scales = "free") +
  #Ajuste eixo y
  ggh4x::facetted_pos_scales(y = list(
    sexo == "Homem" ~ scale_y_continuous(breaks = seq(0, 25000, 5000) ),
    sexo == "Mulher" ~ scale_y_continuous(breaks = seq(0, 1750, 250) ) ) ) +
  labs(x="",y = "Frequência",fill = "") + 
  theme(legend.position = "bottom",legend.text=element_text(size=13),
        strip.text = element_text(size=10, face="bold")) + guides(fill = guide_legend(nrow = 1) )
ggsave(filename ="hist_idade_sexo_intencao_instrumento.bmp",width = 13,height = 8,device='bmp', dpi=200)

#Mediana das idades
data = sim_doext |> 
  filter(!is.na(idade) & ano %in% year & intencao_homic == "Homicídio" & sexo != "Ignorado") |> 
  summarise(idade = median(idade), .by = c(sexo) )


# Tabela instrumento do óbito ---------------------------------------------
sim_doext |>  
  #mutate(intencao = recode(intencao,"h_legal" = "Homicídio")) |> droplevels() |>
  filter(idade %in% c(15:29) & ano %in% year & intencao != "h_legal") |>
  tabyl(instrumento,intencao) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") %>%  knitr::kable(format = "simple")


# Anos Potenciais de vida Perdidos ----------------------------------------
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/gabli/Desktop/r/homic_preds.RData")
year <- c(2012:2022)

#Método Romeder e McWhinnie.
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
  geom_col(aes(x=idade,y=apvp, fill = intencao), show.legend = FALSE) +
  facet_wrap(vars(intencao), scales = "free") + 
  scale_x_continuous(breaks = seq(1,69,3) ) + 
  #scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3) ) +
  #Ajuste eixo y
  ggh4x::facetted_pos_scales(y = list(
    intencao == "Homicídio" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                                 breaks = seq(0, 1250000, 250000) ), 
     intencao == "Acidente" ~ scale_y_continuous(labels = scales::label_number(scale = 1 / 1e3),
                                                 breaks = seq(0,600000,100000) )
                                                 )  )  +
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



# Raça\cor ----------------------------------------------------------------
library(tidyverse)
library(janitor)

#Importando a base
load("C:/Users/b224552695/Desktop/r/homic_preds.RData")
year <- c(2012:2022)

sim_doext |> #slice_sample(n=1000) |>
  filter(ano %in% year & idade %in% c(15:29) & intencao_homic == "Homicídio") |> droplevels() |>
  mutate(racacor = case_match(racacor, c("Parda", "Preta") ~ "Negro", .default = racacor ) |> as_factor() ) |> 
  ggplot() +
  geom_bar(aes(idade, fill  = racacor ),position = "dodge")
  geom_freqpoly(aes(x = idade,
                     color = racacor), binwidth = 1 ) 
  geom_histogram(aes(x = idade, fill = racacor),binwidth = 1,
                    alpha = 0.5,
                    position = "fill") +
  #facet_wrap(vars(racacor), scales = "free")


#Raça\cor, por escolaridade    
sim_doext |> #slice_sample(n=10000) |>
    filter(ano %in% year & idade %in% c(15:29) & intencao_homic == "Homicídio") |> droplevels() |>
    mutate(racacor = case_match(racacor, c("Parda", "Preta") ~ "Negro", .default = racacor) |> as_factor(),
          esc = fct_relevel(esc,"Ignorado","Nenhuma"," 1 a 3 anos","4 a 7 anos","8 a 11 anos") ) |>     
    tabyl(racacor,esc)  %>% adorn_totals(where = c("row","col")) %>%
    adorn_percentages(denominator = c("row") ) %>% adorn_pct_formatting(digits = 1) %>%
    adorn_ns(position = "front") %>%  knitr::kable(format = "simple")

    
    
    
# Homicídio Oculto Jovens -------------------------------------------------
#Importando bases de jovens
readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2024/base/Jovem_eca/taxa_homicidio_jovem_uf_br.xlsx",skip = 1) |>
    select(uf,tx_homic = "2022") |>
    left_join(x=_,y = (
      readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2024/base/Jovem_eca/taxa_homicidio_jovem_projetado_uf_br.xlsx",skip = 1) |>
  select(uf = uf_resd,tx_proj = "2022") ), by = join_by(uf) ) |>
  mutate(uf = uf |> as_factor(),
         across( c(tx_homic,tx_proj), ~ parse_number(.,locale = locale(decimal_mark = ",") ) ),  
         tx_ocult = round(tx_proj - tx_homic,1) ) |>
    pivot_longer(cols = !c(uf,tx_proj), names_to = "variable", values_to = "value") |>
    mutate(uf = fct_reorder(uf,tx_proj) ) |> 
    ggplot() +
    #geom_col(aes(x = value, y = uf, fill = variable),position = position_stack(reverse = T) ) +
    geom_col(aes(x = value, y = uf, fill = variable),position = position_stack(reverse = T) ) +
    geom_text(aes(x = value, y = uf, label = tx_proj) ) 
    scale_fill_manual(values = c("#6897bb","#ff6666"), 
                      labels = c("Tx. H. Registrada","Tx. H. Projetada") ) +
    guides(fill = guide_legend(position = "inside") ) + labs(fill="",x = "", y = "") + 
    theme(legend.position.inside = c(0.6,0.2),legend.text = element_text(size=10),
          legend.direction = c("horizontal"),axis.text = element_text(size = 10),
          legend.background = element_rect(fill = "transparent", colour = NA) )
  
#Importando bases de jovens
    readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2024/base/Jovem_eca/taxa_homicidio_jovem_uf_br.xlsx",skip = 1) |>
      select(uf,tx_homic = "2022") |>
      left_join(x=_,y = (
        readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas 2024/base/Jovem_eca/taxa_homicidio_jovem_projetado_uf_br.xlsx",skip = 1) |>
          select(uf = uf_resd,tx_proj = "2022") ), by = join_by(uf) ) |>
      mutate(uf = uf |> as_factor(),
             across( c(tx_homic,tx_proj), ~ parse_number(.,locale = locale(decimal_mark = ",") ) ),
             uf = fct_reorder(uf,tx_proj) ) |>
      pivot_longer(cols = !c(uf), names_to = "variable", values_to = "value") |>
      #mutate(uf = fct_reorder(uf,tx_proj) ) |> 
      ggplot() +
      #geom_col(aes(x = value, y = uf, fill = variable),position = position_stack(reverse = T) ) +
      geom_col(aes(x = value, y = uf, fill = variable),position = "dodge") +
      geom_text(aes(x = value, y = uf, label = value) )
    scale_fill_manual(values = c("#6897bb","#ff6666"), 
                      labels = c("Tx. H. Registrada","Tx. H. Projetada") ) +
      guides(fill = guide_legend(position = "inside") ) + labs(fill="",x = "", y = "") + 
      theme(legend.position.inside = c(0.6,0.2),legend.text = element_text(size=10),
            legend.direction = c("horizontal"),axis.text = element_text(size = 10),
            legend.background = element_rect(fill = "transparent", colour = NA) )