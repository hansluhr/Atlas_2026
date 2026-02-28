library(tidyverse)
library(janitor)

# Nessa rotina vou trabalhar com a base nova. 
#É a base com variáveis individualizadas e nova forma de identificar grupos de interesse.
#Importando a base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_14_23.RData")


# Provável autor da violência doméstica e idade --------------------------------------------------
sinan |> 
  #slice_sample(n = 100) |>
  
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" & !is.na(idade) &
                nome_rel != "Não/Ignorado/Missing" & grupo_viol == "Doméstica") |> droplevels() |> 
  
  mutate(
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                                              "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
    #Colocar primeira letra da string maiúscula
    stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.025, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) )  |>
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE)  |> 
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) + 
  geom_point( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) +
  
  #Apontações de algumas autoria.
  #Mãe
  annotate("text", x = 0, y = 47, label = "Mãe", size = 2.5, alpha = 0.6) +
  #Namorado
  annotate("text", x = 12.5, y = 40, label = "Namorado", size = 2.5, alpha = 0.6, fontface =2) +
  #Pai
  annotate("text", x = 0, y = 15, label = "Pai", size = 2.5, alpha = 0.6, fontface =2) +
  #Padrasto
  annotate("text", x  = 10,  y = 30, label = "Padrasto", size = 2.5, alpha = 0.6, fontface =2) +
  #Ex-Cônjuge
  annotate("text", x  = 33,  y = 29, label = "Ex-Cônjuge", size = 2.5, alpha = 0.6, fontface =2) +
  #Côjuge
  annotate("text", x  = 33,  y = 58, label = "Cônjuge", size = 2.5, alpha = 0.6, fontface =2) +
  #Filho
  annotate("text", x  = 65,  y = 58, label = "Filho(a)", size = 2.5, alpha = 0.6, fontface =2) +
  
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.43, 0.93), legend.text = element_text(size = 10),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "%de provável autor na idade.", color = "", title = "") 
ggsave(filename ="provável_autor_idade.bmp",width = 9,height = 5,device='bmp', dpi=150)
#ggsave(filename ="provável_autor_idade.eps",width = 8,height = 5, dpi=150)


#Tabela idade e provável autor.
sinan |> 
  #slice_sample(n = 100) |>
  
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" & !is.na(idade) &
                  nome_rel != "Não/Ignorado/Missing" & grupo_viol == "Doméstica") |> droplevels() |> 
  
  mutate(
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
      #Colocar primeira letra da string maiúscula
      stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.025, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) )  |>
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE)  |> 
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  
  pivot_wider(names_from = nome_rel, values_from = c(n,p_nome_rel) ) |>
  
  rio::export(x = _, "provável_autor_idade.xlsx")





# Tipo de violência ---------------------------------------
library(tidyverse)
library(janitor)

# Nessa rotina vou trabalhar com a base nova. 
#É a base com variáveis individualizadas e nova forma de identificar grupos de interesse.
#Importando a base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_14_23.RData")


#Proporção do tipo de violência no clico de vida.
sinan |> 
  filter(t_viol %in% c("V.Física","Negligência","V.Sexual","V.Psico","Int. Legal","Tortura",'Múltiplas',"Ao menos uma") &
           cs_sexo == "Mulher" & les_autop!="Sim" & !is.na(idade) ) |> droplevels() |>
  mutate(
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  #Contagem de tipo de violência por idade
  count(idade,t_viol, name = "n", .drop = FALSE) |>
  mutate(
    #Proporção da violência na idade
    p_t_viol = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |> 
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_t_viol, color = t_viol ) ) + geom_point( aes(x = idade, y = p_t_viol, color = t_viol ) ) +
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.4, 0.93), legend.text = element_text(size = 10),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "%de violência na Idade", color = "", title = "")  
ggsave(filename ="t_viol_idade.bmp",width = 9,height = 5,device='bmp', dpi=150)
#ggsave(filename ="provável_autor_idade.eps",width = 8,height = 5, dpi=150)


#Tabela proporção da violência
sinan |> 
  filter(t_viol %in% c("V.Física","Negligência","V.Sexual","V.Psico","Int. Legal","Tortura",'Múltiplas',"Ao menos uma") &
           cs_sexo == "Mulher" & les_autop!="Sim" & !is.na(idade) ) |> droplevels() |>
  mutate(
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  #Contagem de tipo de violência por idade
  count(idade,t_viol, name = "n", .drop = FALSE) |>
  mutate(
    #Proporção da violência na idade
    p_t_viol = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |> 
  
  pivot_wider(names_from = t_viol, values_from = c(n,p_t_viol) ) |>
  
  rio::export(x = _, "tipo_viol.xlsx")


# Tipo de violência individualizada ---------------------------------------
sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" &
                 !is.na(idade) & grupo_viol == "Doméstica") |> droplevels() |>
  
  mutate(
    #Retirar viol_ do nome individualizado da violência
    nome_viol = nome_viol |> 
      str_replace_all( c("(?i)viol_" = "", "fisic" = "Física", "sexu" = "Sexual" ) ) |> 
      #Primeira letra de cada violência em maiusculo
      str_to_title(),
 
     #Transforma contagens pouco frequentes na categoria outros   
    nome_viol = nome_viol |> fct_lump(prop = 0.005, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  
  #Contagem de tipo de violência por idade
  count(idade,nome_viol, name = "n", .drop = FALSE) |>
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_nome_rel, color = nome_viol ) ) + geom_point( aes(x = idade, y = p_nome_rel, color = nome_viol ) ) +
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 3) ) +
  theme(legend.position.inside = c(0.5, 0.90), legend.text = element_text(size = 11),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9), legend.title=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "% de violência na Idade", color = "",title = "")
ggsave(filename ="t_viol_ind_idade.bmp",width = 10,height = 5,device='bmp', dpi=150)




#Tabela tipo de violência individualizada
sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" &
                  !is.na(idade) & grupo_viol == "Doméstica") |> droplevels() |>
  
  mutate(
    #Retirar viol_ do nome individualizado da violência
    nome_viol = nome_viol |> 
      str_replace_all( c("(?i)viol_" = "", "fisic" = "Física", "sexu" = "Sexual" ) ) |> 
      #Primeira letra de cada violência em maiusculo
      str_to_title(),

    #Transforma contagens pouco frequentes na categoria outros   
    nome_viol = nome_viol |> fct_lump(prop = 0.005, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  
  #Contagem de tipo de violência por idade
  count(idade,nome_viol, name = "n", .drop = FALSE) |>
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  
  pivot_wider(names_from = nome_viol, values_from = c(n, p_nome_rel) ) |> 
  
  rio::export(x=_, "tipo_viol_indv.xlsx")




# Tipo de violência individualizada e provável autor ----------------------

sinan |> 
  
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" &
                  !is.na(idade) & grupo_viol == "Doméstica") |> droplevels() |>
  
  mutate(
    #Retirar viol_ do nome individualizado da violência
    nome_viol = nome_viol |> 
      str_replace_all( c("(?i)viol_" = "", "fisic" = "Física", "sexu" = "Sexual" ) ) |> 
      #Primeira letra de cada violência em maiusculo
      str_to_title(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_viol = nome_viol |> fct_lump(prop = 0.005, other_level = "Outros"), 
    
    
    #Substituição em nomes do provável autor.
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
      #Colocar primeira letra da string maiúscula
      stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.025, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) )  |>
    
  tabyl(nome_viol, nome_rel) |> 
  
  adorn_percentages(denominator = "col") |>
  adorn_totals(where = "row") |> adorn_pct_formatting() |>
  rio::export(x = _, "viol_ind_prov_autor2.xlsx")
