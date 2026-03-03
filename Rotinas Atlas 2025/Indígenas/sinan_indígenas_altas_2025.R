library(tidyverse)
library(janitor)

#Importando a base do Sinan
load("C:/Users/gabli/Desktop/r/Sinan/sinan_14_23.RData")
year <- c(2013:2023)
age <- c(15:59)


#Tabela de grupo de violência contra indígenas.
sinan |> 
  filter(les_autop!="Sim" & cs_raca == "Indígena" & ano_not %in% year) |>
  tabyl(grupo_viol,cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".",decimal.mark=",") } ) |>
  #Exportando
  rio::export(x = _, "sinan_grupo_viol_indg.xlsx")
#Das Violências domésticas, 77,8% são mulheres.

#Tabela de grupo de violência contra indígenas.
sinan |> filter(les_autop!="Sim" & cs_raca == "Indígena" & ano_not %in% year) |>
  tabyl(grupo_viol,cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front", format_func = function(x) format(x, big.mark = ".", decimal.mark=",") ) |>
  #Exportando
  rio::export(x = _, "sinan_grupo_viol_indg2.xlsx")
#Das mulheres indígenas, 58,2% são agredidas por familiares.



# Provável autor do grupo doméstico  --------------------------------------
sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & cs_raca == "Indígena" & les_autop!="Sim" & ano_not %in% year & !is.na(idade) &
                nome_rel != "Não/Ignorado/Missing" & grupo_viol == "Doméstica") |> droplevels() |>
  mutate(
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.003, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE) |>
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) + geom_point( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) +
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside", nrow = 4) ) +
  theme(legend.position.inside = c(0.2, 0.93),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size=9),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="", y = "%do provável autor na Idade", color = "", title = "Provável autor de violência doméstica") 
ggsave(filename ="sinan_autor_doméstic_indg.bmp",width = 13,height = 8,device='bmp', dpi=200)


sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & cs_raca == "Indígena" & les_autop!="Sim" & ano_not %in% year & !is.na(idade) &
                  nome_rel != "Não/Ignorado/Missing" & grupo_viol == "Doméstica") |> droplevels() |>
  mutate(
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.003, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE) |>
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  #Mantém somente a proporção
  select(!c(n)) |> 
  #Pivot para transformar em tabelão
  pivot_wider(names_from = nome_rel, values_from = p_nome_rel ) |> 
  #Exportando
  rio::export(x = _, "sinan_autor_doméstic_indg.xlsx")



# Tipo de violência -------------------------------------------------------
sinan |> 
  filter(les_autop!="Sim" & cs_raca == "Indígena" & ano_not %in% year & grupo_viol == "Doméstica") |> 
  tabyl(t_viol, cs_sexo) |> adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  #Exportando
  rio::export(x = _, "sinan_t_viol.xlsx")
  


#Tabela da proporção de tipo de violência  
sinan |> 
  filter(t_viol %in% c("V.Física","Negligência","V.Sexual","V.Psico","Int. Legal","Tortura",'Múltiplas',"Ao menos uma") &
           cs_raca == "Indígena" &    
           cs_sexo == "Mulher" & les_autop!="Sim" & ano_not %in% year & !is.na(idade) ) |> droplevels() |>
  mutate(
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  #Contagem de tipo de violência por idade
  count(idade,t_viol, name = "n", .drop = FALSE) |>
  mutate(
    #Proporção da violência na idade
    p_t_viol = round( ( n/sum(n) )*100,1) , .by = c(idade) )  |>
  #Mantém variáveis de interesse
  select(!c(n)) |>
  #Pivot para tabelão
  pivot_wider(names_from = t_viol, values_from = p_t_viol) |>
  #Exportando
  rio::export(x=_, "sinan_t_viol_idade.xlsx")

#Proporção do tipo de violência no clico de vida.
sinan |> 
  filter(t_viol %in% c("V.Física","Negligência","V.Sexual","V.Psico","Int. Legal","Tortura",'Múltiplas',"Ao menos uma") &
           cs_raca == "Indígena" &    
           cs_sexo == "Mulher" & les_autop!="Sim" & ano_not %in% year & !is.na(idade) ) |> droplevels() |>
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
  guides(color = guide_legend(position = "inside",nrow = 3) ) +
  theme(legend.position.inside = c(0.4, 0.93),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size=9),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "%de violência na Idade", color = "", title = "Tipo de violência") 
ggsave(filename ="sinan_t_viol_idade__indg.bmp",width = 13,height = 8,device='bmp', dpi=200)

# Tipo de Violência individualizada ---------------------------------------
sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & 
                  cs_raca == "Indígena" &
                  les_autop!="Sim" & ano_not %in% year & !is.na(idade) & grupo_viol == "Doméstica") |> droplevels() |>
  
  mutate(
    #Transforma contagens pouco frequentes na categoria outros   
    nome_viol = nome_viol |> fct_lump(prop = 0.005, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  
  #Contagem de tipo de violência por idade
  count(idade,nome_viol, name = "n", .drop = FALSE) |>
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |> 
  #Mantém variáveis de interesse
  select(!c(n)) |>
  #Tabela 
  pivot_wider(names_from = nome_viol, values_from = p_nome_rel) |>
  #Exportando
  rio::export(x = _, "sinan_t_viol_individualizada_indg.xlsx" )

#Gráfico com a proporção de violência individualizada
sinan |> 
  dplyr::filter(cs_sexo == "Mulher" & 
                  cs_raca == "Indígena" &
                  les_autop!="Sim" & ano_not %in% year & !is.na(idade) & grupo_viol == "Doméstica") |> droplevels() |>
    mutate(
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
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,85,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 5) ) +
  theme(legend.position.inside = c(0.5, 0.90),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size = 9), legend.title=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "% de violência na Idade", color = "",title = "Violência individualizada") 
ggsave(filename ="sinan_t_viol_individualizada_indg.bmp",width = 13,height = 8,device='bmp', dpi=200)




# Características da vítima -----------------------------------------------
library(gtsummary)
library(kableExtra)

theme_gtsummary_language(language  = "pt", big.mark = ".",decimal.mark=",")
tabs <- list( 

# Tabela com perfil das vítimas de violência doméstica
t1 <- sinan |> 
  
  #Filtro de interesse.
  dplyr::filter(cs_sexo == "Mulher" & les_autop!="Sim" & ano_not %in% year & 
                 cs_raca == "Indígena" & grupo_viol == "Doméstica") |> droplevels() |>
  
  #Reduzindo levels de provável autor.  
  mutate(nome_rel = nome_rel |> fct_lump(prop = 0.005, other_level = "Outros"),
         ocupa = ocupa |> fct_lump(prop = 0.001, other_level = "Outros") ) |>
  
  select(out_vezes, idade, cs_escol_n, cs_raca, sit_conjug, nome_rel, autor_alco, viol_motiv, cs_gestant, orient_sex, ident_gen) |>
  
  tbl_summary(
    by = out_vezes,
    # don't list missing data separately
    missing = "no",
    #Alterando o label das variáveis 
    label = list(idade ~ "Idade", cs_escol_n ~ "Escolaridade", cs_raca ~ "Raça\\Cor",
                 sit_conjug = "Situação Conjugal", nome_rel ~ "Provável Autor", 
                 autor_alco ~ "Provável autor com suspeita de álcool", viol_motiv ~ "Motivação da Violência", 
                 cs_gestant ~ "Idade Gestacional",  orient_sex ~ "Orientação Sexual", ident_gen ~ "Identidade de Gênero"),
    
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
  
    add_overall() |>
  
  #Alterando label das estatísticas de idade.
  add_stat_label(label = idade ~ c("N(%)", "N faltante (%)", "Média (SD)", "Mediana (IQR)", "Min - Max")) |>
  
  #Alterando header da tabela
  modify_spanning_header(c("stat_1","stat_2","stat_3","stat_4") ~ "**Informar se a violência é de repetição**") %>%
  #
  modify_header(label = "**Características**",   # update the column header
                #Alterando as estatísticas dos totais apresentados no topo da tabela.
                all_stat_cols() ~ "**{level}** N = {style_number(n, big.mark = '.', decimal.mark = ',')} ({style_percent(p,digits = 1)}%)") ) 
## Output to excel using openxlsx package
lapply(tabs, as_tibble) |> 
openxlsx::write.xlsx(file = "caract_vitma.xlsx")
rm(t1,tabs)





  

