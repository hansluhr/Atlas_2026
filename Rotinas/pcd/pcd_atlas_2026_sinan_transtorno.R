# 
# # Gráfico PCds ------------------------------------------------------------
# library(tidyverse)
# library(janitor)
# 
# here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
# #Carrgando base SINAN Violência
# load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
# year <- as.integer(format(Sys.Date(), "%Y")) - 2;gc()
# 
# #Notificações deficiência auditiva
# sinan %>% filter(def_auditi == "Sim" & grupo_viol!="Autoprovocada") %>%
#   count(ano_not, name = "Auditiva") |> 
#   
#   left_join(x = _, y = 
#   
# 
# #Notificações deficiência Física
# sinan %>% filter(def_fisica == "Sim" & grupo_viol!="Autoprovocada") %>%
#   count(ano_not, name = "Física"), by = join_by("ano_not") ) |>
# 
#   left_join(x = _, y = 
#   
# #Notificações deficiência Intelectual
# sinan %>% filter(def_mental == "Sim" & grupo_viol!="Autoprovocada") %>%
#   count(ano_not, name = "Intelectual"), by = c("ano_not") ) |>
#   
#   left_join(x = _, y = 
# 
# #Notificações deficiência Visual
# sinan %>% filter(def_visual == "Sim" & grupo_viol!="Autoprovocada") %>%
#   count(ano_not, name = "Visual"), by = join_by("ano_not") ) |>
#   
#   pivot_longer(cols = !c(ano_not), names_to = "def", values_to = "value", names_transform = list(def = as_factor)) -> base
#   
# 
# base |>
#   #Gráfico
#   ggplot() + 
#   geom_line(aes(x = ano_not, y = value, color = def, group = def) ) +
#   
#   geom_point(aes(x = ano_not, y = value, color = def) ) +
#   
#   ggrepel::geom_text_repel(aes(x = ano_not, y = value, label = value, 
#                                color = def ), seed = 747,
#                            show.legend = FALSE, force = 10 ) +
#   
#   scale_y_continuous(breaks = seq(1000,6000,1000) ) +
#   
#   guides(color = guide_legend(position = "inside", nrow = 2)) +
#   theme(legend.position.inside = c(0.2,0.8),
#         axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
#         axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
#         legend.text = element_text(size = 10,face="bold"),
#         legend.key = element_rect(fill = "transparent", colour = NA),
#         legend.background = element_rect(fill = "transparent", colour = NA) ) +
#   labs(x = "", y = "", color = "")
# 
# ggsave(filename ="base/pcd/figuras/evolucao_defs.bmp",width = 8,height = 5,device='bmp', dpi=150)
# ggsave(filename ="base/pcd/figuras/evolucao_defs.eps",width = 8,height = 5,device='bmp', dpi=150)
# 
# 
# 
# 
# # Tabelas PcD -------------------------------------------------
# library(tidyverse)
# library(janitor)
# 
# here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
# #Carrgando base SINAN Violência
# load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
# year <- 2024;gc()
# 
# #Cobertura municipal das pessoas com deficiência - Municípios e CNES com ao menos uma notificação
# sinan %>% filter(!is.na(id_municip)|!is.na(id_unidade)) %>%
#   group_by(ano_not) %>% summarise(Municípios = n_distinct(id_municip),
#                                   CNES = n_distinct(id_unidade)) 
# 
# #Notificações pessoas com deficiência	
# sinan %>% filter(def == "Sim" & grupo_viol!="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #Notificações deficiência auditiva
# sinan %>% filter(def_auditi == "Sim" & grupo_viol!="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #Notificações deficiência Física
# sinan %>% filter(def_fisica == "Sim" & grupo_viol!="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# 
# #Notificações deficiência Intelectual
# sinan %>% filter(def_mental == "Sim" & grupo_viol!="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# 
# #Notificações deficiência Visual
# sinan %>% filter(def_visual == "Sim" & grupo_viol!="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #TipodefxAgr-8.1
# #tipo de deficiência por grupo agressor - GERAL
# sinan %>% filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = "col") %>%
#   arrange(desc(Total)) %>% #Ordem das linhas
#   adorn_totals(where = "row") %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 2) %>%
#   adorn_ns(position = "front",
#            ns = tabyl(filter(sinan,ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada"),
#                       tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
#              adorn_totals(where = "col") %>%
#              arrange(desc(Total)) %>%
#              adorn_totals(where = "row")) %>% 
#   #Como Fazer isso melhor?
#   relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
#   #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
#   knitr::kable(format = "pipe")
# 
# #tipo de deficiência por grupo agressor - MULHER  
# sinan %>% filter(ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = "col") %>%
#   arrange(desc(Total)) %>% #Ordem das linhas
#   adorn_totals(where = "row") %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 2) %>%
#   adorn_ns(position = "front",
#            ns = tabyl(filter(sinan,ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada"),
#                       tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
#              adorn_totals(where = "col") %>%
#              arrange(desc(Total)) %>%
#              adorn_totals(where = "row")) %>% 
#   #Como Fazer isso melhor?
#   relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
#   #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
#   knitr::kable(format = "pipe",caption = "Tipo de deficiência por grupo agressor - MULHER")
# 
# #tipo de deficiência por grupo agressor - Homem  
# sinan %>% filter(ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = "col") %>%
#   arrange(desc(Total)) %>% #Ordem das linhas
#   adorn_totals(where = "row") %>%
#   adorn_percentages("row") %>%
#   adorn_pct_formatting(digits = 2) %>%
#   adorn_ns(position = "front",
#            ns = tabyl(filter(sinan,ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada"),
#                       tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
#              adorn_totals(where = "col") %>%
#              arrange(desc(Total)) %>%
#              adorn_totals(where = "row")) %>% 
#   #Como Fazer isso melhor?
#   relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
#   #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
#   knitr::kable(format = "pipe",caption = "Tipo de deficiência por grupo agressor - Homem")
# 
# 
# 
# ###Grupo agressor por faixa etária -  Agrxfxet80-8.3
# #Criando  Faixa etária 80+
# sinan %>% mutate(fxet80 = as.factor(case_when(
#   nu_idade_n<=4009 ~ "0 a 9",
#   nu_idade_n>=4010 & nu_idade_n<=4019 ~ "10 a 19",
#   nu_idade_n>=4020 & nu_idade_n<=4029 ~ "20 a 29",
#   nu_idade_n>=4030 & nu_idade_n<=4039 ~ "30 a 39",
#   nu_idade_n>=4040 & nu_idade_n<=4049 ~ "40 a 49",
#   nu_idade_n>=4050 & nu_idade_n<=4059 ~ "50 a 59",
#   nu_idade_n>=4060 & nu_idade_n<=4069 ~ "60 a 69",
#   nu_idade_n>=4070 & nu_idade_n<=4079 ~ "70 a 79",
#   nu_idade_n>=4080 & !is.na(nu_idade_n) ~    "80 ou mais"))) -> sinan
# 
# #Grupo Agresso x Faixa etária 80 - GERAL
# sinan %>% filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = c("col","row")) %>%
#   adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") |>
#   rio::export(x= _ ,"agressor.xlsx" )
#   #knitr::kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - GERAL") 
# 
# #Grupo Agresso x Faixa etária 80 - Mulher
# sinan %>% filter(ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = c("col","row")) %>%
#   adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") %>% 
#   knitr::kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - Mulher")
# 
# #Grupo Agresso x Faixa etária 80 - Homem
# sinan %>% filter(ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
#   tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
#   adorn_totals(where = c("col","row")) %>%
#   adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") %>%
#   kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - Homem")
# 
# 
# ###Tipo de deficiência por tipo de violência não individualizada - tipodef_t_violência-8.4
# sinan %>% filter(!is.na(tipodef)) %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) %>% 
#   arrange(desc(notificações)) %>%  #Ordem tabelas do atlas. 
#   adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_Geral.xlsx")
#   kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Geral") 
# 
# #tipodef_t_violência-8.4 - Mulher
# sinan %>% filter(!is.na(tipodef) & cs_sexo == "F") %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) %>% 
#   arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
#   adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_mulher.xlsx")
#   kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Mulher")
# 
# 
# #tipodef_t_violência-8.4 - Homem
# sinan %>% filter(!is.na(tipodef) & cs_sexo == "M") %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) %>% 
#   arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
#   adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_homem.xlsx")
#   kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Homem")
# 
# 
# ###Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Geral
# library(scales)
# sinan %>% filter(!is.na(fxet80)) %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(fxet80) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
#   # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
#   #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
#   pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
#   pivot_wider(names_from = fxet80,values_from = Violências) %>% 
#   rio::export(x=., "base/pcd/base/t_violxfxet80.xlsx")
#   kable(caption = "Tipo de Violência x Faixa etária 80 - Geral")
# 
# 
# 
# #Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Mulher
# sinan %>% filter(!is.na(fxet80) & cs_sexo == "F") %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(fxet80) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
#   # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
#   #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
#   pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
#   pivot_wider(names_from = fxet80,values_from = Violências) %>% 
#   rio::export(x=., "base/pcd/base/t_violxfxet80_mulher.xlsx")
#   kable(caption = "Tipo de Violência x Faixa etária 80 - Mulher")
# 
# 
# 
# #Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Homem
# sinan %>% filter(!is.na(fxet80) & cs_sexo == "M") %>%
#   mutate(
#     v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
#     v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
#     v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
#     v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
#                           viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
#     v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
#   filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
#   group_by(fxet80) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
#   # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
#   #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
#   pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
#   pivot_wider(names_from = fxet80,values_from = Violências) %>% 
#   rio::export(x=., "base/pcd/base/t_violxfxet80_homem.xlsx") 
#   kable(caption = "Tipo de Violência x Faixa etária 80 - Homem")
