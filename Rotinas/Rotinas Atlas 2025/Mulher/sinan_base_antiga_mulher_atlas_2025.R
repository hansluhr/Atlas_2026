library(tidyverse)
library(janitor)
#Importando base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
year <- 2023


#Grupo\Autoria da Violência
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F"  & ano_not %in% year) |>
  tabyl(grupo_viol, show_missing_levels = FALSE, show_na = FALSE) |> arrange(desc(n)) |> adorn_totals(where = "row")  |>
  adorn_pct_formatting(digits = 1) |> rio::export(x = _, "mulher_grupo_viol.xlsx")
#Proporção dos prováveis autores\grupo de violência


#Tipo de violência, nos grupos\autorias da violência
sinan |> filter(grupo_viol != "Autoprovocada"  & cs_sexo == "F"  & ano_not %in% year) |>
  tabyl(t_viol,grupo_viol,#cs_raca,
        show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1, affix_sign = TRUE,) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") } ) |> 
  rio::export(x=_,"t_violência_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são violência física.  


#Local de Ocorrência da violência 
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" & cs_sexo == "F" & ano_not %in% year) |>
  #Ordem de apresentação de local de ocorrência
  mutate(local_ocor = local_ocor |> fct_relevel("Residência","Via pública",
                                                "Ignorado","Outro","Bar ou similar",
                                                "Comércio/Serviços","Escola",
                                                "Habitação coletiva","Local de prática esportiva",
                                                "Indústrias/Construção") ) |> 
  tabyl(local_ocor,grupo_viol,#cs_raca,
        show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |>
  rio::export(x=_,"local_ocor_por_grupo_autoria.xlsx")
#Das violências domésticas, x% ocorrem na residência. 


#Situação conjugal
sinan |> filter(grupo_viol != "Autoprovocada" &  cs_sexo == "F" & ano_not %in% year) |>
  tabyl(sit_conjug,grupo_viol,#cs_raca,
        show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x=_,"situacao_conjugal_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são Solteiras. 

#Ocorreu outraz vezes?
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> droplevels() |>
  tabyl(out_vezes,grupo_viol,#fxet,
        show_missing_levels = FALSE, show_na = FALSE) %>% 
  adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x=_,"ocorreu_outra_vez_por_grupo_autoria.xlsx")
#Das violências domésticas, x% a violência é repetição. 

#Instrumento da ocorrência
# sinan |> filter(grupo_viol != "Autoprovocada" &  cs_sexo == "F" & ano_not %in% year) |>
#   tabyl(ag_corte,grupo_viol,#cs_raca,
#         show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
#   adorn_ns(position = "front")
#Das violências domésticas, x% utilizam arma de fogo


# ##Horário da agressão por dia da semana
# sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" & cs_sexo == "F" & ano_not %in% year) |>
#   #Criando dia da Semana
#   mutate(wday = lubridate::wday(dt_notific, label = TRUE),
#   #Criando faixa de horários
#   # f_hora_ocor = hora_ocor |> as.character() |> as.numeric() ) |>
#   f_hora_ocor = hora_ocor |> as.POSIXct(format = "%H:%M") |> floor_date("1 hour")  ) |> 
#   #count(hora_ocor,f_hora_ocor) |> view()
# 
#   tabyl(f_hora_ocor,wday,
#   show_missing_levels = TRUE, show_na = TRUE) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |>
#   adorn_ns(position = "front")


#Criando faixa etária Mulher+
sinan %>% mutate(fxet = as.factor( 
  case_when(nu_idade_n<=4009 ~ "0 a 9",
            nu_idade_n>=4010  & nu_idade_n<=4014 ~ "10 a 14",
            nu_idade_n>=4015  & nu_idade_n<=4019 ~ "15 a 19",
            nu_idade_n>=4020  & nu_idade_n<=4024 ~ "20 a 24",
            nu_idade_n>=4025  & nu_idade_n<=4029 ~ "25 a 29",
            nu_idade_n>=4030  & nu_idade_n<=4034 ~ "30 a 34",
            nu_idade_n>=4035  & nu_idade_n<=4039 ~ "35 a 39",
            nu_idade_n>=4040  & nu_idade_n<=4044 ~ "40 a 44",
            nu_idade_n>=4045  & nu_idade_n<=4049 ~ "45 a 49",
            nu_idade_n>=4050  & nu_idade_n<=4054 ~ "50 a 54",
            nu_idade_n>=4055  & nu_idade_n<=4059 ~ "55 a 59",
            nu_idade_n>=4060  & nu_idade_n<=4064 ~ "60 a 64",
            nu_idade_n>=4065  & nu_idade_n<=4069 ~ "65 a 69",
            nu_idade_n>=4070  & nu_idade_n<=4074 ~ "70 a 74",
            nu_idade_n>=4075  & nu_idade_n<=4079 ~ "75 a 79",
            nu_idade_n>=4080  & !is.na(nu_idade_n) ~ "80 ou mais",
            is.na(nu_idade_n) ~ "Sem informação"))) -> sinan



#Faixa etária por autoria\grupo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" & cs_sexo == "F" & ano_not %in% year) |> droplevels() |>
  tabyl(fxet,autor_sexo,
        show_missing_levels = FALSE, show_na = FALSE) %>% 
  adorn_totals(where=c("col","row") ) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x=_,"faixa_etaria_sexo_autor_por_grupo_autoria.xlsx")




#Faixa etária por autoria\grupo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & cs_sexo == "F" & ano_not %in% year) |> droplevels() |>
  tabyl(fxet,grupo_viol,#autor_sexo,
        show_missing_levels = FALSE, show_na = FALSE) %>% 
  adorn_totals(where=c("col","row") ) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x=_,"faixa_etaria_sexo_autor_por_grupo_autoria.xlsx")
#Das violências domésticas, x% são na faixa 30 a 34. 
#Olhar sexo do provável autor da agressão.


#Faixa etária por tipo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" &  cs_sexo == "F" & ano_not %in% year) |>
  tabyl(fxet,t_viol,# autor_sexo,
        show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x=_,"faixa_etaria_por_tipo_violência.xlsx")
#Da viol sexual, 46,8% são na faixa 10 a 14. 


#Situação conjugal, por faixa etária
# sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica" & cs_sexo == "Mulher" & ano_not %in% year) |>
#   tabyl(fxet,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
#   ) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
#   adorn_ns(position = "front") |> rio::export(x=_,"sit_conju_fext_doméstica.xlsx")
#Das solteiras vítimas de violência doméstica, 18,7 % estão na faixa 10 a 14 anos. 


#Situação conjugal por faixa etária
# sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |>
#   tabyl(fxet,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
#   ) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
#   adorn_ns(position = "front")
#Da faixa etária 30 a 34 anos, 44,5% são Casado-União estável.


#Tipo de violência por situação conjugal.
# sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
#   tabyl(t_viol,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
#   ) %>% adorn_totals(where=c("col","row")) %>%
#   adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
#   adorn_ns(position = "front") |> rio::export(x = _, "t_viol_x_sit_conjug_doméstica.xlsx")
#Da violência física, as solteiras são 34,2% das vítimas


#Raça\Cor por tipo de violência
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(cs_raca,t_viol , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  rio::export(x = _, "cs_raca_x_t_viol_doméstica.xlsx")

#Raca\cor por escolaridade
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  #Ordenando escolaridade
  mutate(cs_escol_n = fct_relevel(
    cs_escol_n,"1ª a 4ª série incompleta do EF","4ª série completa do EF (antigo 1° grau)","5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
    "Ensino fundamental completo (antigo ginásio ou 1° grau)","Ensino médio incompleto (antigo colegial ou 2° grau)","Ensino médio completo (antigo colegial ou 2° grau)",
    "Educação superior incompleta","Educação superior completa","Ignorado","Não se aplica","Missing") ) |>
  tabyl(cs_raca,cs_escol_n , show_missing_levels = FALSE, show_na = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "cs_raca_x_escol_doméstica.xlsx")
#Dos brancos, 5,3% são 1ª a 4ª série incompleta do EF

#Raça\cor por faixa etária
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(fxet,cs_raca , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "fxet_x_cs_raca_doméstica.xlsx")
#Na fxet 0 a 9, os brancos são 39,1% 


#Raça\cor por situação conjutal
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(cs_raca,sit_conjug , show_missing_levels = FALSE, show_na = FALSE
  ) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "row") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front") |> rio::export(x = _, "cs_raca_x_sit_conj_doméstica.xlsx")
#Dos pardos 32,2% são Casado-União Consensual


#Suspeita de álcool
sinan |> filter(grupo_viol != "Autoprovocada" & grupo_viol == "Doméstica"  & cs_sexo == "F" & ano_not %in% year) |> 
  tabyl(autor_alco) |> rio::export(x=_,"alco_doméstica.xlsx")
#Em ao menos 33,05% das notificações de violência doméstica contra mulher, o autor estava com suspeita de uso de álcool. 
