library(tidyverse)
library(janitor)

#Pasta raiz
here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
###Importação de todos os arquivos 
#Pasta com os arquivos
filenames <- list.files(paste0(dirname(getwd()),"/bases/sinan_violencia/dbc"),
                        full.names = TRUE,pattern=".dbc")
#Lista dos arquivos
list_data <- lapply(filenames, read.dbc::read.dbc)
#Bind da lista de dbcs
sinan <- data.table::rbindlist(list_data, use.names = TRUE, fill=TRUE ) %>% clean_names()
rm(filenames,list_data); gc()
beepr::beep()


# Tratamento da base ------------------------------------------------------
#Ano de notificação
sinan %>% mutate(ano_not = as.factor(year(dt_notific))) -> sinan
sinan |> count(ano_not,nu_ano) 
#nu_ano é variável do sistema, ano de notificação.

#Ano de ocorrência
sinan %>% mutate(ano_ocor = as.factor(year(dt_ocor))) -> sinan

sinan %>% tabyl(ano_ocor,ano_not) 

#Tipo de notificação
sinan %>% mutate(tp_not = as.factor(case_when(tp_not == 1 ~ "Negativa", 
                                              tp_not == 2 ~ "Individual", tp_not == 3 ~ "Surto",tp_not == 4 ~ "Agregado"))) -> sinan

sinan %>% tabyl(ano_not, tp_not, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#Tipo de agravo
sinan %>% tabyl(ano_not, id_agravo, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#Data da notificação
sinan %>% tabyl(dt_notific,ano_not, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#Semana de notificação AAAASS
sinan %>% tabyl(sem_not, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#Ano de notificação - Ano de notificação dos microdados de 2019 contém missing.
sinan %>% tabyl(nu_ano,ano_not, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#UF de notificação
sinan %>% mutate(sg_uf_not = as.factor(case_when(sg_uf_not == 11 ~ "Rondônia",sg_uf_not == 12 ~"Acre", 
                                                 sg_uf_not == 13 ~ "Amazonas", sg_uf_not == 14 ~ "Roraima",  sg_uf_not == 15 ~"Pará",
                                                 sg_uf_not == 16 ~ "Amapá", sg_uf_not == 17 ~ "Tocantins", sg_uf_not == 21 ~"Maranhão",
                                                 sg_uf_not == 22 ~ "Piauí", sg_uf_not == 23 ~ "Ceará", sg_uf_not == 24 ~ "Rio Grande do Norte",
                                                 sg_uf_not == 25 ~ "Paraíba", sg_uf_not == 26 ~ "Pernambuco", sg_uf_not == 27 ~ "Alagoas",
                                                 sg_uf_not == 28 ~ "Sergipe", sg_uf_not == 29 ~ "Bahia", sg_uf_not == 31 ~ "Minas Gerais",
                                                 sg_uf_not == 32 ~ "Espírito Santo", sg_uf_not == 33 ~ "Rio de Janeiro", sg_uf_not == 35 ~ "São Paulo",
                                                 sg_uf_not == 41 ~ "Paraná", sg_uf_not == 42 ~ "Santa Catarina", sg_uf_not == 43 ~ "Rio Grande do Sul",
                                                 sg_uf_not == 50 ~ "Mato Grosso do Sul", sg_uf_not == 51 ~ "Mato Grosso",  sg_uf_not == 52 ~ "Goiás",
                                                 sg_uf_not == 53 ~ "Distrito Federal", sg_uf_not == 99 ~ "CNRAC"))) -> sinan

sinan %>% tabyl(sg_uf_not, show_na = T,show_missing_levels=T) %>% 
  adorn_totals(where = c("row"))

#12. Idade do paciente
#Existe variável data de nascimento. Poderia ser utilizada na construção da idade.
sinan %>% tabyl(nu_idade_n, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))
#Criando idade
sinan %>% mutate(idade = case_when(nu_idade_n <= 4000 ~ 0, nu_idade_n >4000 ~ nu_idade_n - 4000, TRUE ~ NA)) -> sinan
sinan %>% tabyl(idade)

#13. Sexo do paciente
sinan %>% tabyl(cs_sexo, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#14. Gestante
sinan %>% mutate(cs_gestant = case_when(cs_gestant == 1 ~ "1º Trimestre", cs_gestant ==  2 ~ "2º Trimestre",
                                        cs_gestant == 3 ~ "3º Trimestre", cs_gestant == 4 ~ "Idade gestacional ignorada",
                                        cs_gestant == 5 ~ "Não",cs_gestant == 6 ~ "Não se aplica", cs_gestant == 9 ~"Ignorado"),
                 cs_gestant =  fct_explicit_na(cs_gestant,na_level = "Missing")) -> sinan
sinan %>% tabyl(cs_gestant, cs_sexo, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#15. Raça\Cor
sinan %>% mutate(cs_raca = case_when(cs_raca == 1 ~ "Branco", cs_raca == 2 ~ "Preto", cs_raca == 3 ~ "Amarelo", 
                                     cs_raca == 4 ~ "Pardo", cs_raca == 5 ~ "Indígena", cs_raca == 9 ~ "Ignorado"),
                 cs_raca = fct_explicit_na(cs_raca,na_level = "Missing")) -> sinan
sinan %>% tabyl(ano_not,cs_raca, show_na = T,show_missing_levels=T) %>%  adorn_totals(where = c("row", "col"))

#16. Escolaridade - Ocorre valor zero que não está no dicionário. analfabeto?
sinan %>% mutate(cs_escol_n = 
                   case_when(cs_escol_n == "01" ~ "1ª a 4ª série incompleta do EF", cs_escol_n == "02" ~ "4ª série completa do EF (antigo 1° grau)",
                             cs_escol_n == "03" ~ "5ª à 8ª série incompleta do EF (antigo ginásio ou 1° grau)",
                             cs_escol_n == "04" ~ "Ensino fundamental completo (antigo ginásio ou 1° grau)",
                             cs_escol_n == "05" | cs_escol_n == "5" ~ "Ensino médio incompleto (antigo colegial ou 2° grau)", 
                             cs_escol_n == "06" ~ "Ensino médio completo (antigo colegial ou 2° grau)",
                             cs_escol_n == "07" ~ "Educação superior incompleta", cs_escol_n == "08" ~ "Educação superior completa", 
                             cs_escol_n == "09" ~ "Ignorado", cs_escol_n == "10" ~ "Não se aplica"),
                 cs_escol_n = fct_explicit_na(cs_escol_n,na_level = "Missing")) -> sinan

#Level 00 está classificado como missing
sinan %>% tabyl(ano_not,cs_escol_n, show_na = T,show_missing_levels=T) %>% 
  adorn_totals(where = c("row", "col")) 


#19 UF de residência
sinan %>% mutate(sg_uf = as.factor(case_when(sg_uf == 11 ~ "Rondônia",sg_uf == 12 ~"Acre", 
                                             sg_uf == 13 ~ "Amazonas", sg_uf == 14 ~ "Roraima",  sg_uf == 15 ~"Pará",
                                             sg_uf == 16 ~ "Amapá", sg_uf == 17 ~ "Tocantins", sg_uf == 21 ~"Maranhão",
                                             sg_uf == 22 ~ "Piauí", sg_uf == 23 ~ "Ceará", sg_uf == 24 ~ "Rio Grande do Norte",
                                             sg_uf == 25 ~ "Paraíba", sg_uf == 26 ~ "Pernambuco", sg_uf == 27 ~ "Alagoas",
                                             sg_uf == 28 ~ "Sergipe", sg_uf == 29 ~ "Bahia", sg_uf == 31 ~ "Minas Gerais",
                                             sg_uf == 32 ~ "Espírito Santo", sg_uf == 33 ~ "Rio de Janeiro", sg_uf == 35 ~ "São Paulo",
                                             sg_uf == 41 ~ "Paraná", sg_uf == 42 ~ "Santa Catarina", sg_uf == 43 ~ "Rio Grande do Sul",
                                             sg_uf == 50 ~ "Mato Grosso do Sul", sg_uf == 51 ~ "Mato Grosso",  sg_uf == 52 ~ "Goiás",
                                             sg_uf == 53 ~ "Distrito Federal", sg_uf == 99 ~ "CNRAC"))) -> sinan

sinan %>% tabyl(sg_uf_not, sg_uf, show_na = T,show_missing_levels=T) %>% 
  adorn_totals(where = c("row", "col"))

#20.Município de residência
sinan %>% tabyl(id_mn_resi, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row", "col")) %>% arrange(desc(n))


#20. Regional de saúde onde está localizado o município de residência do paciente por ocasião da notificação
sinan %>% tabyl(id_rg_resi, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row", "col")) %>% arrange(desc(n))


#32. País (se residente fora do Brasil)
sinan %>% tabyl(id_pais, show_na = T, show_missing_levels=T) %>%  adorn_totals(where = c("row", "col")) %>% arrange(desc(n))

#32. Não Listar/ Não Contar
sinan %>% tabyl(nduplic, show_na = T, show_missing_levels=T) %>%  adorn_totals(where = c("row", "col")) %>% arrange(desc(n))

#34. Ocupação
sinan %>% tabyl(id_ocupa_n, show_na = T, show_missing_levels=T) %>%  adorn_totals(where = c("row", "col")) %>% arrange(desc(n))

#35.Situação conjugal do Paciente
sinan %>% mutate(sit_conjug = case_when(sit_conjug == 1 ~ "Solteiro", sit_conjug == 2 ~ "Casado-União consensual",
                                        sit_conjug == 3 ~ "Viúvo",sit_conjug == 4 ~ "Separado", sit_conjug == 8 ~ "Não se aplica", 
                                        sit_conjug == 9 ~ "Ignorado"),
                 sit_conjug = fct_explicit_na(sit_conjug,na_level = "Missing")) -> sinan

sinan %>% filter(cs_sexo == "F") %>%
  tabyl(ano_not,sit_conjug, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row")) %>% adorn_percentages() %>%
  adorn_pct_formatting(digits = 2) 

#36 Orientação sexual do paciente
sinan %>% mutate(orient_sex = case_when(orient_sex == "1" ~ "Heterossexual", orient_sex == "2" ~ "Homossexual (gay-lésbica)",
                                        orient_sex == "3" ~ "Bissexual", orient_sex == "8" ~ "Não se aplica", orient_sex == "9" ~ "Ignorado"),
                 orient_sex = fct_explicit_na(orient_sex,na_level = "Missing")) -> sinan

sinan %>% tabyl(orient_sex, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row")) %>% adorn_pct_formatting(digits = 2) %>% arrange(desc(n))



#37 Identidade de gênero - valor 5 ausente do dicionário. Entra como missing.
sinan %>% mutate(ident_gen = case_when(ident_gen == 1  ~ "Travesti", ident_gen == 2 ~ "Transexual Mulher", 
                                       ident_gen == 3 ~ "Transexual Homem",ident_gen == 8 ~ "Não se aplica", 
                                       ident_gen == 9 ~ "Ignorado"), ident_gen = fct_explicit_na(ident_gen,na_level = "Missing")) -> sinan

sinan %>% tabyl(ident_gen, cs_sexo, show_na = T, show_missing_levels=T) %>% adorn_totals(where = c("row","col"))



#38. Possui algum tipo de deficiência/transtorno -  vai além das quatro deficiências principais - contém asterístico. Entra como missing
sinan %>% mutate(def_trans = case_when(def_trans == 1 ~ "Sim", def_trans == 2 ~ "Não", 
                                       def_trans == 9 ~ "Ignorado"),
                 def_trans = fct_explicit_na(def_trans,na_level = "Missing")) -> sinan
sinan %>% tabyl(def_trans, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col"))


#39. Deficiência física
sinan %>% mutate(def_fisica = case_when(def_fisica == 1 ~ "Sim", def_fisica == 2 ~ "Não",
                                        def_fisica == 8 ~ "Não se aplica", def_fisica == 9 ~ "Ignorado"),
                 def_fisica = fct_explicit_na(def_fisica,na_level = "Missing")) -> sinan

sinan %>% tabyl(def_fisica,def_trans, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 

#39. Deficiência intelectual
sinan %>% mutate(def_mental = 
                   case_when(def_mental == 1 ~ "Sim", def_mental == 2 ~ "Não",
                             def_mental == 8 ~ "Não se aplica",def_mental == 9 ~ "Ignorado"),
                 def_mental = fct_explicit_na(def_mental,na_level = "Missing")) -> sinan

sinan %>% tabyl(def_mental,def_trans, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 

#39. Def. Visual
sinan %>% mutate(def_visual = 
                   case_when(def_visual == 1 ~ "Sim", def_visual == 2 ~ "Não", 
                             def_visual == 8 ~ "Não se aplica",def_visual == 9 ~ "Ignorado"),
                 def_visual = fct_explicit_na(def_visual,na_level = "Missing")) -> sinan

sinan %>% tabyl(def_visual,def_trans, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 

#Def. auditiva
sinan %>% mutate(def_auditi = 
                   case_when(def_auditi == 1 ~ "Sim", def_auditi == 2 ~ "Não", 
                             def_auditi == 8 ~ "Não se aplica",def_auditi == 9 ~ "Ignorado"),
                 def_auditi = fct_explicit_na(def_auditi,na_level = "Missing")) -> sinan

sinan %>% tabyl(def_auditi,def_trans, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 

#39. Transtorno mental
sinan %>% mutate(tran_ment =
                   case_when(tran_ment == 1 ~ "Sim", tran_ment == 2 ~ "Não",
                             tran_ment == 8 ~ "Não se aplica",tran_ment == 9 ~ "Ignorado"),
                 tran_ment = fct_explicit_na(tran_ment,na_level = "Missing")) -> sinan

sinan %>% tabyl(tran_ment,def_trans, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row","col"))

#39. Transtorno comportamental
sinan %>% mutate(tran_comp = 
                   case_when(tran_comp == 1 ~ "Sim", tran_comp == 2 ~ "Não",
                             tran_comp == 8 ~ "Não se aplica",tran_comp == 9 ~ "Ignorado"),
                 tran_comp = fct_explicit_na(tran_comp,na_level = "Missing")) -> sinan

sinan %>% tabyl(tran_comp,def_trans, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row","col")) 

#39. Outras deficiências 
sinan %>% mutate(def_out = 
                   case_when(def_out == 1 ~ "Sim", def_out == 2 ~ "Não",
                             def_out == 8 ~ "Não se aplica",def_out == 9 ~ "Ignorado"), 
                 def_out = fct_explicit_na(def_out, na_level = "Missing")) -> sinan

sinan %>% tabyl(def_out,def_trans,
                show_na = T, show_missing_levels=T) %>% adorn_totals(where = c("row","col"))

#39. Outras deficiências/ síndromes (especificar)
sinan %>% tabyl(def_espec, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row")) %>% adorn_pct_formatting(digits = 2) %>% arrange(desc(n))


#Tabela com a qualidade  das variáveis sobre deficiência\transtorno
sinan %>% tabyl(tran_comp, show_na = T, show_missing_levels=T) %>%  adorn_totals(where = c("row")) 


# Criando tipo de deficiência - considerando as 4 principais defic e transtorno mental--------
sinan %>% mutate(def = as.factor(case_when(
  #Transtorno mental
  tran_ment == "Sim" | 
  #Deficiências
  def_fisica=="Sim" | def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim" ~ "Sim"))) -> sinan

sinan %>% tabyl(ano_not,def, show_na = T, show_missing_levels=T) %>%
  adorn_totals(where = c("row","col")) 


# Individualizando as deficiências principais -----------------------------
sinan %>% mutate( 
  tipodef = case_when(
    #Individualizar significa ter certeza das deficiências. 
    #Então def_fisica == "Sim" e def_visual == "Ignorado" não é ter certeza. Não sei o que é ignorado. 
    #Por segurança vou deixar somente o Não nas condições secundárias.
    #Somente transtorno mental
    tran_ment == "Sim" & (def_fisica == "Não" & def_visual== "Não" & def_mental== "Não"  & def_auditi== "Não") ~ "Trans. Mental",
    #Somente deficiência física. 
    def_fisica == "Sim" & (tran_ment == "Não" & def_visual== "Não" & def_mental== "Não"  & def_auditi== "Não") ~ "Física",
    #Somente deficiência Intelectual
    def_mental == "Sim" & (tran_ment == "Não" & def_visual== "Não" & def_fisica== "Não" & def_auditi== "Não") ~ "Intelectual",
    #Somente deficiência visual
    def_visual == "Sim" & (tran_ment == "Não" & def_fisica== "Não" & def_mental== "Não" & def_auditi== "Não") ~ "Visual",
    #Somente deficiência auditiva
    def_auditi == "Sim" & (tran_ment == "Não" & def_fisica== "Não" & def_mental== "Não" & def_visual== "Não") ~ "Auditiva"), 
  #Pessoa com múltiplas deficiênciais ou transtorno mental
  tipodef = replace(tipodef,tran_ment == "Sim" & (def_fisica =="Sim"|def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_fisica =="Sim" & (tran_ment == "Sim"|def_visual=="Sim" | def_mental=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_mental =="Sim" & (tran_ment == "Sim"|def_visual=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = replace(tipodef,def_visual =="Sim" & (tran_ment == "Sim"|def_mental=="Sim" | def_fisica=="Sim" | def_auditi=="Sim"),"Múltipla"),
  tipodef = as.factor(replace(tipodef,def_auditi =="Sim" & (tran_ment == "Sim"| def_mental=="Sim" | def_fisica=="Sim" | def_visual=="Sim"),"Múltipla"))) -> sinan

sinan %>% tabyl(ano_not,tipodef, show_na = T, show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 

#40. UF de Ocorrência
sinan %>% mutate(sg_uf_ocor = as.factor(case_when(sg_uf_ocor == 11 ~ "Rondônia",sg_uf_ocor == 12 ~"Acre", 
                                                  sg_uf_ocor == 13 ~ "Amazonas", sg_uf_ocor == 14 ~ "Roraima",  sg_uf_ocor == 15 ~"Pará",
                                                  sg_uf_ocor == 16 ~ "Amapá", sg_uf_ocor == 17 ~ "Tocantins", sg_uf_ocor == 21 ~"Maranhão",
                                                  sg_uf_ocor == 22 ~ "Piauí", sg_uf_ocor == 23 ~ "Ceará", sg_uf_ocor == 24 ~ "Rio Grande do Norte",
                                                  sg_uf_ocor == 25 ~ "Paraíba", sg_uf_ocor == 26 ~ "Pernambuco", sg_uf_ocor == 27 ~ "Alagoas",
                                                  sg_uf_ocor == 28 ~ "Sergipe", sg_uf_ocor == 29 ~ "Bahia", sg_uf_ocor == 31 ~ "Minas Gerais",
                                                  sg_uf_ocor == 32 ~ "Espírito Santo", sg_uf_ocor == 33 ~ "Rio de Janeiro", sg_uf_ocor == 35 ~ "São Paulo",
                                                  sg_uf_ocor == 41 ~ "Paraná", sg_uf_ocor == 42 ~ "Santa Catarina", sg_uf_ocor == 43 ~ "Rio Grande do Sul",
                                                  sg_uf_ocor == 50 ~ "Mato Grosso do Sul", sg_uf_ocor == 51 ~ "Mato Grosso",  sg_uf_ocor == 52 ~ "Goiás",
                                                  sg_uf_ocor == 53 ~ "Distrito Federal", sg_uf_ocor == 99 ~ "CNRAC"))) -> sinan

sinan %>% tabyl(sg_uf_ocor,ano_not, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row", "col")) 

#41. Município de ocorrência
sinan %>% tabyl(id_mn_ocor, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row", "col")) %>% arrange(desc(n))

#51. Hora de ocorrência
sinan %>% tabyl(hora_ocor, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row", "col")) %>% arrange(desc(n))


#52. Local de ocorrência
sinan %>% mutate(local_ocor = as.factor(case_when(local_ocor == "01" ~ "Residência", local_ocor == "02" ~ "Habitação coletiva",
                                                  local_ocor == "03" ~ "Escola", local_ocor == "04" ~ "Local de prática esportiva", local_ocor == "05" ~ "Bar ou similar",
                                                  local_ocor == "06" ~ "Via pública", local_ocor == "07" ~ "Comércio/Serviços", local_ocor == "08" ~ "Indústrias/Construção",
                                                  local_ocor == "09" ~ "Outro", local_ocor == "99" ~ "Ignorado"))) -> sinan

sinan %>%  tabyl(local_ocor,ano_not, show_na = T,show_missing_levels=T) %>% 
  adorn_totals(where = c("row","col")) 


#52. Local de ocorrência - Outro (especificar) se local_ocor = 9 "Outro", então especificar.
sinan %>% filter(local_ocor == "Outro") %>% droplevels() %>%
  tabyl(local_espe, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) %>% slice_head(n=20) %>%
  arrange(desc(n)) #Outro com vários problemas.

#53. Ocorreu outra vez?
sinan %>% mutate(out_vezes = as.factor(case_when(out_vezes == 1 ~ "Sim", out_vezes == 2 ~ "Não", 
                                                 out_vezes == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(out_vezes,ano_not, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row"))

#54. Lesão autoprovocada.
sinan %>% mutate(les_autop = as.factor(case_when(les_autop == 1 ~ "Sim", les_autop == 2 ~ "Não", 
                                                 les_autop == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,les_autop, show_na = T,show_missing_levels=T) %>% 
  adorn_totals(where = c("row")) 

#55. Violência motivada por
sinan %>% mutate(viol_motiv = as.factor(case_when(viol_motiv == "01" ~ "Sexismo",
                                                  viol_motiv == "02" ~ "Homofobia/Lesbofobia/Bifobia/Transfobia",viol_motiv == "03" ~ "Racismo",
                                                  viol_motiv == "04" ~ "Intolerância religiosa", viol_motiv == "05" ~ "Xenofobia", viol_motiv == "06" ~ "Conflito geracional",
                                                  viol_motiv == "07" ~ "Situação de rua", viol_motiv == "08" ~ "Deficiência", viol_motiv == "09" ~ "Outros",
                                                  viol_motiv == "88" ~ "Não se aplica", viol_motiv == "99" ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_motiv, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


# Tipos de violência ------------------------------------------------------
#Houve violência física?
#Assumindo * como missing.
sinan %>% mutate(viol_fisic = as.factor(case_when(viol_fisic == 1 ~ "Sim", viol_fisic == 2 ~ "Não", 
                                                  viol_fisic == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_fisic, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve violência psicológica/moral?
#Assumindo * como missing.
sinan %>% mutate(viol_psico = as.factor(case_when(viol_psico == 1 ~ "Sim", viol_psico == 2 ~ "Não", 
                                                  viol_psico == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_psico, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve tortura?
sinan %>% mutate(viol_tort = as.factor(case_when(viol_tort == 1 ~ "Sim", viol_tort == 2 ~ "Não", 
                                                 viol_tort == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_tort, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve violência sexual?
sinan %>% mutate(viol_sexu = as.factor(case_when(viol_sexu == 1 ~ "Sim", viol_sexu == 2 ~ "Não", 
                                                 viol_sexu == 9 ~ "Ignorado"))) -> sinan 
sinan %>% tabyl(ano_not,viol_sexu, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve tráfico de seres humanos? 
sinan %>% mutate(viol_traf = as.factor(case_when(viol_traf == 1 ~ "Sim", viol_traf == 2 ~ "Não", 
                                                 viol_traf == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_traf, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve violência financeira?
sinan %>% mutate(viol_finan = as.factor(case_when(viol_finan == 1 ~ "Sim", viol_finan == 2 ~ "Não", 
                                                  viol_finan == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_finan, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Houve negligência/abandono?
sinan %>% mutate(viol_negli = as.factor(case_when(viol_negli == 1 ~ "Sim", viol_negli == 2 ~ "Não",
                                                  viol_negli == 9 ~ "Ignorado"))) -> sinan  
sinan %>% tabyl(ano_not,viol_negli, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Ocorreu trabalho infantil
sinan %>% mutate(viol_infan = as.factor(case_when(viol_infan == 1 ~ "Sim", viol_infan == 2 ~ "Não",
                                                  viol_infan == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_infan, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#56. Intervenção legal?
sinan %>% mutate(viol_legal = as.factor(case_when(viol_legal == 1 ~ "Sim", viol_legal == 2 ~ "Não",
                                                  viol_legal == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_legal, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Houve outro tipo de violência?
sinan %>% mutate(viol_outr = as.factor(case_when(viol_outr == 1 ~ "Sim", viol_outr == 2 ~ "Não",
                                                 viol_outr == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,viol_outr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Especificar tipo de violência - Se viol_outr com problema, então especificar em viol_espec.
sinan %>% filter(viol_outr == "Não") %>% droplevels() %>% 
  tabyl(viol_espec, show_na = T,show_missing_levels=T) %>% arrange(desc(n))


# Individualizando o tipo de violência ------------------------------------
sinan %>% 
  #Violência física.
  #Individualizando as violências.
  mutate(t_viol = case_when(viol_fisic == "Sim" & viol_psico=="Não" & viol_tort== "Não" & 
                              viol_sexu== "Não" & viol_traf== "Não" & viol_finan== "Não" & viol_negli== "Não" & viol_infan== "Não" & 
                              viol_legal== "Não" & viol_outr== "Não" ~ "V.Física",
                            #Viol. Sexual 
                            viol_fisic=="Não" & viol_psico=="Não" & viol_tort=="Não" & viol_sexu=="Sim" & viol_traf=="Não" & viol_finan=="Não" & 
                              viol_negli=="Não" & viol_infan=="Não" & viol_legal=="Não" & viol_outr=="Não" ~ "V.Sexual",
                            #Negligência
                            viol_fisic=="Não" & viol_psico=="Não" & viol_tort=="Não" & viol_sexu=="Não" & viol_traf=="Não" & viol_finan=="Não" & 
                              viol_negli=="Sim" & viol_infan=="Não" & viol_legal=="Não" & viol_outr=="Não" ~ "Negligência",
                            #Viol. Outros inclui outros, tortura, tráfico de seres humanos, trabalho infantil, intervenção legal
                            (viol_outr=="Sim" | viol_tort=="Sim" | viol_traf=="Sim" | viol_infan=="Sim" | viol_legal=="Sim") & viol_fisic=="Não" & 
                              viol_psico=="Não" &  viol_sexu=="Não" & viol_finan=="Não" & viol_negli=="Não" ~ "Outro"),
         #Viol. Psico inclui psicológica, financeira/econômica), 
         t_viol = replace(t_viol,viol_finan=="Sim" & (viol_fisic=="Não" & viol_tort=="Não" & viol_sexu=="Não" & 
                                                        viol_traf=="Não"  & viol_negli=="Não" & viol_infan=="Não" & viol_legal=="Não" & viol_outr=="Não"),"V.Psico"),
         t_viol = replace(t_viol,viol_psico=="Sim" & (viol_fisic=="Não" & viol_tort=="Não" & viol_sexu=="Não" & 
                                                        viol_traf=="Não"  & viol_negli=="Não" & viol_infan=="Não" & viol_legal=="Não" & viol_outr=="Não"),"V.Psico"), 
         #Tipo de violência múltipla 
         t_viol = replace(t_viol,viol_fisic=="Sim" & (viol_psico=="Sim" | viol_tort=="Sim" | viol_sexu=="Sim" | viol_traf=="Sim" | viol_finan=="Sim" | viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),
         t_viol = replace(t_viol,viol_psico=="Sim" & (viol_tort=="Sim" | viol_sexu=="Sim" | viol_traf=="Sim" | viol_finan=="Sim" | viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),
         t_viol = replace(t_viol,viol_tort=="Sim" & (viol_sexu=="Sim" | viol_traf=="Sim" | viol_finan=="Sim" | viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),
         t_viol = replace(t_viol,viol_sexu=="Sim" & (viol_traf=="Sim" | viol_finan=="Sim" | viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),                     
         t_viol = replace(t_viol,viol_traf=="Sim" & (viol_finan=="Sim" | viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),                     
         t_viol = replace(t_viol,viol_finan=="Sim" & (viol_negli=="Sim" | viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),                    
         t_viol = replace(t_viol,viol_negli=="Sim" & (viol_infan=="Sim" | viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),                       
         t_viol = replace(t_viol,viol_infan=="Sim" & (viol_legal=="Sim" | viol_outr=="Sim"),"Multipla"),
         t_viol = as.factor(replace(t_viol,viol_legal=="Sim" & viol_outr=="Sim","Multipla"))) -> sinan

sinan %>% tabyl(t_viol, ano_not, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row","col"))

# Meio de agressão --------------------------------------------------------
#57. Agressão através de força corporal?
sinan %>% mutate(ag_forca = as.factor(case_when(ag_forca == 1 ~ "Sim", 
                                                ag_forca == 2 ~ "Não",ag_forca == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_forca, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de enforcamento?
sinan %>% mutate(ag_enfor = as.factor(case_when(ag_enfor == 1 ~ "Sim", 
                                                ag_enfor == 2 ~ "Não",ag_enfor == 9 ~ "Ignorado"))) -> sinan 
sinan %>% tabyl(ano_not,ag_enfor, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57.  Agressão através de objeto contundente?
sinan %>% mutate(ag_objeto = as.factor(case_when(ag_objeto == 1 ~ "Sim", 
                                                 ag_objeto == 2 ~ "Não",ag_objeto == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_objeto, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de objeto perfuro-cortante?
sinan %>% mutate(ag_corte = as.factor(case_when(ag_corte == 1 ~ "Sim", 
                                                ag_corte == 2 ~ "Não",ag_corte == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_corte, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de objeto substancia/objeto quente? 
sinan %>% mutate(ag_quente = as.factor(case_when(ag_quente == 1 ~ "Sim", 
                                                 ag_quente == 2 ~ "Não",ag_quente == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_quente, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de envenenamento/Intoxicação?
sinan %>% mutate(ag_enven = as.factor(case_when(ag_enven == 1 ~ "Sim", 
                                                ag_enven == 2 ~ "Não",ag_enven == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_enven, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de Arma de fogo?
sinan %>% mutate(ag_fogo = as.factor(case_when(ag_fogo == 1 ~ "Sim", 
                                               ag_fogo == 2 ~ "Não",ag_fogo == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_fogo, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de Ameaça?
sinan %>% mutate(ag_ameaca = as.factor(case_when(ag_ameaca == 1 ~ "Sim", 
                                                 ag_ameaca == 2 ~ "Não",ag_ameaca == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_ameaca, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#57. Agressão através de outros?
sinan %>% mutate(ag_outros = as.factor(case_when(ag_outros == 1 ~ "Sim", 
                                                 ag_outros == 2 ~ "Não",ag_outros == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,ag_outros, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


###Especificar outro meio de agressão. se ag_outros == Sim, então especificar.
sinan %>% filter(ag_outros == "Não") %>% droplevels() %>%
  tabyl(ag_espec, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) %>%
  arrange(desc(n))

#Olhando outro meio de agressão x houve envenenamento
sinan %>% filter(ag_outros == "Não") %>% droplevels() %>%
  tabyl(ag_espec,ag_enven, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row"))

# Tipo de violência -------------------------------------------------------
#58. Se ocorreu violência sexual, qual tipo? - Assedio sexual
#Assumindo *,M,F e O como missing.
sinan %>% mutate(sex_assedi = as.factor(case_when(sex_assedi == 1 ~ "Sim", sex_assedi == 2 ~ "Não",sex_assedi == 8 ~ "Não se aplica",
                                                  sex_assedi == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,sex_assedi, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


#58 Se ocorreu violência sexual, qual tipo? - Estupro
sinan %>% mutate(sex_estupr = as.factor(case_when(sex_estupr == 1 ~ "Sim", sex_estupr == 2 ~ "Não",
                                                  sex_estupr == 8 ~ "Não se aplica",sex_estupr == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,sex_estupr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
#Houve estupro x houve violência sexual.
sinan %>% tabyl(sex_estupr,viol_sexu) %>% adorn_title(placement="top")


#58. Se ocorreu violência sexual, qual tipo? - Pornografia infantil
sinan %>% mutate(sex_porno = as.factor(case_when(sex_porno == 1 ~ "Sim", sex_porno == 2 ~ "Não",
                                                 sex_porno == 8 ~ "Não se aplica",sex_porno == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,sex_porno, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
#Houve Pornografia infantil x violência sexual.
sinan %>% tabyl(sex_porno,viol_sexu) %>% adorn_title(placement="top")

#58. Se ocorreu violência sexual, qual tipo? - Exploração sexual
sinan %>% mutate(sex_explo = as.factor(case_when(sex_explo == 1 ~ "Sim", sex_explo == 2 ~ "Não",
                                                 sex_explo == 8 ~ "Não se aplica",sex_explo == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,sex_explo, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#58. Se ocorreu violência sexual, qual tipo? -Outro
sinan %>% mutate(sex_outro = as.factor(case_when(sex_outro == 1 ~ "Sim", sex_outro == 2 ~ "Não",
                                                 sex_outro == 8 ~ "Não se aplica",sex_outro == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,sex_outro, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row"))  


# Se ocorreu violência sexual, qual tipo? -Outro =1 
sinan %>% filter(sex_outro == "Não") %>% droplevels() %>%
  tabyl(sex_espec, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) %>%
  arrange(desc(n))


# Ocorreu Penetração. Qual tipo? ------------------------------------------
#Variáveis penetração mal prenchidas. 
sinan %>% tabyl(ano_not,pen_oral, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row"))  
#Variáveis penetração mal prenchidas. 
sinan %>% tabyl(ano_not,pen_vagina, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row"))  


# Procedimento realizado --------------------------------------------------
#Procedimento realizado -Profilaxia DST
sinan %>% mutate(proc_dst = as.factor(case_when(proc_dst == 1 ~ "Sim", proc_dst == 2 ~ "Não",
                                                proc_dst == 8 ~ "Não se aplica",proc_dst == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_dst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#59. Procedimento realizado -Profilaxia HIV
sinan %>% mutate(proc_hiv = as.factor(case_when(proc_hiv == 1 ~ "Sim", proc_hiv == 2 ~ "Não",
                                                proc_hiv == 8 ~ "Não se aplica",proc_hiv == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_hiv, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

# 59 Procedimento realizado - Profilaxia Hepatite B
sinan %>% mutate(proc_hepb = as.factor(case_when(proc_hepb == 1 ~ "Sim", proc_hepb == 2 ~ "Não",
                                                 proc_hepb == 8 ~ "Não se aplica",proc_hepb == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_hepb, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#59. Procedimento realizado - Coleta de sangue
sinan %>% mutate(proc_sang = as.factor(case_when(proc_sang == 1 ~ "Sim", proc_sang == 2 ~ "Não",
                                                 proc_sang == 8 ~ "Não se aplica",proc_sang == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_sang, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


#59. Procedimento realizado - Coleta de sêmen
sinan %>% mutate(proc_semen = as.factor(case_when(proc_semen == 1 ~ "Sim", proc_semen == 2 ~ "Não",
                                                  proc_semen == 8 ~ "Não se aplica",proc_semen == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_semen, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#59. Procedimento realizado - Coleta de secreção vaginal
sinan %>% mutate(proc_vagin = as.factor(case_when(proc_vagin == 1 ~ "Sim", proc_vagin == 2 ~ "Não",
                                                  proc_vagin == 8 ~ "Não se aplica",proc_vagin == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_vagin, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#59. Procedimento realizado - Contracepção de emergência
sinan %>% mutate(proc_contr = as.factor(case_when(proc_contr == 1 ~ "Sim", proc_contr == 2 ~ "Não",
                                                  proc_contr == 8 ~ "Não se aplica",proc_contr == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_contr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#59. Procedimento realizado - Aborto previsto em lei
sinan %>% mutate(proc_abort = as.factor(case_when(proc_abort == 1 ~ "Sim", proc_abort == 2 ~ "Não",
                                                  proc_abort == 8 ~ "Não se aplica",proc_abort == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,proc_abort, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


# Conseqüências da ocorrência no momento da notificação -------------------------------------------------------




# Autor da agressão -------------------------------------------------------
#60. Número de envolvidos 
#Assumindo 3 e * como missing.
sinan %>% mutate(num_envolv = as.factor(case_when(num_envolv == 1 ~ "Um", num_envolv == 2 ~ "Dois ou mais",
                                                  num_envolv == 3 ~ "Ignorado",num_envolv == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,num_envolv, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#61. Provável autor da agressão era o pai?
sinan %>% mutate(rel_pai = as.factor(case_when(rel_pai == 1 ~ "Sim", rel_pai == 2 ~ "Não",
                                               rel_pai == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_pai, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era a mae?
sinan %>% mutate(rel_mae = as.factor(case_when(rel_mae == 1 ~ "Sim", rel_mae == 2 ~ "Não",
                                               rel_mae == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_mae, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o padastro?
sinan %>% mutate(rel_pad = as.factor(case_when(rel_pad == 1 ~ "Sim", rel_pad == 2 ~ "Não",
                                               rel_pad == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_pad, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era a madastra?
sinan %>% mutate(rel_mad = as.factor(case_when(rel_mad == 1 ~ "Sim", rel_mad == 2 ~ "Não",
                                               rel_mad == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_mad, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o cônjuge?
sinan %>% mutate(rel_conj = as.factor(case_when(rel_conj == 1 ~ "Sim", rel_conj == 2 ~ "Não",
                                                rel_conj == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_conj, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o namorado?
sinan %>% mutate(rel_namo = as.factor(case_when(rel_namo == 1 ~ "Sim", rel_namo == 2 ~ "Não",
                                                rel_namo == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_namo, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o ex-cônjuge?
sinan %>% mutate(rel_excon = as.factor(case_when(rel_excon == 1 ~ "Sim", rel_excon == 2 ~ "Não",
                                                 rel_excon == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_excon, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o ex-namorado?
sinan %>% mutate(rel_exnam = as.factor(case_when(rel_exnam == 1 ~ "Sim", rel_exnam == 2 ~ "Não",
                                                 rel_exnam == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_exnam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o filho(a)?
sinan %>% mutate(rel_filho = as.factor(case_when(rel_filho == 1 ~ "Sim", rel_filho == 2 ~ "Não",
                                                 rel_filho == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_filho, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era desconhecido?
sinan %>% mutate(rel_desco = as.factor(case_when(rel_desco == 1 ~ "Sim", rel_desco == 2 ~ "Não",
                                                 rel_desco == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_desco, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era irmão ou irmã?
sinan %>% mutate(rel_irmao = as.factor(case_when(rel_irmao == 1 ~ "Sim", rel_irmao == 2 ~ "Não",
                                                 rel_irmao == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_irmao, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão eram amigo(s)/conhecido(s)?
sinan %>% mutate(rel_conhec = as.factor(case_when(rel_conhec == 1 ~ "Sim", rel_conhec == 2 ~ "Não",
                                                  rel_conhec == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_conhec, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o cuidador(a)?
sinan %>% mutate(rel_cuida = as.factor(case_when(rel_cuida == 1 ~ "Sim", rel_cuida == 2 ~ "Não",
                                                 rel_cuida == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_cuida, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era o patrã(a)?
sinan %>% mutate(rel_patrao = as.factor(case_when(rel_patrao == 1 ~ "Sim", rel_patrao == 2 ~ "Não",
                                                  rel_patrao == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_patrao, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era pessoa com relação institucional?
sinan %>% mutate(rel_inst = as.factor(case_when(rel_inst == 1 ~ "Sim", rel_inst == 2 ~ "Não",
                                                rel_inst == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_inst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era policial/agente da lei?
sinan %>% mutate(rel_pol = as.factor(case_when(rel_pol == 1 ~ "Sim", rel_pol == 2 ~ "Não",
                                               rel_pol == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_pol, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão era a prórpia pessoa?
sinan %>% mutate(rel_propri = as.factor(case_when(rel_propri == 1 ~ "Sim", rel_propri == 2 ~ "Não",
                                                  rel_propri == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_propri, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Provável autor da agressão tinha outro tipo de relação?
sinan %>% mutate(rel_outros = as.factor(case_when(rel_outros == 1 ~ "Sim", rel_outros == 2 ~ "Não",
                                                  rel_outros == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_outros, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


#Especificar relação outros 
sinan %>% filter(rel_outros == "Não") %>% droplevels() %>%
  tabyl(rel_espec) %>% arrange(desc(n))


#Sexo do provável agressor.
sinan %>% mutate(autor_sexo = as.factor(case_when(autor_sexo == 1 ~ "Masculino", autor_sexo == 2 ~ "Feminino",
                                                  autor_sexo == 3 ~ "Ambos os sexos",autor_sexo == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,autor_sexo, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


#Provável autor tinha suspeita de alcóol?
sinan %>% mutate(autor_alco = as.factor(case_when(autor_alco == 1 ~ "Sim", autor_alco == 2 ~ "Não",
                                                  autor_alco == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,autor_alco, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

sinan %>% filter(ano_not %in% c(2009:2022)) %>% droplevels() %>%
  tabyl(ano_not,autor_alco, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) %>% 
  adorn_percentages() %>% adorn_pct_formatting(digits = 2)


#Ciclo de vida autor da agressão.
sinan %>% mutate(cicl_vid = as.factor(
  case_when(cicl_vid == 1 ~ "Criança",cicl_vid == 2 ~ "Adolescente",cicl_vid == 3 ~ "Jovem",
            cicl_vid == 4 ~ "Pessoa adulta",cicl_vid == 5 ~ "Pessoa idosa", cicl_vid == 9 ~ "Ignorado"))) -> sinan

sinan %>% tabyl(ano_not,cicl_vid, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


###Criando grupo agressor - Como melhorar essa rotina?
sinan %>% mutate(
  #Doméstica
  agr_fam = case_when(rel_pai=="Sim" | rel_mae== "Sim" | rel_mad== "Sim" | rel_pad=="Sim" | rel_conj=="Sim" | rel_excon=="Sim" |
                        rel_namo=="Sim" | rel_exnam=="Sim" | rel_filho=="Sim" | rel_irmao=="Sim" | rel_cuida=="Sim" ~ "Sim", TRUE ~ "Não"),
  #Comunitária
  agr_extfam = case_when(rel_conhec=="Sim" | rel_desco=="Sim" ~ "Sim", TRUE ~ "Não"),
  #Institucional
  agr_inst = case_when(rel_patrao=="Sim" | rel_inst=="Sim" | rel_pol=="Sim" ~ "Sim", TRUE ~ "Não"),
  #*Autoprovocada*
  agr_propr = case_when(rel_propri=="Sim" ~ "Sim", TRUE ~ "Não"),
  #*Outros*
  agr_outro = case_when(rel_outros=="Sim" ~ "Sim", TRUE ~ "Não"),
  #Qualquer uma das anteriores
  agr = case_when(rel_pai=="Sim" | rel_mae== "Sim" | rel_mad== "Sim" | rel_pad=="Sim" | rel_conj=="Sim" | rel_excon=="Sim" | 
                    rel_namo=="Sim" | rel_exnam=="Sim" | rel_filho=="Sim" | rel_irmao=="Sim" | rel_cuida=="Sim" | rel_conhec=="Sim" |
                    rel_desco=="Sim" | rel_patrao=="Sim" | rel_inst=="Sim" | rel_pol=="Sim" | rel_propri=="Sim" | rel_outros=="Sim" ~ "Sim", TRUE ~ "Não")) -> sinan


sinan %>% tabyl(ano_not,agr_fam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,agr_extfam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,agr_inst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,agr_propr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,agr_outro, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,agr, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Individualizando o grupo de agressão.
sinan %>% mutate(g_fam = case_when(agr_fam=="Sim" & (agr_extfam!="Sim" & agr_inst!="Sim" & agr_propr!="Sim" & agr_outro!="Sim") ~ "Sim")) -> sinan
sinan %>% mutate(g_extrafam = case_when(agr_extfam=="Sim" & (agr_fam=="Não" & agr_inst=="Não" & agr_propr=="Não" & agr_outro=="Não") ~ "Sim")) -> sinan
sinan %>% mutate(g_inst = case_when(agr_inst=="Sim" & (agr_fam!="Sim" & agr_extfam!="Sim" & agr_propr!="Sim" & agr_outro!="Sim") ~ "Sim")) -> sinan
sinan %>% mutate(g_auto = case_when(agr_propr=="Sim" & (agr_fam!="Sim" & agr_inst!="Sim" & agr_extfam!="Sim" & agr_outro!="Sim") ~ "Sim")) -> sinan
sinan %>% mutate(g_outros = case_when(agr_outro=="Sim" & (agr_fam!="Sim" & agr_inst!="Sim" & agr_extfam!="Sim" & agr_propr!="Sim") ~ "Sim")) -> sinan

sinan %>% tabyl(ano_not,g_fam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,g_extrafam, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,g_inst, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,g_auto, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 
sinan %>% tabyl(ano_not,g_outros, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 


#Variável única do grupo de agressão.- Melhorar a criação de grupo_viol
sinan %>% mutate(grupo_viol = case_when(g_fam == "Sim" ~ "Doméstica",
                                        g_extrafam=="Sim" ~ "Comunitária",
                                        g_inst=="Sim" ~ "Institucional", 
                                        g_auto=="Sim" ~ "Autoprovocada")) -> sinan
sinan %>% mutate(grupo_viol = as.factor(replace(grupo_viol,
                                                (g_outros == "Sim" | (agr == "Sim" & is.na(grupo_viol))),"Misto"))) -> sinan

sinan %>% tabyl(ano_not,grupo_viol, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

sinan %>% select(!c(agr_fam, agr_extfam, agr_fam, agr_extfam,  agr_inst,  agr_propr, 
                    agr_outro, agr, g_fam, g_extrafam, g_inst, g_auto, g_outros)) -> sinan


# Encaminhamento ----------------------------------------------------------
#Encaminhamento - Rede da Saúde?
sinan %>% mutate(rede_sau = as.factor(case_when(rede_sau == 1 ~ "Sim", rede_sau == 2 ~ "Não",
                                                rede_sau == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(nu_ano,rede_sau, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento a rede de assistência social?
sinan %>% mutate(assist_soc = as.factor(case_when(assist_soc == 1 ~ "Sim", assist_soc == 2 ~ "Não",
                                                  assist_soc == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,assist_soc, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento no setor da rede Educação?
sinan %>% mutate(rede_educa = as.factor(case_when(rede_educa == 1 ~ "Sim", rede_educa == 2 ~ "Não",
                                                  rede_educa == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rede_educa, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento no setor da rede atendimento à mulher?
sinan %>% mutate(atend_mulh = as.factor(case_when(atend_mulh == 1 ~ "Sim", atend_mulh == 2 ~ "Não",
                                                  atend_mulh == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,atend_mulh, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para o Conselho Tutelar?
sinan %>% mutate(cons_tutel = as.factor(case_when(cons_tutel == 1 ~ "Sim", cons_tutel == 2 ~ "Não",
                                                  cons_tutel == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,cons_tutel, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para o Conselho do Idoso?
sinan %>% mutate(cons_ido = as.factor(case_when(cons_ido == 1 ~ "Sim", cons_ido == 2 ~ "Não",
                                                cons_ido == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,cons_ido, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para o delegacia do Idoso?
sinan %>% mutate(deleg_idos = as.factor(case_when(deleg_idos == 1 ~ "Sim", deleg_idos == 2 ~ "Não",
                                                  deleg_idos == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,deleg_idos, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para centro de referência dos direitos Humanos?
sinan %>% mutate(dir_human = as.factor(case_when(dir_human == 1 ~ "Sim", dir_human == 2 ~ "Não",
                                                 dir_human == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,dir_human, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para o minstério público?
sinan %>% mutate(mpu = as.factor(case_when(mpu == 1 ~ "Sim", mpu == 2 ~ "Não",
                                           mpu == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,mpu, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para Delegacia Especializada de Proteção à Criança e Adolescente?
sinan %>% mutate(deleg_cria = as.factor(case_when(deleg_cria == 1 ~ "Sim", deleg_cria == 2 ~ "Não",
                                                  deleg_cria == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,deleg_cria, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para Delegacia de atendimento a mulher?
sinan %>% mutate(deleg_mulh = as.factor(case_when(deleg_mulh == 1 ~ "Sim", deleg_mulh == 2 ~ "Não",
                                                  deleg_mulh == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,deleg_mulh, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para outas Delegacias?
sinan %>% mutate(deleg = as.factor(case_when(deleg == 1 ~ "Sim", deleg == 2 ~ "Não",
                                             deleg == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,deleg, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para justiça da infência e da juventude?
sinan %>% mutate(infan_juv = as.factor(case_when(infan_juv == 1 ~ "Sim", infan_juv == 2 ~ "Não",
                                                 infan_juv == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,infan_juv, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Encaminhamento para defensoria pública?
sinan %>% mutate(defen_publ = as.factor(case_when(defen_publ == 1 ~ "Sim", defen_publ == 2 ~ "Não",
                                                  defen_publ == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,defen_publ, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Violência relacionada ao trabalho - Essa variável indica contexto da violência.
sinan %>% mutate(rel_trab = as.factor(case_when(rel_trab == 1 ~ "Sim", rel_trab == 2 ~ "Não",
                                                rel_trab == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(ano_not,rel_trab, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row")) 

#Se sim (66), foi emitida a comunicação de acidente de trabalho (CAT)
sinan %>% mutate(rel_cat = as.factor(case_when(rel_cat == 1 ~ "Sim", rel_cat == 2 ~ "Não",
                                               rel_cat == 8 ~ "Não se aplica",
                                               rel_cat == 9 ~ "Ignorado"))) -> sinan
sinan %>% tabyl(rel_trab,rel_cat, show_na = T,show_missing_levels=T) %>% adorn_totals(where = c("row","col")) %>%
  adorn_title(placement="top")

#Circunstância da lesão - Nome e código do agravo notificado segundo CID-10 CAPITULO XX 
sinan %>% filter(ag_fogo == "Sim") %>% droplevels() %>%
  tabyl(circ_lesao) %>% arrange(desc(n)) #circ_lesao com vários erros?


#Data de encerramento
sinan %>% tabyl(dt_encerra) %>% arrange(desc(n))


#Relação sexual? 
sinan %>% tabyl(ano_not,rel_sexual, show_na = T,show_missing_levels=T) 


# Tabelas LGBT ------------------------------------------------------------
library(knitr)
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2024;gc()

###Características vitima
#Raça\cor
sinan %>% filter(ano_not == year & grupo_viol!="Autoprovocada" & cs_raca!="Missing" &
                   (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(orient_sex,cs_raca,show_na = F,show_missing_levels=F) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "all") %>% adorn_pct_formatting(digits = 1,affix_sign=FALSE) %>%
  arrange(desc(Total)) %>% #ordem das linhas seguindo o atlas.
  select("orient_sex","Branco","Preto","Amarelo","Pardo","Indígena","Ignorado","Total") %>% #ordem das colunas seguindo o atlas.
  rio::export(.,"base/lgbt/base/Raça por orientação sexual da vítima.xlsx")
  #kable(caption = "Orientação Sexual x Raça/Cor",format = "simple")


#Sexo do autor	 
sinan %>% filter(ano_not == year & grupo_viol!="Autoprovocada" & 
                   (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(orient_sex,autor_sexo,show_na = F,show_missing_levels=F) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "all") %>%  adorn_pct_formatting(digits = 1,affix_sign=FALSE) %>%
  arrange(desc(Total)) %>% #ordem das linhas seguindo o atlas.
  select("orient_sex","Masculino","Feminino","Ambos os sexos","Ignorado","Total") %>% #ordem das colunas seguindo o atlas.
  rio::export(.,"base/lgbt/base/Sexo do autor da violência segundo orientação sexual da vítima.xlsx")
  #kable(caption = "Orientação Sexual x Sexo do Autor",format = "simple")

#Zona de residência - ACHO QUE ZONA SÓ EXISTE NO ARQUIVO ENVIADO PELO MS
sinan %>% tabyl(ano_not,cs_zona)


#Situação conjugal
sinan %>% filter((ano_not == year & grupo_viol!="Autoprovocada" & sit_conjug!="Missing") &
                   (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(orient_sex,sit_conjug,show_na = F,show_missing_levels=F) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "all") %>%  adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>% 
  arrange(desc(Total)) %>% #ordem linhas do atlas
  select("orient_sex","Solteiro","Casado-União consensual","Viúvo","Separado","Não se aplica","Ignorado","Total") %>% #ordem coluna atlas.
  rio::export(.,"base/lgbt/base/Situação conjugal segundo orientação sexual da vítima.xlsx")
  #kable(caption = "orientação sexual x Sitaução conjugal",format = "simple")

#sexo da vítima
sinan %>% filter((ano_not == year &  grupo_viol!="Autoprovocada") &
                   (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(orient_sex,cs_sexo,show_na = F,show_missing_levels=F) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator = "all") %>%  adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>%
  arrange(desc(Total)) %>% #ordem linhas do atlas
  select("orient_sex","F","I","M","Total") %>% #Pode conter sexo indeterminado - Verficar. #Ordem das colunas.
  rio::export(.,"base/lgbt/base/Sexo da vítima segundo orientação sexual.xlsx")
  kable(caption = "Orientação Sexual x Sexo da Vítima",format = "simple")


#Número total de casos de violência contra homossexuais e bissexuais
sinan %>% filter(grupo_viol!="Autoprovocada"  &
                   (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(ano_not,orient_sex,show_na = F,show_missing_levels=F) %>% adorn_totals(where = "col") %>%
  select("ano_not","Total","Homossexual (gay-lésbica)","Bissexual") %>% 
  kable(caption = "Total de notificações Homo e bi",format = "simple")



#Orientação sexual por faixa etária lgbtqi+
#Criando faixa etária LGBTQI+
sinan %>% mutate(fxetlgbt = as.factor( 
  case_when(nu_idade_n<=4009 ~ "0 a 9",
            nu_idade_n>=4010 & nu_idade_n<=4014 ~ "10 a 14",
            nu_idade_n>=4015 & nu_idade_n<=4019 ~ "15 a 19",
            nu_idade_n>=4020 & nu_idade_n<=4024 ~ "20 a 24",
            nu_idade_n>=4025 & nu_idade_n<=4029 ~ "25 a 29",
            nu_idade_n>=4030 & nu_idade_n<=4034 ~ "30 a 34",
            nu_idade_n>=4035 & nu_idade_n<=4039 ~ "35 a 39",
            nu_idade_n>=4040 & nu_idade_n<=4044 ~ "40 a 44",
            nu_idade_n>=4045 & nu_idade_n<=4049 ~ "45 a 49",
            nu_idade_n>=4050 & nu_idade_n<=4054 ~ "50 a 54",
            nu_idade_n>=4055  & nu_idade_n<=4059 ~ "55 a 59",
            nu_idade_n>=4060  & nu_idade_n<=4064 ~ "60 a 64",
            nu_idade_n>=4065  & nu_idade_n<=4069 ~ "65 a 69",
            nu_idade_n>=4070  & nu_idade_n<=4074 ~ "70 a 74",
            nu_idade_n>=4075  & nu_idade_n<=4079 ~ "75 a 79",
            nu_idade_n>=4080  & !is.na(nu_idade_n) ~ "80 ou mais",
            is.na(nu_idade_n) ~ "Sem informação"))) -> sinan

#Faixa etária x Orientação Sexual
sinan %>% filter(ano_not == year &
                   (fxetlgbt!="0 a 9" & fxetlgbt!="Sem informação") &
                   grupo_viol!="Autoprovocada"  &
                   (orient_sex == "Heterossexual"| orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(fxetlgbt,orient_sex,show_na = TRUE, show_missing_levels = FALSE) %>% adorn_totals(where=c("col","row")) %>% 
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>% 
  select("fxetlgbt","Heterossexual","Homossexual (gay-lésbica)","Bissexual") %>% 
  rio::export(.,"base/lgbt/base/Faixa etária x Orientação Sexual.xlsx")
 #kable(caption = "Faixa etária x Orientação Sexual",format = "simple") 



#Identidade de gênero por faixa etária lgbtqi+
sinan %>% filter(ano_not == year &
                   (ident_gen!="Ignorado" & ident_gen!="Não se aplica") &
                   (fxetlgbt!="0 a 9" & fxetlgbt!="Sem informação") &
                   grupo_viol!="Autoprovocada") %>% 
  tabyl(fxetlgbt,ident_gen,show_na = TRUE, show_missing_levels = FALSE) %>% adorn_totals(where=c("row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>% 
  select("fxetlgbt","Travesti","Transexual Mulher","Transexual Homem") %>%
  rio::export(.,"base/lgbt/base/Identidade de gênero por faixa etária lgbt.xlsx") 
  #kable(caption = "Faixa etária x Identidade de gênero",format = "simple")


#Perfil orientação sexual por raça/cor
sinan %>% filter(ano_not == year & (grupo_viol!="Autoprovocada"  & cs_raca!="Missing") &
                   (orient_sex == "Heterossexual"| orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) %>% 
  tabyl(cs_raca,orient_sex,how_na = TRUE, show_missing_levels = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>% 
  select("cs_raca","Heterossexual","Homossexual (gay-lésbica)","Bissexual","Total") %>% #ordem das colunas atlas
  arrange(match(cs_raca,c("Branco","Preto","Amarelo","Pardo","Indígena","Ignorado","Total"))) %>% #ordem das linhas atlas.
  rio::export(.,"base/lgbt/base/orientação sexual por raça.xlsx") 
  #kable(caption = "Orietação Sexual x Raça/Cor",format = "simple")



#Perfil identidade de genêro por raça\cor
sinan %>% filter(ano_not == year &
                   (grupo_viol!="Autoprovocada" & cs_raca!="Missing") & 
                   (ident_gen!="Missing" & ident_gen!="Ignorado" & ident_gen!="Não se aplica")) %>% 
  tabyl(cs_raca,ident_gen,show_na = TRUE, show_missing_levels = FALSE) %>% adorn_totals(where=c("col","row")) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1,affix_sign = FALSE) %>% 
  select("cs_raca","Travesti","Transexual Mulher","Transexual Homem","Total") %>% #ordem das colunas atlas
  arrange(match(cs_raca,c("Branco","Preto","Amarelo","Pardo","Indígena","Ignorado","Total"))) %>% #ordem das linhas atlas.
  rio::export(.,"base/lgbt/base/identidade de genêro por raça.xlsx")
  #kable(caption = "Identidade de Gênero x Raça/Cor",format = "simple")


#Número de registros de violência contra trans e travestis
sinan %>% filter(grupo_viol!="Autoprovocada" & 
                   (ident_gen!="Missing" & ident_gen!="Ignorado" & ident_gen!="Não se aplica")) %>% 
  tabyl(ano_not,ident_gen,show_na = TRUE, show_missing_levels = FALSE) %>%
  adorn_title(col_name = "Identidade de Gênero") %>% knitr::kable() 

# Gráfico de variações no mesmo município e mesma unidade LGBT -----------------
###Homossexuais 
sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2023 & orient_sex == "Homossexual (gay-lésbica)" & grupo_viol!= "Autoprovocada") %>% 
  droplevels() -> base2023

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2024 & orient_sex == "Homossexual (gay-lésbica)" & grupo_viol!= "Autoprovocada") %>% 
  droplevels() -> base2024

#Agressão por município
base2017 %>% tabyl(id_municip,show_na = T,show_missing_levels = T)%>% 
  adorn_totals("row") %>% arrange(id_municip) -> x
sum(is.na(x$id_municip)) 


#Agressão por unidade
base2019 %>% tabyl(id_unidade,show_na = T,show_missing_levels = T)  %>% 
  adorn_totals("row") %>% arrange(id_unidade) -> x
sum(is.na(x$id_unidade)) 


#Valor Bruto
((tabyl(base2018,orient_sex)/tabyl(base2017,orient_sex))-1)*100
tabyl(base2017,orient_sex)
tabyl(base2018,orient_sex)
tabyl(base2019,orient_sex)
tabyl(base2020,orient_sex)

#Seleciona municípios que constam nas duas bases - Atlerar os anos com mudanças de atlas.
base2023 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2024 %>% filter(id_municip != "") -> b2
#x <- semi_join(base2018,base2017, by = "id_municip", keep = TRUE) #Mantém todos os municípios observados em 2018 com ocorrência em 2017.
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2019 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2024 %>% filter(id_municip != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior.(Mesmos Municípios)
tabyl(base_municipios_2024, orient_sex) %>% adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)


#Criando bases com unidades de saúde comuns entre os anos.
#Seleciona unidades que constam nas duas bases - Alterar os anos a medida que o altas avançar.
base2023 %>% filter(id_unidade != "") -> b1 #Seleciona unidades de saúde conhecidas.
base2024 %>% filter(id_unidade != "") -> b2
vetor_unidade = intersect(b1$id_unidade,b2$id_unidade)
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidade)

#Unidades conhecidas (Brutos)
tabyl(base2023 %>% filter(id_unidade != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Unidades iguais as do período anterior. (Mesma Unidade)
tabyl(base_unidades_2024, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidade,base_unidades_2024,base2023,base2024)


####Bissexuais
sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2023 & orient_sex == "Bissexual" & grupo_viol!="Autoprovocada") %>%
  droplevels() -> base2023

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,orient_sex,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = lubridate::year(dt_notific), #Ano de notificação
         ano_ocor = lubridate::year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2024 & orient_sex == "Bissexual" & grupo_viol!="Autoprovocada") %>%
  droplevels() -> base2024


#Agressão por município
base2020 %>% tabyl(id_municip,show_na = T,show_missing_levels = T)%>% 
  adorn_totals("row") %>% arrange(id_municip) -> x
sum(is.na(x$id_municip)) 

#Agressão por unidade
base2019 %>% tabyl(id_unidade,show_na = T,show_missing_levels = T)  %>% 
  adorn_totals("row") %>% arrange(id_unidade) -> x
sum(is.na(x$id_unidade)) 


#Valor Bruto
((tabyl(base2018,orient_sex)/tabyl(base2017,orient_sex))-1)*100
tabyl(base2017,orient_sex)
tabyl(base2018,orient_sex)
tabyl(base2019,orient_sex)

#Seleciona municípios que constam nas duas bases - Atlerar os anos com mudanças de atlas.
base2023 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2024 %>% filter(id_municip != "") -> b2
#x <- semi_join(base2018,base2017, by = "id_municip", keep = TRUE) #Mantém todos os municípios observados em 2018 com ocorrência em 2017.
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2019 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2023 %>% filter(id_municip != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos municípios)
tabyl(base_municipios_2024, orient_sex) %>% adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)

#Criando bases com unidades de saúde comuns entre os anos.
#Seleciona unidades que constam nas duas bases - Alterar os anos a medida que o altas avançar.
base2023 %>% filter(id_municip != "") -> b1 #Seleciona municípios conhecidos.
base2024 %>% filter(id_municip != "") -> b2
vetor_unidade = intersect(b1$id_unidade,b2$id_unidade)
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidade)

#Unidades conhecidos (Brutos)
tabyl(base2024 %>% filter(id_unidade != ""),orient_sex) %>% adorn_totals(where = c("row", "col"))
#Unidades iguais ao do período anterior. (Mesma Unidade)
tabyl(base_unidades_2024, orient_sex) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidade,base_unidades_2024,base2023,base2024)


### Transsexuais e Travestis
sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,ident_gen,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2023 & (ident_gen == "Transexual Homem" |ident_gen == "Transexual Mulher" | ident_gen == "Travesti") & 
           grupo_viol!="Autoprovocada") -> base2023

sinan %>% select(dt_notific,dt_ocor,id_municip,id_unidade,ident_gen,viol_fisic:viol_espec,viol_motiv,grupo_viol) %>% 
  mutate(ano = year(dt_notific), #Ano de notificação
         ano_ocor = year(dt_ocor)) %>% #Ano de ocorrência.
  filter(ano == 2024 & (ident_gen == "Transexual Homem" |ident_gen == "Transexual Mulher" | ident_gen == "Travesti") 
         & grupo_viol!="Autoprovocada") -> base2024


#Violência Física - LGBTQI
base2023 %>% filter(viol_fisic == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(viol_fisic == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2018 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2024 %>% filter(id_municip != "" & viol_fisic == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos Municípios)
tabyl(base_municipios_2024 %>% filter(viol_fisic == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)

#Mesmo unidade - Violência Física - LGBTQI
base2023 %>% filter(viol_fisic == "Sim" & id_unidade!= "") ->b1 #Seleciona unidades com violência física.
base2024 %>% filter(viol_fisic == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona unidades que constam nas duas bases.
rm(b1,b2)
#Criando bases com unidades comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos (Brutos)
tabyl(base2024 %>% filter(id_unidade != "" & viol_fisic == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesma Unidade)
tabyl(base_unidades_2024 %>% filter(viol_fisic == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2024)#Não apagar baseano1 e baseano2. Será usado a frente.



#Violência psicológica - LGBTQI
base2023 %>% filter(viol_psico == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(viol_psico == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2019 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2024 %>% filter(id_municip != "" & viol_psico == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos municípios)
tabyl(base_municipios_2024 %>% filter(viol_psico == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)


#Mesma unidade - Violência psicológica - LGBTQI
base2023 %>% filter(viol_psico == "Sim" & id_unidade!= "") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(viol_psico == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2018,base2017, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com Unidades comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos (Brutas)
tabyl(base2024 %>% filter(id_unidade != "" & viol_psico == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Unidades iguais ao do período anterior. (Memsma unidade)
tabyl(base_unidades_2024 %>% filter(viol_psico == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2024)



#Tortura - LGBTQI 
base2023 %>% filter(viol_tort == "Sim" & id_municip!= "") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(viol_tort == "Sim" & id_municip!= "") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2019 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2024 %>% filter(id_municip != "" & viol_tort == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos municípios)
tabyl(base_municipios_2024 %>% filter(viol_tort == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)

#Mesmo Unidade - Tortura - LGBTQI 
base2023 %>% filter(viol_tort == "Sim" & id_unidade!= "") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(viol_tort == "Sim" & id_unidade!= "") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com Unidades comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidades)

#Unidades conhecidos (Brutos)
tabyl(base2024 %>% filter(id_unidade != "" & viol_tort == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesma unidade)
tabyl(base_unidades_2024 %>% filter(viol_tort == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2024)



#Outros - LGBTQI 
base2023 %>% filter(id_municip != "" & viol_outr == "Sim") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(id_municip != "" & viol_outr == "Sim") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_municipios = intersect(b1$id_municip,b2$id_municip) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com municípios comuns entre os anos.
#base_municipios_2019 <- base2018 %>% filter(id_municip %in% vetor_municipios)
base_municipios_2024 <- base2023 %>% filter(id_municip %in% vetor_municipios)

#Municípios conhecidos (Brutos)
tabyl(base2024 %>% filter(id_municip != "" & viol_outr == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Municípios iguais ao do período anterior. (Mesmos municípios)
tabyl(base_municipios_2024 %>% filter(viol_outr == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_municipios,base_municipios_2024)


##Mesmo Unidade - Outros - LGBTQI 
base2023 %>% filter(id_unidade != "" & viol_outr == "Sim") ->b1 #Seleciona municípios com violência física.
base2024 %>% filter(id_unidade != "" & viol_outr == "Sim") ->b2
#x <- semi_join(base2019,base2018, by = "id_unidade", keep = TRUE)
vetor_unidades = intersect(b1$id_unidade,b2$id_unidade) #Seleciona municípios que constam nas duas bases.
rm(b1,b2)
#Criando bases com Unidades comuns entre os anos.
#base_unidades_2019 <- base2018 %>% filter(id_unidade %in% vetor_unidades)
base_unidades_2024 <- base2023 %>% filter(id_unidade %in% vetor_unidades) 

#Unidades conhecidos (Brutos)
tabyl(base2024 %>% filter(id_unidade != "" & viol_outr == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
#Unidades iguais ao do período anterior. (Mesmas Unidades)
tabyl(base_unidades_2024 %>% filter(viol_outr == "Sim"),ident_gen) %>%  adorn_totals(where = c("row", "col"))
rm(vetor_unidades,base_unidades_2024)

rm(base2023,base2024)



# Gráfico PCds ------------------------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2024;gc()

#Notificações deficiência auditiva
sinan %>% filter(def_auditi == "Sim" & grupo_viol!="Autoprovocada") %>%
  count(ano_not, name = "Auditiva") |> 
  
  left_join(x = _, y = 
  

#Notificações deficiência Física
sinan %>% filter(def_fisica == "Sim" & grupo_viol!="Autoprovocada") %>%
  count(ano_not, name = "Física"), by = join_by("ano_not") ) |>

  left_join(x = _, y = 
  
#Notificações deficiência Intelectual
sinan %>% filter(def_mental == "Sim" & grupo_viol!="Autoprovocada") %>%
  count(ano_not, name = "Intelectual"), by = c("ano_not") ) |>
  
  left_join(x = _, y = 

#Notificações deficiência Visual
sinan %>% filter(def_visual == "Sim" & grupo_viol!="Autoprovocada") %>%
  count(ano_not, name = "Visual"), by = join_by("ano_not") ) |>
  
  pivot_longer(cols = !c(ano_not), names_to = "def", values_to = "value", names_transform = list(def = as_factor)) -> base
  

base |>
  #Gráfico
  ggplot() + 
  geom_line(aes(x = ano_not, y = value, color = def, group = def) ) +
  
  geom_point(aes(x = ano_not, y = value, color = def) ) +
  
  ggrepel::geom_text_repel(aes(x = ano_not, y = value, label = value, 
                               color = def ), seed = 747,
                           show.legend = FALSE, force = 10 ) +
  
  scale_y_continuous(breaks = seq(1000,6000,1000) ) +
  
  guides(color = guide_legend(position = "inside", nrow = 2)) +
  theme(legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),
        axis.title.x=element_text(size=5.75),axis.title.y=element_text(size=7),
        legend.text = element_text(size = 10,face="bold"),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "", y = "", color = "")

ggsave(filename ="base/pcd/figuras/evolucao_defs.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="base/pcd/figuras/evolucao_defs.eps",width = 8,height = 5,device='bmp', dpi=150)




# Tabelas PcD -------------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2024;gc()

#Cobertura municipal das pessoas com deficiência - Municípios e CNES com ao menos uma notificação
sinan %>% filter(!is.na(id_municip)|!is.na(id_unidade)) %>%
  group_by(ano_not) %>% summarise(Municípios = n_distinct(id_municip),
                                  CNES = n_distinct(id_unidade)) 

#Notificações pessoas com deficiência	
sinan %>% filter(def == "Sim" & grupo_viol!="Autoprovocada") %>%
  tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))

#Notificações deficiência auditiva
sinan %>% filter(def_auditi == "Sim" & grupo_viol!="Autoprovocada") %>%
  tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))

#Notificações deficiência Física
sinan %>% filter(def_fisica == "Sim" & grupo_viol!="Autoprovocada") %>%
  tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))


#Notificações deficiência Intelectual
sinan %>% filter(def_mental == "Sim" & grupo_viol!="Autoprovocada") %>%
  tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))


#Notificações deficiência Visual
sinan %>% filter(def_visual == "Sim" & grupo_viol!="Autoprovocada") %>%
  tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))

#TipodefxAgr-8.1
#tipo de deficiência por grupo agressor - GERAL
sinan %>% filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = "col") %>%
  arrange(desc(Total)) %>% #Ordem das linhas
  adorn_totals(where = "row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front",
           ns = tabyl(filter(sinan,ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada"),
                      tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
             adorn_totals(where = "col") %>%
             arrange(desc(Total)) %>%
             adorn_totals(where = "row")) %>% 
  #Como Fazer isso melhor?
  relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
  #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
  knitr::kable(format = "pipe")

#tipo de deficiência por grupo agressor - MULHER  
sinan %>% filter(ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = "col") %>%
  arrange(desc(Total)) %>% #Ordem das linhas
  adorn_totals(where = "row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front",
           ns = tabyl(filter(sinan,ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada"),
                      tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
             adorn_totals(where = "col") %>%
             arrange(desc(Total)) %>%
             adorn_totals(where = "row")) %>% 
  #Como Fazer isso melhor?
  relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
  #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
  knitr::kable(format = "pipe",caption = "Tipo de deficiência por grupo agressor - MULHER")

#tipo de deficiência por grupo agressor - Homem  
sinan %>% filter(ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(tipodef,grupo_viol, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = "col") %>%
  arrange(desc(Total)) %>% #Ordem das linhas
  adorn_totals(where = "row") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns(position = "front",
           ns = tabyl(filter(sinan,ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada"),
                      tipodef,grupo_viol,show_na = F, show_missing_levels = F) %>%
             adorn_totals(where = "col") %>%
             arrange(desc(Total)) %>%
             adorn_totals(where = "row")) %>% 
  #Como Fazer isso melhor?
  relocate(Misto, .before = Institucional) %>% relocate(Doméstica, .before = Comunitária) %>%
  #adorn_title(row_name = "Tipo de deficiência", col_name = "Grupo de Contexto/Autoria") %>% 
  knitr::kable(format = "pipe",caption = "Tipo de deficiência por grupo agressor - Homem")



###Grupo agressor por faixa etária -  Agrxfxet80-8.3
#Criando  Faixa etária 80+
sinan %>% mutate(fxet80 = as.factor(case_when(
  nu_idade_n<=4009 ~ "0 a 9",
  nu_idade_n>=4010 & nu_idade_n<=4019 ~ "10 a 19",
  nu_idade_n>=4020 & nu_idade_n<=4029 ~ "20 a 29",
  nu_idade_n>=4030 & nu_idade_n<=4039 ~ "30 a 39",
  nu_idade_n>=4040 & nu_idade_n<=4049 ~ "40 a 49",
  nu_idade_n>=4050 & nu_idade_n<=4059 ~ "50 a 59",
  nu_idade_n>=4060 & nu_idade_n<=4069 ~ "60 a 69",
  nu_idade_n>=4070 & nu_idade_n<=4079 ~ "70 a 79",
  nu_idade_n>=4080 & !is.na(nu_idade_n) ~    "80 ou mais"))) -> sinan

#Grupo Agresso x Faixa etária 80 - GERAL
sinan %>% filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = c("col","row")) %>%
  adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") |>
  rio::export(x= _ ,"agressor.xlsx" )
  #knitr::kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - GERAL") 

#Grupo Agresso x Faixa etária 80 - Mulher
sinan %>% filter(ano_not == year & cs_sexo == "F" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = c("col","row")) %>%
  adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") %>% 
  knitr::kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - Mulher")

#Grupo Agresso x Faixa etária 80 - Homem
sinan %>% filter(ano_not == year & cs_sexo == "M" & def=="Sim" & grupo_viol!="Autoprovocada") %>% 
  tabyl(grupo_viol,fxet80, show_na = F, show_missing_levels = F) %>%
  adorn_totals(where = c("col","row")) %>%
  adorn_percentages("row") %>% adorn_pct_formatting(digits = 2) %>%  adorn_ns(position = "front") %>%
  kable(format = "pipe",caption = "Grupo agressor x Faixa etária 80 - Homem")


###Tipo de deficiência por tipo de violência não individualizada - tipodef_t_violência-8.4
sinan %>% filter(!is.na(tipodef)) %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(tipodef) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            notificações = n()) %>% 
  arrange(desc(notificações)) %>%  #Ordem tabelas do atlas. 
  adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_Geral.xlsx")
  kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Geral") 

#tipodef_t_violência-8.4 - Mulher
sinan %>% filter(!is.na(tipodef) & cs_sexo == "F") %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(tipodef) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            notificações = n()) %>% 
  arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
  adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_mulher.xlsx")
  kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Mulher")


#tipodef_t_violência-8.4 - Homem
sinan %>% filter(!is.na(tipodef) & cs_sexo == "M") %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(tipodef) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            notificações = n()) %>% 
  arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
  adorn_totals(where = "row") %>% rio::export(x=.,"base/pcd/base/Tipo de deficiência_Tipo de violência não individualizada_homem.xlsx")
  kable(caption = "Tipo de deficiência x Tipo de violência não individualizada - Homem")


###Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Geral
library(scales)
sinan %>% filter(!is.na(fxet80)) %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(fxet80) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
  # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
  #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
  pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
  pivot_wider(names_from = fxet80,values_from = Violências) %>% 
  rio::export(x=., "base/pcd/base/t_violxfxet80.xlsx")
  kable(caption = "Tipo de Violência x Faixa etária 80 - Geral")



#Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Mulher
sinan %>% filter(!is.na(fxet80) & cs_sexo == "F") %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(fxet80) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
  # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
  #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
  pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
  pivot_wider(names_from = fxet80,values_from = Violências) %>% 
  rio::export(x=., "base/pcd/base/t_violxfxet80_mulher.xlsx")
  kable(caption = "Tipo de Violência x Faixa etária 80 - Mulher")



#Tipo de violência x faixa etária 80 - t_violência x fxet80 - 8.5 - Homem
sinan %>% filter(!is.na(fxet80) & cs_sexo == "M") %>%
  mutate(
    v_fisica = case_when(viol_fisic =="Sim" ~ 1, TRUE ~ 0),
    v_psico = case_when(viol_psico=="Sim" | viol_finan=="Sim" ~ 1, TRUE ~ 0),
    v_sexual = case_when(viol_sexu=="Sim" ~ 1, TRUE ~ 0),
    v_outro = case_when(viol_outr=="Sim" | viol_legal=="Sim" | viol_infan=="Sim" | 
                          viol_traf=="Sim" | viol_tort=="Sim" ~ 1, TRUE ~ 0),
    v_neglig = case_when(viol_negli=="Sim" ~ 1, TRUE ~ 0 )) %>%
  filter(ano_not == year & def=="Sim" & grupo_viol!="Autoprovocada") %>%
  group_by(fxet80) %>% 
  summarise(V_Física = sum(v_fisica),
            V_Psicol = sum(v_psico),
            Neglig_aband = sum(v_neglig),
            Outros = sum(v_outro),
            V_sexual = sum(v_sexual),
            Casos = n()) %>% adorn_totals(where = "row") %>% ungroup() %>%
  # mutate(r_v_fis = round(V_Física/Casos*100,2), #Porcentagens
  #        r_v_Psi = round(V_Psicol/Casos*100,2)) %>%
  pivot_longer(cols = !c("fxet80"),names_to = "variables",values_to = "Violências") %>%
  pivot_wider(names_from = fxet80,values_from = Violências) %>% 
  rio::export(x=., "base/pcd/base/t_violxfxet80_homem.xlsx") 
  kable(caption = "Tipo de Violência x Faixa etária 80 - Homem")



# Tabelas violência PcD autoprovocada -------------------------------------
# library(tidyverse)
# library(janitor)
# #Importando base
# load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
# 
# #Cobertura municipal das pessoas com deficiência - Municípios e CNES com ao menos uma notificação
# sinan %>% filter(!is.na(id_municip)|!is.na(id_unidade)) %>%
#   group_by(ano_not) %>% summarise(Municípios = n_distinct(id_municip),
#                                   CNES = n_distinct(id_unidade)) 
# 
# #Notificações pessoas com deficiência	
# sinan %>% filter(def == "Sim" & grupo_viol=="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #Notificações deficiência auditiva
# sinan %>% filter(def_auditi == "Sim" & grupo_viol=="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #Notificações deficiência Física
# sinan %>% filter(def_fisica == "Sim" & grupo_viol=="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# 
# #Notificações deficiência Intelectual
# sinan %>% filter(def_mental == "Sim" & grupo_viol=="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# 
# #Notificações deficiência Visual
# sinan %>% filter(def_visual == "Sim" & grupo_viol=="Autoprovocada") %>%
#   tabyl(ano_not,cs_sexo) %>% adorn_totals(where = c("col"))
# 
# #TipodefxAgr-8.1
# #tipo de deficiência por grupo agressor - GERAL
# sinan %>% filter(ano_not == 2021 & def=="Sim" & cs_sexo!="I" & grupo_viol=="Autoprovocada") |> 
#   mutate(grupo_viol = fct_drop(grupo_viol), cs_sexo = fct_drop(cs_sexo)) |>
#   tabyl(tipodef,cs_sexo, show_na = F, show_missing_levels = T) |>
#   adorn_totals(where = c("row","col")) |> slice(match(c("Trans. Mental","Múltipla","Física","Intelectual","Auditiva","Visual","Total"), tipodef))
# 
# #Gráfico tipo de deficiência agressão autoprovocada.
# sinan %>% filter(ano_not == 2021 & def=="Sim" & cs_sexo!="I" & grupo_viol=="Autoprovocada" & !is.na(tipodef)) |> 
#   mutate(grupo_viol = fct_drop(grupo_viol) ) |>
#   ggplot() +
#   geom_bar(aes(x= cs_sexo, fill =  tipodef)) +
#   #scale_y_continuous(breaks = seq(0,2500,250)) +
#   labs(x="",y="",fill="") 
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
# 
# #Cs_sexo por faixa etária 80 - Somente autoprovocada
# sinan %>% filter(ano_not == 2021 & def=="Sim" & cs_sexo!="I" & grupo_viol=="Autoprovocada") |> 
#   mutate(grupo_viol = fct_drop(grupo_viol), cs_sexo = fct_drop(cs_sexo)) |>
#   tabyl(cs_sexo,fxet80, show_na = F, show_missing_levels = T) |>
#   adorn_totals(where = c("row","col")) |> rio::export(x=_,"cs_sexo_x_fxt80.xlsx")
# 
# sinan %>% filter(ano_not == 2021 & def=="Sim" & cs_sexo!="I" & grupo_viol=="Autoprovocada" & !is.na(tipodef)) |> 
#   mutate(grupo_viol = fct_drop(grupo_viol) ) |>
#   ggplot() +
#   geom_bar(aes(x= fxet80, fill =  cs_sexo)) +
#   #scale_y_continuous(breaks = seq(0,2500,250)) +
#   labs(x="",y="",fill="") 
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) |> 
#   arrange(desc(notificações)) |> #Ordem tabelas do atlas.
#   adorn_totals(where = "row") |> rio::export(x=_,"tipodef_x_tipoviol.xlsx") 
# 
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) %>% 
#   arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
#   adorn_totals(where = "row") |> rio::export(x=_,"tipodef_x_tipoviol - Mulher.xlsx") 
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
#   group_by(tipodef) %>% 
#   summarise(V_Física = sum(v_fisica),
#             V_Psicol = sum(v_psico),
#             Neglig_aband = sum(v_neglig),
#             Outros = sum(v_outro),
#             V_sexual = sum(v_sexual),
#             notificações = n()) %>% 
#   arrange(desc(notificações)) %>% #Ordem tabelas do atlas.
#   adorn_totals(where = "row") |> rio::export(x=_,"tipodef_x_tipoviol - Homem.xlsx")
# 
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
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
#   pivot_wider(names_from = fxet80,values_from = Violências) |> rio::export(x=_,"tipviol_X_fx80.xlsx")
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
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
#   pivot_wider(names_from = fxet80,values_from = Violências) |> rio::export(x=_,"tipviol_X_fx80 - Mulher.xlsx")
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
#   filter(ano_not == 2021 & def=="Sim" & grupo_viol=="Autoprovocada") %>%
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
#   pivot_wider(names_from = fxet80,values_from = Violências) |> rio::export(x=_,"tipviol_X_fx80 - homem.xlsx")



# Tabelas violência contra crianças e jovens ---------------------------------------
library(tidyverse)
library(janitor)
here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2014:2024;gc()

sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year) |> 
  mutate(fxet_eca = as.factor(case_when(
    nu_idade_n<=4004 ~ "0 a 4",
    nu_idade_n>4004 & nu_idade_n<=4014 ~ "5 a 14",
    nu_idade_n>4014 & nu_idade_n<=4019 ~ "15 a 19",
    TRUE ~ NA_character_) ) ) |>
  tabyl(fxet_eca, show_na = F) |> adorn_totals(where = c("row") ) |>
  adorn_pct_formatting(digits = 1) |>
  rio::export(x=_, "base/eca/base/eca_fxet.xlsx") 


#Tabela 4.5 Brasil: Distribuição da violência contra crianças e adolescentes por local da violência e faixa etária da vítima (2011-2021)*
#Criando  Faixa crianças e jovens
sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year) |> 
  mutate(fxet_eca = as.factor(case_when(
  nu_idade_n<=4004 ~ "0 a 4 anos",
  nu_idade_n>4004 & nu_idade_n<=4014 ~ "5 a 14 anos",
  nu_idade_n>4014 & nu_idade_n<=4019 ~ "15 a 19 anos",
  TRUE ~ NA_character_)),
  #Ordem da faixa etária
  fxet_eca = fxet_eca |> fct_relevel( c("0 a 4 anos","5 a 14 anos","15 a 19 anos") ),
  
  local_ocor = local_ocor |> as.character() ) |>
  #Tabela
  tabyl(local_ocor,fxet_eca,show_na = F) |> #adorn_totals(where = c("row")) |>
  adorn_percentages("col") %>% adorn_pct_formatting(digits = 1) |> 
  #Nota de rodapé
  add_row(
  local_ocor = glue::glue("Fonte: Sinan/MS. Elaboração: Diest/Ipea e FBSP. 
  *Microdados do SINAN referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}.") ) |> 
  #Título da tabela
   adorn_title(placement = "top", row_name = "Local da Violência",
            col_name = glue::glue("Distribuição do local da violência e faixa etária da vítima ({min(year)}–{max(year)})") ) |>
  rio::export(x=_,"base/eca/base/eca_local_fxet.xlsx")
  

#Tabela 4.12 Brasil: Distribuição da violência contra crianças e adolescentes por autor da violência 
#Grupo autor da violência
sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year ) |> 
  
  mutate(
    
    fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras")),
    
    grupo_viol = grupo_viol |> fct_drop(),
    
    grupo_viol = grupo_viol |> fct_relevel("Doméstica", "Comunitária",
                                           "Institucional","Misto") ) |> 
  
  tabyl(grupo_viol,fxet_eca,show_na = T, show_missing_levels = T) |>
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front") |> 
  #Colocando vírgula no lugar de ponto.
  mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\,", ".") ),
         grupo_viol = grupo_viol |> as.character())  |>
  #Ordem das colunas na tabela.
  select("Autoria" = grupo_viol,"0 a 4 anos","5 a 14 anos","15 a 19 anos")  |>
  
  #Nota de rodapé
  add_row(
    Autoria = glue::glue("Fonte: Sinan/MS. Elaboração: Diest/Ipea e FBSP. 
  *Microdados do SINAN referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}.") ) |> 
  #Necessário para o título da tabela
  as_tibble() |>

  #Título da tabela
  adorn_title(placement = "top", row_name = "Autoria",
              col_name = glue::glue("Violência contra crianças e adolescentes, por autoria provável ({min(year)}–{max(year)})") ) |> 
  rio::export(x=_,"base/eca/base/eca_grupo_viol_fxet.xlsx")
  


#Número de violência física contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Física" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
   #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4 anos","5 a 14 anos","15 a 19 anos"), fxet_eca)) |>
  adorn_title(placement = "top", row_name = "Faixa Etária",
            col_name = glue::glue("Notificações com violência Física, por autoria faixa etária ({min(year)}–{max(year)})") ) |>
 rio::export(x=_,"base/eca/base/eca_viol_fisc_fxet.xlsx")


#Número de violência sexual contra crianças e adolescentes – Brasil 
sinan |>
  filter(t_viol == "V.Sexual" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4 anos", "5 a 14 anos", "15 a 19 anos"),fxet_eca)) |>
  adorn_title(placement = "top", row_name = "Faixa Etária",
              col_name = glue::glue("Notificações com violência Sexual, por autoria faixa etária ({min(year)}–{max(year)})") ) |>
  rio::export(x=_,"base/eca/base/eca_viol_sex_fxet.xlsx")


#Número de violência psicológica contra crianças e adolescentes
sinan |>
  filter(t_viol == "V.Psico" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4 anos", "5 a 14 anos", "15 a 19 anos"),fxet_eca)) |>
  adorn_title(placement = "top", row_name = "Faixa Etária",
              col_name = glue::glue("Notificações com violência Psicológica, por autoria faixa etária ({min(year)}–{max(year)})") ) |>
  rio::export(x=_,"base/eca/base/eca_viol_psic_fxet.xlsx")


#Número de negligência/abandono de crianças e adolescentes por faixa etária
sinan |>
  filter(t_viol == "Negligência" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> droplevels() |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4 anos", "5 a 14 anos", "15 a 19 anos"),fxet_eca)) |>
  adorn_title(placement = "top", row_name = "Faixa Etária",
              col_name = glue::glue("Notificações com negligência/abandono, por autoria faixa etária ({min(year)}–{max(year)})") ) |>
 rio::export(x=_,"base/eca/base/eca_viol_negli_fxet.xlsx")



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
ggsave(filename = "base/eca/figura/V. não letal_eca.bmp", width = 15,height = 10,dpi=250)
ggsave(filename = "base/eca/figura/V. não letal_eca.eps", width = 15,height = 10,dpi=350)


#tabela de proporção por sexo.
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras")),
    t_viol = t_viol |> as.character() ) |>
  filter(grupo_viol!="Autoprovocada" & fxet_eca != "Outras" & ano_not %in% year &
           t_viol %in% c("Negligência","V.Física","V.Psico","V.Sexual") ) |> droplevels() |>
  tabyl(t_viol,cs_sexo) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "row") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")  |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\,", ".") ) )  |>
  #Nota de rodapé
  add_row(
    t_viol = glue::glue("Fonte: Sinan/MS. Elaboração: Diest/Ipea e FBSP. 
  *Microdados do SINAN referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}.") ) |> 
  #Necessário para o título da tabela
  as_tibble() |>
  #Título da tabela
  adorn_title(placement = "top", row_name = "Tipo de Violência",
              col_name = glue::glue("Tipos de violência, por sexo da vítima ({min(year)}–{max(year)})") ) |>
rio::export(x=_,"base/eca/base/eca_t_viol_fxet.xlsx")

  
#tabela de proporção faixa etária
sinan |>
  #Criando faixa etária
  mutate(fxet_eca = case_when(
    idade<=4 ~ "0 a 4 anos",
    idade>=5 & idade<=14 ~ "5 a 14 anos",
    idade>=15 & idade<=19 ~ "15 a 19 anos",
    TRUE ~ "Outras") |> as.character() ) |>
  filter(grupo_viol!="Autoprovocada" & fxet_eca != "Outras" & ano_not %in% year &
           t_viol %in% c("Negligência","V.Física","V.Psico","V.Sexual") ) |> droplevels() |>
  tabyl(fxet_eca,t_viol) %>% adorn_totals(where = c("row","col")) %>%
  adorn_percentages(denominator  = "col") %>% adorn_pct_formatting(digits = 1) %>%
  adorn_ns(position = "front")  |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\,", ".") ) )  |>
  #Nota de rodapé
  add_row(
    fxet_eca = glue::glue("Fonte: Sinan/MS. Elaboração: Diest/Ipea e FBSP. 
  *Microdados do SINAN referentes a {max(year)} são preliminares e foram coletados em março de {max(year+2)}.") ) |> 
  #Necessário para o título da tabela
  as_tibble() |>
  #Título da tabela
  adorn_title(placement = "top", row_name = "Tipo de Violência",
              col_name = glue::glue("Tipos de violência, por sexo da vítima ({min(year)}–{max(year)})") ) |>
  rio::export(x=_,"base/eca/base/eca_fxet_t_viol.xlsx")





# Tabelas Agravos de notificações Idosos 60+ -----------------------------------------
library(tidyverse)
library(janitor)
#Importando população de idosos 
data_list <- rio::import_list("C:/Users/gabli/Dropbox/Ipea/Atlas/Pop-PNADc-60+.xlsx", 
                              which = seq(from = 2, to = 12, by = 1), 
                              #Indicar as sheets desejadas. 
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc_idoso <- do.call(bind_rows, 
                           mget(ls(pattern = "^[^.]+$"))) |> 
  #Selecionando população geral de idosos
  select(uf=UF,ano,pop=Pop)

#Removendo pnadcs, exceto pop_pnadc_idoso
rm(list = setdiff(ls(), "pop_pnadc_idoso"))

#Tratando População PNADc
pop_pnadc_idoso |> 
  mutate(uf = uf |> as_factor(),
         ano = ano |> as_factor()) |> 
  #Excluindo as regiões.
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) -> pop_pnadc_idoso
 
#Acrescentado população Brasil
pop_pnadc_idoso |> 
  summarise(pop = sum(pop), .by = ano) |>
  mutate(uf = "Brasil" |> as_factor()) |>
  bind_rows(pop_pnadc_idoso) -> pop_pnadc_idoso


#Importando base SINAN
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
year <- c(2013:2023)

#Número de agravos de notificação de violência interpessoal de pessoas de 60 anos ou mais de idade, por UF 
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol!="Autoprovocada") |> 
   #Ano de notificação na uf de residência
   count(sg_uf, ano_not, name = "notfs") |> 
  #Excluindo UFs de residência não informadadas e rename das variáveis
  filter(!is.na(sg_uf)) |>  rename(uf=sg_uf,ano=ano_not) |> tibble() -> sinan_not_ufs

                     #### ATENÇÃO ###
#Algmas notificações não indicam a UF de residência, 
#portanto realizar o agregado nacional através do somatório das UFs de residência informa valor inferior a total de notificações nacionais.  
#É necessário contabilizar o agregado nacional e inserir na base com ufs de residência
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol!="Autoprovocada") |> 
  #Ano de notificação no Brasil
  count(ano_not, name = "notfs") |> rename(ano=ano_not) |>
  mutate(uf = "Brasil" |> as.factor()) |> tibble() -> sinan_not_br

bind_rows(sinan_not_ufs,sinan_not_br) -> sinan_idoso
rm(sinan_not_ufs,sinan_not_br)

#Join da população pnadc idoso e notificações
left_join(pop_pnadc_idoso,sinan_idoso, by = c("ano","uf")) |>
  #Criando taxa de notificações
  mutate(tx_nots = round((notfs/pop)*100000,1)) -> sinan_idoso
rm(pop_pnadc_idoso)

#Tabela de agravos de notificação de violência interpessoal de pessoas de 60 anos ou mais de idade por UF – Brasil
sinan_idoso |> select(ano,uf,notfs) |>
  pivot_wider(names_from = ano, values_from = notfs) %>%
  #Variações 
  mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
       z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
       y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
  adorn_title(placement = "top",col_name = "Número de agravos de notificação de violência interpessoal idosos, por UF - Brasil") %>% 
  rio::export(.,"n_agravos_idoso.xlsx")


#Tabela taxa de agravos de notificação de violência interpessoal idosos(60+), por UF de residência - Brasil
sinan_idoso |> select(ano,uf,tx_nots) |>
  pivot_wider(names_from = ano, values_from = tx_nots) %>%
  #Variações 
  mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
         z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
         y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character)|where(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
  adorn_title(placement = "top",col_name = "Taxa de agravos de notificação de violência interpessoal idosos, por UF - Brasil") %>% 
  rio::export(.,"taxa_agravos_idoso.xlsx")

rm(sinan,sinan_idoso,year)



# Tabelas lesão autoprovocadas idosos(60+)  ----------------------------------------------------------------
library(tidyverse)
library(janitor)
#Importando população de idosos 
data_list <- rio::import_list("D:/Dropbox/Ipea/Atlas/Pop-PNADc-60+.xlsx", 
                              which = seq(from = 2, to = 12, by = 1), 
                              #Indicar as sheets desejadas. 
                              setclass = "tbl")
# Loop para extrair dataframe da list data_list e criar dataframe com o nome do  dataframe na data_list
for(i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}
rm(data_list,i)

#Aplicar bind_rows a todos os dataframes no ambiente global e juntar as pnadcs em tabela única
pop_pnadc_idoso <- do.call(bind_rows, 
                           mget(ls(pattern = "^[^.]+$"))) |> 
  #Selecionando população geral de idosos
  select(uf=UF,ano,pop=Pop)

#Removendo pnadcs, exceto pop_pnadc_idoso
rm(list = setdiff(ls(), "pop_pnadc_idoso"))

#Tratando População PNADc
pop_pnadc_idoso |> 
  mutate(uf = uf |> as_factor(),
         ano = ano |> as_factor()) |> 
  #Excluindo as regiões.
  filter(!(uf %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) -> pop_pnadc_idoso

#Acrescentado população Brasil
pop_pnadc_idoso |> 
  summarise(pop = sum(pop), .by = ano) |>
  mutate(uf = "Brasil" |> as_factor()) |>
  bind_rows(pop_pnadc_idoso) -> pop_pnadc_idoso


#Importando base SINAN
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
year <- c(2013:2023)

#Número de notificações de lesão autoprovocada. Usando metodologia do PCD
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol=="Autoprovocada") |> 
  #Ano de notificação na uf de residência
  count(sg_uf, ano_not, name = "notfs") |> 
  #Excluindo UFs de residência não informadadas e rename das variáveis
  filter(!is.na(sg_uf)) |>  rename(uf=sg_uf,ano=ano_not) |> tibble() -> sinan_not_ufs

#### ATENÇÃO ###
#Algmas notificações não indicam a UF de residência, 
#portanto realizar o agregado nacional através do somatório das UFs de residência informa valor inferior a total de notificações nacionais.  
#É necessário contabilizar o agregado nacional e inserir na base com ufs de residência
sinan %>% filter(ano_not %in% year & idade>=60 & grupo_viol=="Autoprovocada") |> 
  #Ano de notificação no Brasil
  count(ano_not, name = "notfs") |> rename(ano=ano_not) |>
  mutate(uf = "Brasil" |> as.factor()) |> tibble() -> sinan_not_br

bind_rows(sinan_not_ufs,sinan_not_br) -> sinan_idoso
rm(sinan_not_ufs,sinan_not_br)

#Join da população pnadc idoso e notificações
left_join(pop_pnadc_idoso,sinan_idoso, by = c("ano","uf")) |> 
  #Zero nas UF sem notificação
  mutate(notfs = replace_na(notfs,0),
  #Criando taxa de notificações
 tx_nots = round((notfs/pop)*100000,1)) -> sinan_idoso
rm(pop_pnadc_idoso)

#Tabela de notificação autoprovocada de idosos (60+)
sinan_idoso |> select(ano,uf,notfs) |>
  pivot_wider(names_from = ano, values_from = notfs) %>%
  #Variações 
  mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
         z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
         y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
  adorn_title(placement = "top",col_name = "Número de notificações autoprovocadas de idosos (60+), por UF - Brasil") %>% 
  rio::export(.,"n_autoprovocadas_idoso.xlsx")


#Tabela taxa de notificação violência autoprovocada de idosos (60+), por UF de residência - Brasil
sinan_idoso |> select(ano,uf,tx_nots) |>
  pivot_wider(names_from = ano, values_from = tx_nots) %>%
  #Variações 
  mutate(x = format(round(pull(((.[, ncol(.)] /.[, 2])-1)*100),digits = 1), nsmall = 1), #Variação de 10 anos.
         z = format(round(pull(((.[, ncol(.)] /.[, 11])-1)*100),digits = 1), nsmall = 1), #Variação anual
         y = format(round(pull(((.[, ncol(.)] /.[, 7])-1)*100),digits = 1), nsmall = 1)) %>% #Variação de 5 anos
  #rename_with(.,.fn = paste(colnames(.)[2],"a",colnames(.)[12], sep = " "), .cols = starts_with("x"))
  #Ordenando a linha de população seguindo a ordem do atlas.
  slice(match(c("Brasil", "Acre", "Alagoas", "Amapá", "Amazonas", "Bahia","Ceará","Distrito Federal","Espírito Santo","Goiás",
                "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí",
                "Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo",
                "Sergipe","Tocantins"),uf)) |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(.cols = where(is.character)|where(is.numeric), ~ str_replace_all(as.character(.x), "\\.", ","))) |>
  adorn_title(placement = "top",col_name = "Taxa de notificação autoprovocada idosos (60+), por UF - Brasil") %>% 
  rio::export(.,"taxa_autoprovocadas_idoso.xlsx")

rm(sinan,sinan_idoso,year)



