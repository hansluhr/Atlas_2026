

library(knitr)
library(tidyverse)
library(janitor)

here::i_am("Rotinas/SINAN_transtorno_atlas_2026.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
year <- 2024;gc()


# Tabelas LGBT ------------------------------------------------------------
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



# Número de casos de violência contra homossexuais e bissexuais ----------------------------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/lgbt/lgbt_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
#Anos de interesse. Últimos dez anos.
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()



#Perfil orientação sexual por raça/cor
sinan |>
  filter(ano_not %in% year & grupo_viol!="Autoprovocada" &
  (orient_sex == "Homossexual (gay-lésbica)" | orient_sex == "Bissexual")) |> 
  
  count(ano_not,orient_sex) %>%
  
  #Acrescentando total Brasil e criando taxas
  bind_rows(. |>
              summarise(orient_sex = "Total" |> as_factor(),
                        n = sum(n),
                        .by = ano_not) ) |>  
  
  ggplot() + 
  
  geom_line(aes(x = ano_not, y = n, color = orient_sex, group = orient_sex) ) +
  
  geom_point(aes(x = ano_not, y = n, color = orient_sex) ) +
  
  ggrepel::geom_text_repel(aes(x = ano_not, y = n, 
                               color = orient_sex, 
                               label = format(n, big.mark = ".", decimal.mark = ",") ), 
                           size = 5,
                           show.legend = FALSE) +
  
  scale_y_continuous(breaks = seq(0,15000,1500) ) +
  
  #Excluir títulos das legendas.
  guides(
    color = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL),
    linewidth = guide_legend(title = NULL) ) +
  
  guides(color = guide_legend(position = "inside", nrow = 3) ) +
  
  theme(legend.position.inside = c(0.25, 0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(size = 10, face = "bold") ) +
  
  labs(x = "Ano de Notificação", y = "Número de notificações", color = "")

ggsave("base/lgbt/figuras/Número casos de violência contra homossexuais e bissexuais.bmp", 
       width = 10, height = 10, device='bmp', dpi=150)

ggsave("base/lgbt/figuras/Número casos de violência contra homossexuais e bissexuais.eps", 
       width = 10, height = 10, device='eps', dpi=150)
  
  
  

# Motivação das notificações ----------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/lgbt/lgbt_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
#Anos de interesse. Últimos dez anos.
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()



sinan |>
  filter(ano_not %in% year & grupo_viol!="Autoprovocada") |>
  tabyl(viol_motiv,ano_not, show_missing_levels = TRUE, show_na = TRUE) %>% 
  adorn_totals(where=c("col","row") ) %>%
  adorn_percentages(denominator = "col") %>%  adorn_pct_formatting(digits = 1,affix_sign = TRUE) |> 
  adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |> 
  adorn_title(row_name = "Faixa Etária", col_name = "Grupo Agressor", placement = "combined") |> view()

  #Elevado Ingorado, outros. Não é informativo.










# Notificações de violência, por identidade de gênero --------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/lgbt/lgbt_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
#Anos de interesse. Últimos dez anos.
year <- seq(as.integer(format(Sys.Date(), "%Y")) - 12, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Perfil orientação sexual por raça/cor
sinan |>
  filter(ano_not %in% year & grupo_viol!="Autoprovocada" &
           ident_gen %in% c("Transexual Homem","Transexual Mulher","Travesti") ) |> 
  
  count(ano_not,ident_gen) %>%
  
  #Acrescentando total Brasil e criando taxas
  bind_rows(. |>
              summarise(ident_gen = "Total" |> as_factor(),
                        n = sum(n),
                        .by = ano_not) ) |>  
  
  ggplot() + 
  
  geom_line(aes(x = ano_not, y = n, color = ident_gen, group = ident_gen) ) +
  
  geom_point(aes(x = ano_not, y = n, color = ident_gen) ) +
  
  ggrepel::geom_text_repel(aes(x = ano_not, y = n, 
                               color = ident_gen, 
                               label = format(n, big.mark = ".", decimal.mark = ",") ), 
                           size = 5,
                           show.legend = FALSE) +
  
  scale_y_continuous(breaks = seq(0,6000,500) ) +
  
  #Excluir títulos das legendas.
  guides(
    color = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL),
    linewidth = guide_legend(title = NULL) ) +
  
  guides(color = guide_legend(position = "inside", ncol = 1) ) +
  
  theme(legend.position.inside = c(0.25, 0.85),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.text = element_text(size = 10, face = "bold") ) +
  
  labs(x = "Ano de Notificação", y = "Número de notificações", color = "")

ggsave("base/lgbt/figuras/Notificações, por identidade de genero.bmp", 
       width = 10, height = 10, device='bmp', dpi=150)

ggsave("base/lgbt/figuras/Notificações, por identidade de genero.eps", 
       width = 10, height = 10, device='eps', dpi=150)





# Proporção de raça\cor na identidade de gênero ----------------------------------------------
library(tidyverse)
library(janitor)

here::i_am("Rotinas/lgbt/lgbt_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") )
#Anos de interesse. Últimos dez anos.
year <- as.integer(format(Sys.Date(), "%Y")) - 2


#Perfil orientação sexual por raça/cor
sinan |>
  
  #Alteração para negro e não negro
  mutate(cs_raca = case_when(cs_raca %in% c("Amarelo", "Branco", "Indígena") ~ "Não Negro",
                             cs_raca %in% c("Pardo", "Preto") ~ "Negro", .default = cs_raca) |> as_factor() ) |>
  filter(ano_not %in% year & grupo_viol!="Autoprovocada" &
           ident_gen %in% c("Transexual Homem","Transexual Mulher","Travesti") ) |> 

  count(ano_not,ident_gen, cs_raca, name = "n_ident_raca") |>
  
  add_count(ident_gen , wt = n_ident_raca, name = "n_total") |>
  #Proporção
  mutate(p_ident_raca = round( (n_ident_raca/n_total)*100,2) ) |>
  
  #Gráfico
  ggplot(aes(x = ident_gen,
             y = p_ident_raca,
             fill = cs_raca)) +
  
  geom_col(position = position_dodge(width = 0.9)) +
  
  geom_text(aes(label = sprintf("%.1f", p_ident_raca)),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  
  
  #Excluir títulos das legendas.
  guides(
    fill = guide_legend(title = NULL),
    linetype = guide_legend(title = NULL),
    linewidth = guide_legend(title = NULL) ) +
  
  labs(
    x = "Identidade de gênero",
    y = "Percentual (%)") +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "bottom")
ggsave("base/lgbt/figuras/ident_gen_raca_cor.bmp", 
       width = 10, height = 10, device='bmp', dpi=150)

ggsave("base/lgbt/figuras/ident_gen_raca_cor.eps", 
       width = 10, height = 10, device='eps', dpi=150)

  

