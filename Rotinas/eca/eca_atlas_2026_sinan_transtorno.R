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
  rio::export(x=_,"base/eca/base/sinan_eca_local_fxet.xlsx")


#Tabela 4.12 Brasil: Distribuição da violência contra crianças e adolescentes por autor da violência
#Grupo autor da violência
sinan %>% filter(!is.na(idade) & grupo_viol!="Autoprovocada" & ano_not %in% year ) |>
  mutate(
    fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras")),
    #Exclusão de factor não utilizado (autoprovocada)
    grupo_viol = grupo_viol |> fct_drop(),
    
    grupo_viol = grupo_viol |> fct_relevel("Doméstica", "Comunitária",
                                           "Institucional","Misto") ) |>
  tabyl(grupo_viol,fxet_eca,show_na = T, show_missing_levels = T) |>
  adorn_totals(c("row", "col")) |>
  # adorn_percentages("col") %>% adorn_pct_formatting(digits = 1) %>%
  # adorn_ns(position = "front") |>
  #Colocando vírgula no lugar de ponto.
  mutate(across(where(is.character), ~ str_replace_all(as.character(.x), "\\,", "."))) |>
  #Ordem das colunas na tabela.
  select("Autoria" = grupo_viol,"0 a 4","5 a 14","15 a 19",Total) |>  

  rio::export(x=_,"base/eca/base/sinan_eca_grupo_viol_fxet.xlsx")

#Número de violência física contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Física" & grupo_viol!="Autoprovocada" & ano_not %in% year) |>
   #Criando faixa etária
  mutate(
    fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |>  
  
  rio::export(x=_,"base/eca/base/sinan_eca_viol_fisc_fxet.xlsx")


#Número de violência sexual contra crianças e adolescentes – Brasil
sinan |>
  filter(t_viol == "V.Sexual" & grupo_viol!="Autoprovocada" & ano_not %in% year) |>
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> 
  rio::export(x=_,"base/eca/base/sinan_eca_viol_sex_fxet.xlsx")


#Número de violência psicológica contra crianças e adolescentes
sinan |>
  filter(t_viol == "V.Psico" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> 
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |> 
  
  rio::export(x=_,"base/eca/base/sinan_eca_viol_psic_fxet.xlsx")


#Número de negligência/abandono de crianças e adolescentes por faixa etária
sinan |>
  filter(t_viol == "Negligência" & grupo_viol!="Autoprovocada" & ano_not %in% year) |> 
  #Criando faixa etária
  mutate(fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade<=14 ~ "5 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras"))) |>
  #Notificações violência física por ano
  tabyl(fxet_eca,ano_not) |>
  #Ordenando as linhas da tabela
  slice(match(c("0 a 4", "5 a 14", "15 a 19"),fxet_eca)) |>
  
  rio::export(x=_,"base/eca/base/sinan_eca_viol_negli_fxet.xlsx")



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
        legend.key =  element_rect(fill = "transparent", colour = NA),
        legend.position.inside = c(0.1, 0.4),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x="",y="",color = "Faixa Etária")
ggsave(filename = "base/eca/figura/V. não letal_eca.bmp", width = 15,height = 10,dpi=250)
ggsave(filename = "base/eca/figura/V. não letal_eca.eps", width = 15,height = 10,dpi=350)


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



# Gráfico - Notificações no mesmo município, estabelecimento e geral 0 a 4 anos --------
library(tidyverse)
library(janitor)
here::i_am("Rotinas//eca/eca_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") ); gc()
#Primeiro ano do perído disponível
year <- sinan |>
  mutate(ano_not = ano_not |> as.character() |> as.integer() ) |>
  summarise(ano_min = min(ano_not, na.rm = TRUE) ) |> pull()

###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(0:4) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(0:4) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == year])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(0:4) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == year])) |> 
  summarise(n_munic = n(), .by = (ano_not) ) -> munic

#Jutando as bases
geral |>
  reduce(
    .x = list(munic,estab),
    .f = ~ left_join(.x, .y, by = "ano_not"),
    .init = _ ) |>
  mutate(
    across(
      c(n_geral, n_munic, n_estab),
      ~ round( (.x / .x[ano_not == year]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
      .names = "{.col}_base100"  # Sufixo para as novas colunas
    ) ) -> base

#Elimina bases não utilizadas
rm(estab,geral,munic); gc()

#Gráficio
base |>
  #Mantém variáveis de interesse
  select(!c(n_geral, n_munic, n_estab) ) |>
  
  rename(Geral = n_geral_base100,
         "Município" = n_munic_base100,
         "Estabelecimento" = n_estab_base100) |>
  
  #Pivot para o gráfico
  pivot_longer(cols = !c(ano_not), names_to = "unidade", values_to = "n" ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano_not = ano_not |> fct_drop() |> as.character() |> as.numeric(),
         ano_not = make_date(ano_not) )  |>
  #Gráfico
  ggplot() +
  
  geom_line(aes(x = ano_not, y = n, group = unidade, color = unidade) ) + 
  geom_point(aes(x = ano_not, y = n, color = unidade) ) +
  #Texto
  ggrepel::geom_text_repel(aes(x = ano_not, y = n, color = unidade, 
                               label = scales::number(n, big.mark = ".", decimal.mark = ",") ),
                           show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  
  guides(color = guide_legend(position = "inside", nrow = 1)) +
  
  theme(legend.title=element_blank(),
        plot.title = element_text(size=10, face = "bold"),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Infante - 0 à 4 anos")


ggsave(filename ="base/eca/figura/base100_infantes0a4.bmp",width = 15,height = 9,device='bmp', dpi=180)
ggsave(filename ="base/eca/figura/base100_infantes0a4.eps",width = 15,height = 9,device=cairo_ps, dpi=180)

rm(base)


# Gráfico - Notificações no mesmo município, estabelecimento e geral 5 a 14 anos --------
library(tidyverse)
library(janitor)
here::i_am("Rotinas/eca/eca_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") ); gc()
#Primeiro ano do perído disponível
year <- sinan |>
  mutate(ano_not = ano_not |> as.character() |> as.integer() ) |>
  summarise(ano_min = min(ano_not, na.rm = TRUE) ) |> pull()

##Notificações na idade de interesse
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Notificações nos estabelecimentos com notificações no primeiro período disponível na base.
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == year])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Notificações nos municípios com notificações no primeiro período disponível na base
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == year])) |> 
  summarise(n_munic = n(), .by = (ano_not) ) -> munic

#Jutando as bases
geral |>
  reduce(
    .x = list(munic,estab),
    .f = ~ left_join(.x, .y, by = "ano_not"),
    .init = _ ) |>
  mutate(
    across(
      c(n_geral, n_munic, n_estab),
      ~ round( (.x / .x[ano_not == year]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
      .names = "{.col}_base100"  # Sufixo para as novas colunas
    ) ) -> base

#Elimina bases não utilizadas
rm(estab,geral,munic)
gc()

#Gráficio
base |>
  #Mantém variáveis de interesse
  select(!c(n_geral, n_munic, n_estab) ) |>
  
  rename(Geral = n_geral_base100,
         "Município" = n_munic_base100,
         "Estabelecimento" = n_estab_base100) |>
  
  #Pivot para o gráfico
  pivot_longer(cols = !c(ano_not), names_to = "unidade", values_to = "n" ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano_not = ano_not |> fct_drop() |> as.character() |> as.numeric(),
         ano_not = make_date(ano_not) )  |>
  #Gráfico
  ggplot() +
  
  geom_line(aes(x = ano_not, y = n, group = unidade, color = unidade) ) + 
  geom_point(aes(x = ano_not, y = n, color = unidade) ) +
  #Texto
  ggrepel::geom_text_repel(aes(x = ano_not, y = n, color = unidade, 
                               label = scales::number(n, big.mark = ".", decimal.mark = ",") ),
                           show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  
  guides(color = guide_legend(position = "inside", nrow = 1)) +
  
  theme(legend.title=element_blank(), 
        plot.title = element_text(size=10, face = "bold"),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Crianças - 5 à 14 anos")


ggsave(filename ="base/eca/figura/base100_crianças5a14.bmp",width = 15,height = 9,device='bmp', dpi=180)
ggsave(filename ="base/eca/figura/base100_crianças5a14.eps",width = 15,height = 9,device=cairo_ps, dpi=180)


rm(base)



# Gráfico - Notificações no mesmo município, estabelecimento e geral 15 a 19 anos --------
library(tidyverse)
library(janitor)
here::i_am("Rotinas//eca/eca_atlas_2026_sinan_transtorno.R")
#Carrgando base SINAN Violência
load(paste0(dirname(getwd()),"/bases/sinan_violencia/sinan_14_24_transtorno.Rdata") ); gc()
#Primeiro ano do perído disponível
year <- sinan |>
  mutate(ano_not = ano_not |> as.character() |> as.integer() ) |>
  summarise(ano_min = min(ano_not, na.rm = TRUE) ) |> pull()

###Notificações na idade de interesse
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Notificações nos estabelecimentos com notificações no primeiro período disponível na base.
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  #Unidades que aparecem no primeiro ano disponível na base
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == year])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Notificações nos municípios com notificações no primeiro período disponível na base
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  #Municípios que aparecem no primeiro ano disponível na base
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == year])) |> 
  summarise(n_munic = n(), .by = (ano_not) ) -> munic

#Jutando as bases
geral |>
  reduce(
    .x = list(munic,estab),
    .f = ~ left_join(.x, .y, by = "ano_not"),
    .init = _ ) |>
  mutate(
    across(
      c(n_geral, n_munic, n_estab),
      ~ round( (.x / .x[ano_not == year]) * 100, 1),  #Divide pelo valor do primeiro período e multiplica por 100
      .names = "{.col}_base100"  #Identifica base 100
    ) ) -> base

#Elimina bases não utilizadas
rm(estab,geral,munic)
gc()

#Gráficio
base |>
  #Mantém variáveis de interesse
  select(!c(n_geral, n_munic, n_estab) ) |>
  
  rename(Geral = n_geral_base100,
         "Município" = n_munic_base100,
         "Estabelecimento" = n_estab_base100) |>
  
  #Pivot para o gráfico
  pivot_longer(cols = !c(ano_not), names_to = "unidade", values_to = "n" ) |>
  #Transforma ano_not em date. Será utilizado no eixo
  mutate(ano_not = ano_not |> fct_drop() |> as.character() |> as.numeric(),
         ano_not = make_date(ano_not) )  |>
  #Gráfico
  ggplot() +
  
  geom_line(aes(x = ano_not, y = n, group = unidade, color = unidade) ) + 
  geom_point(aes(x = ano_not, y = n, color = unidade) ) +
  #Texto
  ggrepel::geom_text_repel(aes(x = ano_not, y = n, color = unidade, 
                               label = scales::number(n, big.mark = ".", decimal.mark = ",") ),
                           show.legend = FALSE) +
  scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  
  guides(color = guide_legend(position = "inside", nrow = 1)) +
  
  theme(legend.title=element_blank(), 
        plot.title = element_text(size=10, face = "bold"),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10, face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Jovens - 15 à 29 anos")
ggsave(filename ="base/eca/figura/base100_jovens.bmp",width = 15,height = 9,device='bmp', dpi=180)
ggsave(filename ="base/eca/figura/base100_jovens.eps",width = 15,height = 9,device=cairo_ps, dpi=180)

rm(base)


