library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")


# Violência Contra Mulher -------------------------------------------------

###Geral
sinan |> 
  dplyr::filter(cs_sexo == "F" & grupo_viol!="Autoprovocada") |> 
   count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  # Filtra mulheres e exclui violência autoprovocada
  dplyr::filter(cs_sexo == "F" & grupo_viol != "Autoprovocada") |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  # Violência contra mulher
  dplyr::filter(cs_sexo == "F" & grupo_viol != "Autoprovocada") |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
      .names = "{.col}_base100"  # Sufixo para as novas colunas
    ) ) -> base
  
#Elimina bases não utilizadas
rm(estab,geral,munic)


base |>
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Mulheres")
ggsave(filename ="base100_mulheres.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)


# Negros ------------------------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & cs_raca %in% c("Pardo", "Preto") ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & cs_raca %in% c("Pardo", "Preto") ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & cs_raca %in% c("Pardo", "Preto")) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Negros")
ggsave(filename ="base100_negros.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)

# Jovens 15 a 29 -------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:29) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Jovens - 15 à 29 anos")
ggsave(filename ="base100_jovens.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)




# Infantes ----------------------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

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
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(0:4) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Infante - 0 à 4 anos")
ggsave(filename ="base100_infantes0a4.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)


# Crianças 5 a 14 anos ----------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")


###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(5:14) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Crianças - 5 à 14 anos")
ggsave(filename ="base100_crianças5a14.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)



# Adolescentes 15 à 19 anos ------------------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")


###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:19) ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:19) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:19) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Adolescentes - 15 à 19 anos")
ggsave(filename ="base100_adolescenes15a19.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)






# LGBT --------------------------------------------------------------------
library(tidyverse)
library(janitor)

### Importar base 
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

###Geral
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & !orient_sex %in% c("Bissexual","Homossexual (gay-lésbica)")  ) |> 
  count(ano_not, name = "n_geral") -> geral

###Mesmo estabelecimento
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:19) ) |> 
  # Mantém apenas unidades que aparecem em 2013 (mesmo que em outros anos)
  dplyr::filter(id_unidade %in% unique(id_unidade[ano_not == 2013])) |> 
  summarise(n_estab = n(), .by = (ano_not) ) -> estab


###Mesmo Município
sinan |> 
  #Filtro categorias desejadas.
  dplyr::filter(grupo_viol!="Autoprovocada" & idade %in% c(15:19) ) |> 
  #Mantém apenas municípios que aparecem em 2013 
  dplyr::filter(id_municip %in% unique(id_municip[ano_not == 2013])) |> 
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
      ~ round( (.x / .x[ano_not == 2013]) * 100, 1),  # Divide pelo valor de 2013 e multiplica por 100
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
  
  theme(legend.title=element_blank(), plot.title = element_text(size=10),
        legend.position.inside = c(0.2,0.8),
        axis.text.x=element_text(size=8.5),axis.text.y=element_text(size=8.5),
        axis.title.x=element_text(size=7.25),axis.title.y=element_text(size=7.25),
        legend.text = element_text(size = 10,face="bold"), 
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x = "Ano da notificação", y = "Notificações", title = "Adolescentes - 15 à 19 anos")
ggsave(filename ="base100_adolescenes15a19.bmp",width = 15,height = 9,device='bmp', dpi=180)
rm(base)




