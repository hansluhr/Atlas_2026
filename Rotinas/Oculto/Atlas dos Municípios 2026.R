library(tidyverse)
library(janitor)

# Import de base homicídio registrado e projetado -------------------------
#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Importação base de interesse
load(paste0(dirname(getwd()),"/bases/homic_oculto/sim_doext_homic_pred_96_24.Rdata"))
#Todos os anos, até o último ano disponível
year <- seq(2000, as.integer(format(Sys.Date(), "%Y")) - 2);gc()


#Importação base de municípíos e elaboração de painel de municípios
base_munics <- 
  readxl::read_excel(paste0(dirname(getwd()),"/bases/Bases Gerais/munics.xlsx")) |>
  #Painel de municípios
  expand_grid(ano = year) |>
  #Rename para permitir o join
  rename(cod_ibge = code_muni, def_uf_resd = name_state)
  
#Contagem de homicídios registrados em cada município de residência. 
sim_doext |> 
  #Filtro da intenção de interesse.
  filter(intencao_homic  == "Homicídio" & ano %in% year) |> 
  count(ano, def_uf_resd, codmunresd, intencao_homic, name = "homic_reg") |> select(!c(intencao_homic)) |>
  mutate(ano = ano |> as.character() |> as.integer(), 
         #No microdado do SIM. A partir de 2006 o código do município aparece com 6. 
         #Vou deixar todos os municípios em todos os anos com 6 dígitos.
         codmunresd = substr(codmunresd,1,6) ) -> homic_regis

#Contagem de homicídios OCULTOS em cada município
homic_preds |>
  #Filtro das intenções de interesse.
  filter(.pred_class %in% c("homic", "Homicídio") & ano %in% year) |> 
  count(ano, def_uf_resd, codmunresd, .pred_class, name = "homic_ocult") |> select(!c(.pred_class)) |>
  mutate(ano = ano |> as.character() |> as.integer(), 
         #No microdado do SIM. A partir de 2006 o código do município aparece com 6. 
         #Vou deixar todos os municípios em todos os anos com 6 dígitos.
         codmunresd = substr(codmunresd,1,6)) -> homic_ocult

#Join de homicídios registrados e homicídios ocultos a base de municípios da saúde.
list(base_munics, homic_regis, homic_ocult) %>% 
  reduce(full_join, by = join_by ("cod_ibge" == "codmunresd","def_uf_resd","ano") ) |> 
  #Os valores são missing, pois não houve ocorrência de homic registrado ou oculto nesse ano no município.
  mutate(across ( where(is.numeric),~replace_na(.,0) ),
         #Homicídio estimado
         homic_proj = homic_reg + homic_ocult) |> 
 #Estou considerando contagem de homicídio no munci de residência. VOu renomear para deixar como munic de residência
 rename(codmunresd = cod_ibge) -> homic_munic
rm(base_munics,homic_ocult,homic_regis)

# #Importando população
# pop <-  readr::read_delim("D:/Dropbox/Ipea/Atlas/Atlas Munic 2025/bases/Pop/pop_geral.csv", 
#                          delim = ";", escape_double = FALSE, trim_ws = TRUE) |>
#   #Formato longo
#   pivot_longer(cols = !c(cod_ibge,munic), names_to = "ano", values_to = "pop", 
#                names_transform = list(ano = as.integer  ) ) |>
#   mutate(cod_ibge = cod_ibge |> as.character() )


#Importando população
load(paste0(dirname(getwd()),"/bases/populacao/ripsa/pop_ripsa.Rdata") )
pop <- pop |> 
  #População, por município e ano
  summarise(pop = sum(pop), .by = c(ano,cod_mun) ) |>
  #Código de municípios com seis dígitos
  mutate(cod_mun = str_sub(as.character(cod_mun), end = -2),
         ano = ano |> as.character() |> as.integer() )  


#Join da base de homicídios nos municípios com a população residente
left_join(x = homic_munic, y = pop,  # |> select(!c(munic))
          by = join_by("codmunresd" == "cod_mun", "ano" ) ) -> homic_munic
  #Munic 510183 foi criado em 2023. Sua população começa a ser contabilizada em 2025.
rm(pop)
  
#Check de NAs
homic_munic |>
  summarise(across(everything(), ~ mean(is.na(.)) ))
#Munic 510183 foi criado em 2023. Sua população começa a ser contabilizada em 2025.
#Missing em pop são os municípios ignorados

#Tratamento da base de homicídios por município.
homic_munic <- 
  homic_munic |>
  mutate(
  #Adicionando Capital.
  cap_resd = case_when(codmunresd %in% c(120040, 270430, 160030, 130260, 292740, 
                                        230440,
                                        530010, 320530, 520870, 211130, 510340, 	
                                        500270,310620, 150140, 250750, 410690, 
                                        261160, 221100, 330455, 240810, 431490,
                                        110020, 140010, 420540, 355030, 280030, 
                                        172100) ~ 1, .default = 0) ) 
#Tabela atlas da violência
homic_munic |>  
  mutate(across(where(is.numeric) & !c(ano,pop,cap_resd), ~ round((./pop)*100000,1), .names = "tx_{col}" ) ) |> 
  rio::export(x=_,"base/oculto/munic/homic_munics.xlsx")
  
#Mantém municípios com pop superior a 100k
homic_munic |>  
  filter(pop >= 100000) |> 
  #Criação das taxas
  mutate(across(starts_with("homic_"),  ~ round((./pop)*100000,1), .names = "tx_{col}" ) ) |>
  #Ordem alfabética
  arrange(def_uf_resd) |> rio::export(x=_,"base/oculto/munic/Homic_oculto_munics100k.xlsx")

 
# Homicídios acumulados por município -------------------------------------------
library(tidyverse)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  rename(def_munic_resd = name_muni) |>
  
#Proporção acumulada de municípios e homicídios por ano
    #Mantém variáveis utilizadas
    select(ano,codmunresd,def_munic_resd,homic_proj) |>
    
    group_by(ano) %>%
    #Ordenando municípios mais violentos, por ano.
    arrange(ano, desc(homic_proj)) |>
    
    mutate(
      #Nº de municípios por ano
      n_municipios = n(),  
      municipio_ordem = row_number(),
      #Proporção de municípios (eixo X)
      prop_municipios = municipio_ordem / n_municipios,
      #Proporção acumulada de homicídios (eixo Y)
      prop_homic_acumulada = cumsum(homic_proj) / sum(homic_proj) ) |>
    ungroup() -> base
  
#Gráfico da Curva de Lorenz por ano
base |>  
  
    #Mantém somente 10% dos municípios
    filter(prop_municipios <= 0.10 & 
    #Sequência bianual dos últimos dez anos         
    ano %in% seq(as.integer( format(Sys.Date(), "%Y") ) -12 ,    as.integer(format(Sys.Date(), "%Y")) - 2, 2)  ) |>
    #Gráfico
    ggplot() +
  
    geom_hline(yintercept = 0.5, linetype="dashed", 
               color = "red", linewidth = 0.5) +
  
    geom_line(aes(x = prop_municipios, y = prop_homic_acumulada, color = factor(ano)),
              linewidth = 0.7, alpha = 0.8) +
  
    #geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +  # Linha de igualdade
  
    scale_x_continuous(labels = scales::percent, breaks = seq(0,0.1, by = 0.005)) +
  
    scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.9, by = 0.05)) +
  
    labs(
      #title = "Curva de Lorenz: Desigualdade na Distribuição de Homicídios por Município (2013-2023)",
      x = "Proporção Acumulada de Municípios (Ordenados por Homicídios)",
      y = "Proporção Acumulada de Homicídios", 
      color = "" ) +
  
  theme( legend.background = element_rect(fill = "transparent", color = NA),  # Fundo transparente
         legend.key = element_rect(fill = "transparent", color = NA),         # Área dos símbolos tra
      legend.position = "bottom",
      panel.grid.minor = element_blank() ) +
  
    guides(color = guide_legend(nrow = 1, override.aes = list(alpha = 1) ) )
ggsave(filename ="base/munic/figuras/Curva de Lorenz.bmp",width = 9,height = 6,device='bmp', dpi=160)
ggsave(filename ="base/munic/figuras/Curva de Lorenz.eps",width = 9,height = 6,device=cairo_ps, dpi=160)

  
base |>  
  #Mantém somente 10% dos municípios
  filter(prop_municipios <= 0.10 & 
  #Sequência bianual dos últimos dez anos         
  ano %in% seq(as.integer( format(Sys.Date(), "%Y") ) -12 ,  as.integer(format(Sys.Date(), "%Y")) - 2, 2) ) |>
  rio::export(x = _, "base/munic/base/curva_lorenz.xlsx")


# GR.1 - Capitais brasileiras Barras ---------------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  #Último ano disponível
  filter(ano == as.integer(format(Sys.Date(), "%Y")) - 2 ) |>
  #Mantém variáveis de interesse. Necessário para criar o total Brasil
  select(ano, munic_resd = name_muni  ,pop ,homic_reg,homic_ocult,homic_proj,cap_resd) %>% 
  
  bind_rows(. |>
              summarise(munic_resd="Brasil" |> as.factor(),
                        pop = sum(pop, na.rm = TRUE), #Retirar população de municípios missing.
                        homic_reg = sum(homic_reg),
                        homic_ocult = sum(homic_ocult),
                        homic_proj = sum(homic_proj), .by=ano) ) |> 
  
  #Mantém Brasil e Capitais
  filter(munic_resd == "Brasil" | cap_resd == 1) |>
  
  #Exclusão das variáveis ano e capitais
  select(!c(ano,cap_resd) )  |>
  
  #Criando taxas de interesse.
  mutate(across(where(is.numeric) & !c(pop), ~ round((./pop)*100000,1), .names = "tx_{col}" ) )  |>
  
  #Mantém variáveis de interesse
  select(munic_resd,tx_homic_reg, tx_homic_ocult, tx_homic_proj) |>
  #Formato long
  pivot_longer(cols = !c(munic_resd,tx_homic_proj),
             names_to = "variable", values_to = "cases")  |>
  #Ordenando o Gráfico.
  mutate(munic_resd = fct_reorder(munic_resd,tx_homic_proj) ) |>
  #Gráfico
  ggplot() +
  
  geom_col(aes(x=cases,y=munic_resd,fill=variable),
           position = position_stack(reverse = F)) +
  geom_text(data = . %>% filter(variable == "tx_homic_reg"),
            aes( x = tx_homic_proj, y = munic_resd, label = tx_homic_proj ),
            size = 3.5, nudge_x = 1.5) +
  #Alterando nome e cor das legendas.
  scale_fill_manual(values = c("#6897bb","#ff6666"), 
                    labels = c("Tx. H. Estimada","Tx. H. Registrada")) +
  scale_x_continuous(breaks = seq(0,80,5)) +   
  labs(fill="",x = "", y = "") + 
  guides(fill = guide_legend(position = "inside", nrow = 2) ) +
  theme(axis.text.y = element_text(size = 7.5, face="bold", colour = "black"),
        legend.position.inside = c(0.85,0.15),legend.text = element_text(size=10),
        legend.direction = c("horizontal"),axis.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA))

ggsave(filename ="base/munic/figuras/GR1.bmp",width = 9,height = 6,device='bmp', dpi=160)
ggsave(filename ="base/munic/figuras/GR1.eps",width = 9,height = 6,device=cairo_ps, dpi=160)



# Gr - N Projetado Capitais linha -----------------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  #Mantém somente as capitais
  filter(cap_resd == 1) |>
  #Mantém variáveis de interesse. 
  select(ano, def_uf_resd, munic_resd = name_muni, homic_proj)  |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
  #Região Norte
  def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
  #Região Nordeste
  def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
  #Região Centro-Oeste
  def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
  #Região Sudeste
  def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
  .default = "Sul") |> as_factor() ) |>
  #Formato wider
  pivot_wider(names_from = ano, values_from = homic_proj) |>

  rio::export(x = _, "base/munic/base/n_homic_proj_cap.xlsx")


# Tabela 1 Taxa por tamanho da população ----------------------------------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Último ano disponível
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  #Mantém ano de interesse
  filter(ano == year) |>
  #Mantém variáveis de interesse.
  select(munic_resd = name_muni , pop, tx_homic_proj) |>
  #Municípios por tamanho da população
  mutate(g_munic = case_when(pop <= 100000 ~ "Pequeno", #Inferior a 100k
                             pop > 100000 & pop <= 500000 ~ "Médio",
                             pop > 500000 ~ "Grande", 
                             .default = "Município ignorado" ) ) |>
  filter(g_munic != "Município ignorado") |> 
  #Tabela considerando o agrupamento por municípios.
  summarise(quantidade = n(), #Quantidade de municípios
            média = mean(tx_homic_proj),
            mediana = median(tx_homic_proj),
            dp = sd(tx_homic_proj),
            mínimo = min(tx_homic_proj),
            máximo = max(tx_homic_proj), .by = g_munic) -> tab1
  

readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  rename(munic_resd = name_muni) |>
  #Mantém ano de interesse e exclusão de municípios ignorados e exterior.
  filter(ano == year & !str_detect(munic_resd, "ignorado|exterior") ) |> 
  #Mantém variáveis de interesse.
  select(munic_resd,pop,tx_homic_proj) |>
  
  filter_out(is.na(tx_homic_proj)) |> 
  
  #Tabela considerando o agrupamento por municípios.
  summarise(g_munic = "Todos",
            quantidade = n(),
            média = mean(tx_homic_proj),
            mediana = median(tx_homic_proj),
            dp = sd(tx_homic_proj),
            mínimo = min(tx_homic_proj),
            máximo = max(tx_homic_proj) ) -> tab2

bind_rows(tab1, tab2) |>
  rename(Grupo = g_munic) |>
  #Exportando
  rio::export(x = _, "base/munic/base/tab1_grupos_munics.xlsx")
rm(tab1,tab2) 
 



# Tab 2 - homicídios estimados, pop > 100k --------------------------------
library(tidyverse)
library(janitor)
#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Último ano disponível
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |>
  rename(munic_resd = name_muni) |>
  #Mantém ano de interesse e exclusão de municípios ignorados e exterior.
  filter(ano == year & !str_detect(munic_resd, "ignorado|exterior") & pop >= 100000  )  |>
  #Ordenação decrescenta na taxa estimada
  arrange(desc(tx_homic_proj) ) |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
  #Região Norte
  def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
  #Região Nordeste
  def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
  #Região Centro-Oeste
  def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
  #Região Sudeste
  def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
  .default = "Sul") |> as_factor(),
  n  = row_number() ) |>
  #Mantém Variáveis de interesse
  select(n, munic_resd, def_uf_resd, reg_resd, pop, homic_reg, homic_ocult, homic_proj, tx_homic_proj) |>  
  #Exportando
  rio::export(x = _, "base/munic/base/homic_munic100k.xlsx")



# Tab 3 - Homicído estimado capitais --------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Último ano disponível
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") ) |> 
  #Mantém ano de interesse e capitais
  filter(ano == year & cap_resd == 1) |>
  #Ordenação decrescenta na taxa estimada
  arrange(desc(tx_homic_proj) ) |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
    #Região Norte
    def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
    .default = "Sul") |> as_factor(),
    n  = row_number() ) |>
  #Mantém Variáveis de interesse
  select(n, munic_resd = name_muni, def_uf_resd, reg_resd, pop, homic_reg, homic_ocult, homic_proj, tx_homic_proj)  |>
  #Exportando
  rio::export(x = _, "base/munic/base/homic_munic_capital.xlsx")
 

# Tab 4 - Homicídios estimados nas capitais brasileiras -------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Último ano disponível
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") )  |>
  #Mantém somente capitais
  filter(cap_resd == 1) |>
  #Seleciona Variáveis de interesse
  select(ano,def_uf_resd,munic_resd = name_muni,homic_proj) |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
  #Região Norte
  def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
  #Região Nordeste
  def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
  #Região Centro-Oeste
  def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
  #Região Sudeste
  def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
  .default = "Sul") |> as_factor() ) |>
  #Formato wide
  pivot_wider(names_from = ano, values_from = homic_proj)  |>
  #Ordenação das variáveis
  dplyr::relocate(munic_resd, .before = def_uf_resd) |>
  #Exportação
  rio::export(x = _, "base/munic/base/tab3_n_homic_proj_capitais.xlsx")



# Tab5 - Tx. homic estimado capitais --------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Último ano disponível
year <- as.integer(format(Sys.Date(), "%Y")) - 2

#Importando base dos homicídios no município de residência
readxl::read_excel(here::here("base","oculto","munic","homic_munics.xlsx") )  |> 
  #Mantém somente capitais
  filter(cap_resd == 1) |>
  #Seleciona Variáveis de interesse
  select(ano,def_uf_resd,munic_resd = name_muni,tx_homic_proj) |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
    #Região Norte
    def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
    .default = "Sul") |> as_factor() ) |>
  #Formato wide
  pivot_wider(names_from = ano, values_from = tx_homic_proj) |>
  #Ordenação das variáveis
  dplyr::relocate(munic_resd, .before = def_uf_resd)  |>
  #Exportação
  rio::export(x = _, "base/munic/base/tab5_tx_homic_proj_capitais.xlsx")



# Gr4 - Tx. Estimada Regiões ---------------------------------------------
library(tidyverse)
library(janitor)

#Pasta Raiz
here::i_am("Rotinas/Oculto/Atlas dos Municípios 2026.R")
#Fazer loop alterando a regiãode interesse.


reg <- c("Sul", "Sudeste", "Norte", "Nordeste", "Centro Oeste")
#Loop 
 for (i in reg) {
  # nome_arquivo <- paste0("tx_proj_",tolower(sub("\\.", "_", i)), ".bmp")
  #Importando base
p <-  readxl::read_excel(here::here("base","oculto","base","tx_homicidio_estimado_uf_br.xlsx"), 
                         sheet = "Plan1" )  |>
  #Mantém colunas de interesse.
  select(def_uf_resd,2:12) |> 
  #Exlusão da linha Brasil   
  filter(def_uf_resd!="Brasil") |>
  #Acrescentar região de residência
  mutate(reg_resd = case_when(
    #Região Norte
    def_uf_resd %in% c("Acre","Amapá","Amazonas","Pará","Rondônia","Roraima", "Tocantins") ~ "Norte",
    #Região Nordeste
    def_uf_resd %in% c("Alagoas","Bahia","Ceará","Maranhão","Paraíba","Pernambuco","Piauí","Rio Grande do Norte","Sergipe") ~ "Nordeste",
    #Região Centro-Oeste
    def_uf_resd %in% c("Goiás","Mato Grosso", "Mato Grosso do Sul","Distrito Federal") ~ "Centro Oeste",
    #Região Sudeste
    def_uf_resd %in% c("Rio de Janeiro","São Paulo","Espírito Santo","Minas Gerais") ~ "Sudeste", 
    .default = "Sul") |> as_factor() ) |>
  #Formato wide
  pivot_longer(cols = !c(def_uf_resd,reg_resd), names_to = "ano",
               values_to = "tx", names_transform =  list(ano = as.numeric) ) |> 
  # #Mantém região de interesse.
  filter(reg_resd == i) |> 
  # #Gráfico
  ggplot( aes(x = ano, y = tx, group = def_uf_resd, color = def_uf_resd, linetype = def_uf_resd)  ) +
  geom_line() + geom_point() +
  #scale_y_continuous(breaks = seq(20,90,5) ) +
  #scale_x_continuous(breaks = seq(2013,2023,1) ) +
  labs(x = "", y = "Tx. Homicídio Estimado", color = "", linetype = "") +
  theme(axis.title.y = element_text(size = 8),
        axis.text.y = element_text(face = "bold"), axis.text.x = element_text(face="bold"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.position = "bottom", panel.grid.minor = element_blank() ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(alpha = 1) ) )
 print(p)
 
ggsave(filename = paste0("GR.tx_proj_", tolower(i), ".bmp") ,width = 9,height = 6,device='bmp', dpi=160)

ggsave(filename = paste0("GR.tx_proj_", tolower(i), ".eps"),width = 9,height = 6,device=cairo_ps, dpi=160)

 }
rm(p,i,reg)





