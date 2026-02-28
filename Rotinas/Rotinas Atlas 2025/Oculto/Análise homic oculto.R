# Proporção das intenções -------------------------------------------------
library(tidyverse)
library(janitor)
#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")


sim_doext |>
  filter(intencao == "Indeterminado" & ano %in% c(2013:2023)) |>
  count(uf_resd) |>
  mutate(p_intencao = n/sum(n)*100 ) |>
  arrange(desc(p_intencao) ) |>
  mutate(p_acum = cumsum(p_intencao) ) |> view()


homic_preds |>
  filter(ano %in% c(2013:2023) ) |>
  count(.pred_class)

sim_doext |>
  filter(ano %in% c(2023) ) |>
  count(intencao_homic)



# Mortes indeterminadas ---------------------------------------------------
library(tidyverse)
library(janitor)
#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")



sim_doext |> filter( ano %in% c(2013:2023) & intencao_homic != "Outros") |>
  #Contagem de intenções
  count(ano,intencao_homic, name = "n_intencoes") |> 
  #Propoção da intenção
  mutate(p_intencao = round( (n_intencoes/sum(n_intencoes) ) * 100,1), .by = ano) |>
  #Gráfico
  ggplot() +
  geom_line(aes(x = ano, y = p_intencao, color = intencao_homic, group = intencao_homic) ) +
  geom_point(aes(x = ano, y = p_intencao, color = intencao_homic) ) +
  ggrepel::geom_text_repel(aes(x = ano, y = p_intencao, label = paste0(p_intencao, "%"), color = intencao_homic), 
                           size = 3, show.legend = FALSE) +
  #scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  scale_y_continuous(breaks = seq(5,60,5) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.15, 0.35),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "", y = "%Intencionalidades")
ggsave(filename ="intencionalidade.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="intencionalidade.eps",width = 8,height = 5, dpi=150)



# Base de MVCI, homicídio registrado, Oculto e projetado, por UF e Brasil--------------------------------------------------------
library(tidyverse)
library(janitor)
library(glue)

#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)
ano <- paste(min(year), max(year), sep = "-")

#Contagem de homicídios registrados, por ano e UF
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Homicídio" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd,intencao_homic, name = "homicidio") |>
  #Mantém variáveis utlizadas
  select(!c(intencao_homic) ) -> homic

#Contagem de mortes indeterminadas
sim_doext |> 
  #Filtro das intenções de interesse.
  filter(intencao_homic  == "Indeterminado" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd,intencao_homic, name = "mvci") |>
  #Mantém variáveis utlizadas
  select(!c(intencao_homic) ) -> mvci

#Contagem de homicídios ocultos, por ano e UF
homic_preds |> 
  #Filtrando homicídios ocultos
  filter(.pred_class == "homic" & ano %in% year) |> droplevels() |>
  count(ano,uf_resd,.pred_class, name = "ppb2_homicidio") |> 
  #Mantém variáveis utlizadas
  select(!c(.pred_class)) -> ocult

#Juntando base homicídio registrado a homicídio coulto
base <- reduce( list(homic,mvci,ocult), left_join, by = c("ano","uf_resd") )  |> 
  #Colocando zeros em UFs sem homicídio registrado (kek) ou sem homicídio oculto   
  mutate(across(where(is.numeric),~replace_na(.,0)),
         #Homicídios projetados
         homicidio_proj = homicidio + ppb2_homicidio)
rm(homic,mvci,ocult)
#Fiz várias contagens, pois a ideia foi mudando ao longa da execução.

##Importando população.
#Caminho do excel com pnadc
excel_pnadc <- "D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"

#Importação e empilhando os últimos dez anos da PNADc
pop_pnadc <- map_dfr(
  #Tail informa que desejamos os últimos dez anos da pnadc
  .x = tail(readxl::excel_sheets(excel_pnadc), 11),
  
  ~ readxl::read_excel(path = excel_pnadc, sheet = .x) ) |> 
  
  #Selecionando população geral
  select(UF,ano,pop = Pop)
rm(excel_pnadc)

#Tratamento base com população PNADc
pop_pnadc |> rename(uf_resd = UF) |>
  #Excluindo as regiões. Não utilizado
  filter(!(uf_resd %in% c("Norte","Centro Oeste","Nordeste","Sudeste","Sul"))) |>
  #Incluindo código das UFs
  mutate(cod_ibge = recode(uf_resd, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                           "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                           "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                           "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                           "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                           "Distrito Federal" = 53, "Tocantins" = 17), .before=uf_resd,
         #Transofrmando em factor variáveis desejadas
         ano = ano |> as_factor(),
         uf_resd = uf_resd |> as_factor() ) -> pop_pnadc

#Join tabela com homicídio registrado, oculto e projetado e população PNADc - UF
left_join(pop_pnadc,base, by = c("ano","uf_resd"))  -> base
rm(pop_pnadc)

#Acrescentando população, registrado, oculto e projetado Brasil à base trabalhada.
base |>
  summarise(uf_resd="Brasil" |> as.factor(),
            pop = sum(pop),
            homicidio = sum(homicidio),
            mvci = sum(mvci),
            ppb2_homicidio = sum(ppb2_homicidio),
            homicidio_proj = sum(homicidio_proj),.by=ano) |>
  #Bind_row de Brasil a base com UFs.
  bind_rows(base) -> base

#Criando homicídios projetados,taxa de homicídios ocultos e taxa de homicídios projetados
base %>%
  mutate(across(c(homicidio,mvci,ppb2_homicidio,homicidio_proj),~(./pop)*100000,.names="tx_{col}"),
         #Padrão Atlas. Taxas com uma casa decimal
         across(starts_with("tx_"),~round(.,digits = 1))) |> 
  rename(tx_oculto = tx_ppb2_homicidio) -> base


# Registrado, Oculto e Projetado ------------------------------------------
base |>
  filter(uf_resd == "Brasil" & ano %in% c(year)) |>
  select(Ano = ano, "Homicídio Registrado" = homicidio, "Homicídio Oculto" = ppb2_homicidio,
         "Homicídio Projetado" = homicidio_proj, "Tx. Homicídio Registrado" = tx_homicidio, 
         "Tx. Oculto" = tx_oculto, "Tx. Homicídio Projetado" =  tx_homicidio_proj ) |>
  #Exportando
  rio::export(x = _, "homicídios_brasil.xlsx")


# Gráfico Taxa registrada x taxa projetada --------------------------------
base |> filter(uf_resd == "Brasil") |>
    #Mantém variáveis de interesse.
    select(ano,"Tx. Homicídios Registrados" = tx_homicidio, 
         "Tx. Homicídio Projetado" =  tx_homicidio_proj) |> 
  
  pivot_longer(cols = starts_with("Tx."), names_to = "nome_taxas", values_to = "valor_taxas") |>
  
  #Gráfico
  ggplot() +
  geom_line(aes(x = ano, y = valor_taxas, colour = nome_taxas, group = nome_taxas), linewidth = 1) + 
  geom_point(aes(x = ano, y = valor_taxas, color = nome_taxas), size = 2) +
  ggrepel::geom_text_repel(aes(x = ano, y = valor_taxas,label = valor_taxas, color = nome_taxas), 
                           size = 3, show.legend = FALSE) +
  #scale_x_date(breaks = scales::breaks_width("1 year"), labels = scales::label_date("'%y") ) +
  scale_y_continuous(breaks = seq(20,40,1) ) +
  guides(color = guide_legend(position = "inside",nrow = 1) ) +
  theme(legend.position.inside = c(0.8, 0.90),
        axis.text.x=element_text(size=8),axis.text.y=element_text(size=8),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(color = "", x = "Ano", y = "Taxas de Homicídio")



# Mudanças de Posições ----------------------------------------------------


base |>
  filter(uf_resd!= "Brasil" & ano %in% c(2013:2023) ) |>
  #Colocando a sigla das UFs
mutate(suf = 
 case_match(uf_resd,
  "Acre" ~ "AC", "Alagoas" ~ "AL", "Amapá" ~ "AP", "Amazonas" ~ "AM",
  "Bahia" ~ "BA", "Ceará" ~ "CE", "Distrito Federal" ~ "DF", "Espírito Santo" ~ "ES",
  "Goiás" ~ "GO", "Maranhão" ~ "MA", "Mato Grosso" ~ "MT", "Mato Grosso do Sul" ~ "MS",
  "Minas Gerais" ~ "MG", "Pará" ~ "PA", "Paraíba" ~ "PB", "Paraná" ~ "PR",
  "Pernambuco" ~ "PE", "Piauí" ~ "PI", "Rio de Janeiro" ~ "RJ", "Rio Grande do Norte" ~ "RN",
  "Rio Grande do Sul" ~ "RS", "Rondônia" ~ "RO", "Roraima" ~ "RR", "Santa Catarina" ~ "SC",
  "São Paulo" ~ "SP", "Sergipe" ~ "SE", "Tocantins" ~ "TO",
  .default = NA_character_ )) |> 
  select(ano,suf,tx_homicidio,tx_oculto,tx_homicidio_proj) |> 
  #Pegando as dez maiores taxas estimadas em cada ano.
  slice_max(abs(tx_homicidio_proj), n = 10, by=ano) |> 
  #Painel tidy
  pivot_longer(cols=c(tx_homicidio,tx_oculto),names_to = "variable",values_to = "cases")  %>%
  mutate(suf = tidytext::reorder_within(suf,tx_homicidio_proj, ano)) %>%
  ggplot(aes(x=cases,y=suf,fill=variable)) +
  geom_col(position = position_stack(reverse = T)) +
  #Alterando nome e cor das legendas.
  scale_fill_manual(values = c("#6897bb","#ff6666"), 
                    labels = c("Tx. H. Registrada","Tx. H. Estimada")) +
  facet_wrap(~ ano, scales = "free") + 
  #Ordem das taxas dentro das colunas.
  tidytext::scale_y_reordered() + scale_x_continuous(position = "bottom") + 
  
  labs(fill="",x = "", y = "") + 
  guides(fill = guide_legend(position = "inside", nrow = 2) ) +
  theme(legend.position.inside = c(0.85,0.15),legend.text = element_text(size=10),
        legend.direction = c("horizontal"),axis.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA))
ggsave(filename ="tx_homic_alteração_posições.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="tx_homic_alteração_posições.eps",width = 8,height = 5, dpi=150)



# Mudanças na posição último ano ------------------------------------------
base |>
  filter(uf_resd!= "Brasil" & ano == 2023 ) |>
  mutate(suf = 
           case_match(uf_resd,
                      "Acre" ~ "AC", "Alagoas" ~ "AL", "Amapá" ~ "AP", "Amazonas" ~ "AM",
                      "Bahia" ~ "BA", "Ceará" ~ "CE", "Distrito Federal" ~ "DF", "Espírito Santo" ~ "ES",
                      "Goiás" ~ "GO", "Maranhão" ~ "MA", "Mato Grosso" ~ "MT", "Mato Grosso do Sul" ~ "MS",
                      "Minas Gerais" ~ "MG", "Pará" ~ "PA", "Paraíba" ~ "PB", "Paraná" ~ "PR",
                      "Pernambuco" ~ "PE", "Piauí" ~ "PI", "Rio de Janeiro" ~ "RJ", "Rio Grande do Norte" ~ "RN",
                      "Rio Grande do Sul" ~ "RS", "Rondônia" ~ "RO", "Roraima" ~ "RR", "Santa Catarina" ~ "SC",
                      "São Paulo" ~ "SP", "Sergipe" ~ "SE", "Tocantins" ~ "TO",
                      .default = NA_character_ )) |> 
  select(ano,suf,tx_homicidio,tx_oculto,tx_homicidio_proj) |> 
  #Pegando as dez maiores taxas estimadas em cada ano.
  #slice_max(abs(tx_homicidio_proj), n = 10, by=ano) |> 
  #Painel tidy
  pivot_longer(cols=c(tx_homicidio,tx_oculto),names_to = "variable",values_to = "cases")  %>%
  mutate(suf = tidytext::reorder_within(suf,tx_homicidio_proj, ano)) %>%
  ggplot(aes(x=cases,y=suf,fill=variable)) +
  geom_col(position = position_stack(reverse = T)) +
  #Alterando nome e cor das legendas.
  scale_fill_manual(values = c("#6897bb","#ff6666"), 
                    labels = c("Tx. H. Registrada","Tx. H. Estimada")) +
  facet_wrap(~ ano, scales = "free") + 
  #Ordem das taxas dentro das colunas.
  tidytext::scale_y_reordered() + scale_x_continuous(position = "bottom") + 
  
  labs(fill="",x = "", y = "") + 
  guides(fill = guide_legend(position = "inside", nrow = 2) ) +
  theme(legend.position.inside = c(0.85,0.15),legend.text = element_text(size=10),
        legend.direction = c("horizontal"),axis.text = element_text(size = 8),
        legend.background = element_rect(fill = "transparent", colour = NA))






# Gráfico de barras ocultos\mvci ------------------------------------------
library(tidyverse)
library(janitor)
library(scales)
#Importação da base de homicídios ocultos e homicídios registrados.
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")
#Período analisado.
year <- c(2013:2023)


base <- homic_preds |> 
  #Mantém anos de interesse
  filter(ano %in% c(year) ) |>
  #Contagem das categorias previstas.
  count(ano,.pred_class, name = "n") |>
  group_by(ano) %>%
  mutate(
    #Proporção das classificações
    p = n / sum(n),
    label = paste0(
      # valor absoluto com separador de milhar
      format(n, big.mark = ".", decimal.mark = ","),  
      # proporção dentro dos parênteses
      " (", percent(p, accuracy = 1), ")" ) ) %>% ungroup() 

#Adicionando total de MVCI e proporção de MVCI nas externas. 
base <-
sim_doext |>
  filter(ano %in% c(year) & intencao_homic!= "Outros") |>
  count(ano,intencao_homic, name = "n_mvci" ) |>
  mutate(p_n = round( (n_mvci/sum(n_mvci))*100,1) , .by = ano) |>
  filter(intencao_homic == "Indeterminado") |>
  #Variável com informações sobre as MVCI
  mutate(mvci = paste0(
    #Número de MVCI + Porcentagem da MVCI nas externas
    format(n_mvci, big.mark = ".", decimal.mark = "," ), " (", p_n, "%)" ) ) |>
  select(ano,n_mvci,mvci) |>
left_join(x = base, y = _, by = join_by(ano) )



#Gráfico
base |> 
  mutate(.pred_class =  case_when(.pred_class == "homic" ~ "H. Oculto",
                                  .pred_class == "n_homic" ~ "Não Homicídio") |> as_factor() ) |>
  ggplot(  ) +
  
  geom_bar(aes(x = ano, y = n, fill = fct_relevel(.pred_class,"Não Homicídio",  "H. Oculto")),
           stat = "identity", position = "stack") +
  #Linha das MVCI 
  geom_line(data = base |> filter(.pred_class == "homic"), 
            aes(x = ano, y = n_mvci, group = 1), 
            lineend = "round", linetype = "twodash" ) +
  #Ponto das MVCI
  geom_point(data = base |> filter(.pred_class == "homic"), aes(x = ano, y = n_mvci, group = 1) ) +
  #Texto das MVCI
  geom_text(aes(x = ano, y = n_mvci, label = mvci), size = 2.5, nudge_y = 1195) +
  
  #Texto das previsões.
  geom_text(aes(x = ano, y = n,  label = label),
  position = position_stack(vjust = 0.7), color = "black", size = 2.5) +
  
  #Alterando nome das legendas
  # #scale_fill_discrete(labels = c("H. Oculto", "Não Homicídio") ) +
  # scale_fill_manual( values = c("H. Oculto" = "#FF6B6B", "Não Homicídio" = "#4ECDC4"),
  #   breaks = c("Não Homicídio", "H. Oculto") ) +
  
  scale_y_continuous(breaks = seq(0,20000,2500),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  
  guides(fill = guide_legend(position = "inside", nrow = 1) ) +
  
  theme(
        axis.text.y = element_text(size = 7.5),
        #Tamanho do texto nos ticks
        axis.text = element_text(size = 7.5, face = "bold"),
        legend.position.inside = c(0.2,0.9),
        legend.text = element_text(size=8), legend.direction = c("horizontal"),
        legend.background = element_rect(fill = "transparent", colour = NA)) +
  labs(x = "", y = "Mortes Violentas por Causa Indeterminada - MVCI", fill = "")  
  
ggsave(filename ="GR3.2.bmp",width = 9,height = 6,device='bmp', dpi=160)
ggsave(filename ="GR3.2.eps",width = 9,height = 6,device=cairo_ps, dpi=160)


