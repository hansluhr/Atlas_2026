library(tidyverse)
library(janitor)
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")


start_time <- Sys.time()
#Lista dos tipos de violência a serem iterados
viol <- c("V.Física", "V.Sexual", "V.Psico", "Negligência")
year <- 2013:2023

# Pipeline com mutate fora do loop
sinan |>
  mutate(
    fxet_eca = case_when(
      between(idade,0,4) ~ "0 a 4",
      between(idade,5,9) ~ "5 a 9",
      between(idade,10,14)  ~ "10 a 14",
      between(idade,15,19) ~ "15 a 19",
      TRUE ~ "Outras"
    ) |> as.factor() ) %>%
  {  # abre o bloco para aplicar o for
    for (i in  viol) {
      nome_arquivo <- paste0("eca_", tolower(sub("\\.", "_", i)), "_fxet.xlsx")
      
      . |>
        filter(t_viol == i, grupo_viol != "Autoprovocada", ano_not %in% year) |>
        tabyl(fxet_eca, ano_not) |>
        #Ordem das linhas
        slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"), fxet_eca)) |>
        rio::export(file = nome_arquivo)
      
      message("Arquivo salvo: ", nome_arquivo)
    }
  }

# Limpeza
rm(viol,  year)
end_time <- Sys.time()
end_time - start_time
rm(start_time,end_time)



FAzer LOOOP salvando em sheets diferentes



# Salvando na sheet -------------------------------------------------------
#Aqui o loop salva cada tabela em uma sheet.
library(tidyverse)
library(janitor)
library(openxlsx)  # Para exportar múltiplas sheets

load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

year <- 2013:2023
viol <- c("V.Física", "V.Sexual", "V.Psico", "Negligência")

# Criar uma lista vazia para armazenar as tabelas
lista_tabelas <- list()

# Pré-processamento + loop
sinan |>
  mutate(
    fxet_eca = case_when(
      between(idade, 0, 4) ~ "0 a 4",
      between(idade, 5, 9) ~ "5 a 9",
      between(idade, 10, 14) ~ "10 a 14",
      between(idade, 15, 19) ~ "15 a 19",
      TRUE ~ "Outras"
    ) |> as.factor()
  ) |>
  {function(dados) {
    for (i in viol) {
      # Gerar o nome da sheet (sem o prefixo "eca_")
      nome_sheet <- tolower(sub("\\.", "_", i))
      
      # Filtra e cria a tabela
      tabela <- dados |>
        filter(t_viol == i, grupo_viol != "Autoprovocada", ano_not %in% year) |>
        tabyl(fxet_eca, ano_not) |>
        slice(match(c("0 a 4", "5 a 9", "10 a 14", "15 a 19"), fxet_eca))
      
      # Adiciona à lista com o nome da violência como identificador
      lista_tabelas[[nome_sheet]] <<- tabela  # <<- para atribuição global
      
      message("Tabela processada: ", nome_sheet)
    }
    return(dados)  # Continua o pipeline se necessário
  }}() |>
  invisible()  # Suprime saída desnecessária

# Exportar todas as tabelas para um único Excel
write.xlsx(
  lista_tabelas,
  file = "eca_violencias_fxet.xlsx",
  asTable = TRUE,
  sheetName = names(lista_tabelas)
)

message("Arquivo Excel salvo: eca_violencias_fxet.xlsx")
rm(lista_tabelas,viol,year)













# Gráfico -----------------------------------------------------------------


#Gráfico com quadros das quatro violências
sinan |>
  
  mutate(
    #Criando faixa etária
    fxet_eca = as.factor(case_when(
    idade<=4 ~ "0 a 4",
    idade>=5 & idade <=9 ~ "5 a 9",
    idade>=10 & idade<=14 ~ "10 a 14",
    idade>=15 & idade<=19 ~ "15 a 19",
    TRUE ~ "Outras") ) |> fct_relevel("0 a 4", "5 a 9", "10 a 14", "15 a 19"),
    t_viol = fct_recode(t_viol,"V. Psicológica" = "V.Psico") ) |>
  filter(t_viol %in% c("Negligência","V.Física","V. Psicológica","V.Sexual") & fxet_eca != "Outras" &
           grupo_viol!="Autoprovocada" & ano_not %in% year ) |> 
  #contagem de notificações
  count(ano_not,fxet_eca,t_viol) |> 
  ggplot(aes(x = ano_not, y = n, color = fxet_eca, group = fxet_eca) ) +
  geom_line() + geom_point() +
  
  ggrepel::geom_text_repel(seed = 787, aes(x= ano_not, y = n, 
                         label = scales::comma(n, big.mark = ".", decimal.mark = ","), color = fxet_eca), 
                          size = 3, show.legend = FALSE, force = 2,  max.time = 10  ) + 
  
  facet_wrap(vars(t_viol), scales = "free_y") +
  
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale() ) ) +
  
  guides(color = guide_legend(position = "inside",title.position = "top") ) +
  theme(strip.text = element_text(size=10, face="bold"),legend.title = element_text(face="bold"),
        legend.text = element_text(face = "bold",size = 10),
        legend.position.inside = c(0.12, 0.4),legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = NA)) + 
  labs(x="",y="",color = "Faixa Etária")
ggsave(filename = "V. não letal_eca.bmp", width = 15,height = 10,dpi=180)
