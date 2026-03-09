library(tidyverse)
library(janitor)




#Importando base
load("C:/Users/gabli/Desktop/r/SIM/Atlas 2025/sim_doext_homic_pred_96_23.RData")

#Homicídios por município de residência
homic_preds |>
  
  
   filter(.pred_class == "homic") |>
  
   count(ano,uf_resd)
   
 

 count(ano,codmunres, name = "n_homic") |>
  
  #Preencher anos ausentes com zero homicídios
  complete(ano, codmunres, fill = list(n_homic = 0))

   