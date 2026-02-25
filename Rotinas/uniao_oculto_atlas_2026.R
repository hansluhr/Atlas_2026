library(tidyverse)

#Pasta raiz
here::i_am("Rotinas/uniao_oculto_atlas_2026.R")

load(paste0(dirname(getwd()),"/bases/homic_oculto/sim_doext_homic_pred_96_23.Rdata"))

     

homic_preds |>
  filter(.pred_class == "homic") |>
  count(ano) |> view()
