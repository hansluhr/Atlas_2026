library(tidyverse)
library(janitor)


#Join da base recente de homicídios ocultos com a última base
#Base com homicídios ocultos utilizada no atlas
load(paste0(dirname(getwd()),"/bases/homic_oculto/homic_preds_96_23.RData"))
homic_preds_96_23 <- homic_preds
#Mantém somente data frame dos homic_preds
rm(list = setdiff(ls(),"homic_preds_96_23"));gc()


#Eu alterei o nome das variáveis com label, adicionei o prefixo (def_).
#Para realizar o bind, será necessário realizar rename das variáveis na base antiga
homic_preds_96_23 <- homic_preds_96_23 |>
  rename(def_sexo = sexo, def_racacor = racacor, def_estciv = estciv,
         def_esc = esc, local_incd = local_obito,
         codmunresd = codmunres, def_uf_resd = uf_resd, 
         def_uf_ocor = uf_ocor, .pred_Homicídio = .pred_homic) |>
  #Compatibilização de tipos
  mutate(mes = factor(mes, ordered = FALSE),
         dia = factor(dia, ordered = FALSE),
         across(.cols = c(cod_uf_resd,cod_uf_ocor), 
                .fns = ~ as_factor(.) ) )

#Importação dos homicídios ocultos de 2023
load(paste0(dirname(getwd()),"/bases/homic_oculto/homic_preds_96_24.RData"))
rm(list = setdiff(ls(),c("homic_preds_96_23","homic_preds")))

#Join base recente de homicídios ocultos e base histórica de homicídios ocultos.
homic_preds |>
  #Mantém último ano
  filter(ano == 2024) |>
  #Join com base histórica de homicídios ocultos.
  bind_rows(homic_preds_96_23) |>
  #Exclusão de variáveis não utilizadas quando estimei a base new. O ideal é considerar todas as variáveis.
  select(!c(dtobito,causabas,causa_letra,causa_num,reg_ocor,reg_resd) ) -> homic_preds 
rm(homic_preds_96_23); gc()


#Mantém os seis primeiros dígitos no código do município
homic_preds <- 
  homic_preds |>
  mutate(across(c(codmunocor, codmunres), ~ substr(., 1, 6))) 


# #Adicionar município de residência
# #Download das informações sobre municípios.
# geobr::read_municipality(year = 2022) |> as_tibble() |>
#   #Código dos municípios com 6 dígitos.
#   mutate(code_muni = code_muni |> str_sub(start = 1, end = 6),
#          #Transforma em factor     
#          across( c(code_muni, name_state, code_state), ~ as_factor(.x) ) ) |> 
#   #Excluindo variáveis não utilizadas.
#   select(!c(code_region, geom)) |> sf::st_drop_geometry(data_all) |>
#   
#   ###Adiciona informações sobre os municípios ignorados 
#   bind_rows(
#     
#     tribble(~code_muni,~name_muni,~code_state,~abbrev_state,~name_state, ~name_region,
#             "000000", "Ignorado ou exterior",    "00", "IGN", "Ignorado ou exterior", "Ignorado ou exterior",
#             "110000", "Município ignorado - RO", "11",  "RO", "Rondônia", "Norte",
#             "130000", "Município ignorado - AM", "13",  "AM", "Amazonas", "Norte",
#             "150000", "Município ignorado - PA", "15",  "PA", "Pará", "Norte",
#             "210000", "Município ignorado - MA", "21",  "MA", "Maranhão", "Nordeste",
#             "170000", "Município ignorado - TO", "17",  "TO", "Tocantins", "Norte",
#             "240000", "Município ignorado - RN", "24",  "RN", "Rio Grande do Norte", "Nordeste",
#             "260000" ,"Município ignorado - PE", "26",  "PE", "Pernambuco", "Nordeste",
#             "280000", "Município ignorado - SE", "28",  "SE", "Sergipe", "Nordeste",
#             "310000", "Município ignorado - MG", "31",  "MG", "Minas Gerais", "Sudeste",
#             "330000", "Município ignorado - RJ", "33",  "RJ", "Rio de Janeiro", "Sudeste",
#             "410000", "Município ignorado - PR", "41",  "PR", "Paraná", "Sul",
#             "430000", "Município ignorado - RS", "43",  "RS", "Rio Grande do Sul", "Sul",
#             "510000", "Município ignorado - MT", "51",  "MT", "Mato Grosso", "Centro Oeste",
#             "520000", "Município ignorado - GO", "52",  "GO", "Goiás", "Centro Oeste",
#             "120000", "Município ignorado - AC", "12",  "AC", "Acre", "Norte",      
#             "140000", "Município ignorado - RR", "14",  "RR", "Roraima", "Norte",
#             "160000", "Município ignorado - AP", "16",  "AP", "Amapá",  "Norte",
#             "220000", "Município ignorado - PI", "22",  "PI", "Piauí", "Nordeste",
#             "230000", "Município ignorado - CE", "23",  "CE", "Ceará",  "Nordeste",
#             "250000", "Município ignorado - PB", "25",  "PB", "Paraíba","Nordeste",
#             "270000", "Município ignorado - AL", "27",  "AL", "Alagoas", "Nordeste",
#             "290000", "Município ignorado - BA", "29",  "BA", "Bahia", "Nordeste",
#             "320000", "Município ignorado - ES", "32",  "ES", "Espírito Santo", "Sul",
#             "350000", "Município ignorado - SP", "35",  "SP", "São Paulo", "Sudeste", 
#             "420000", "Município ignorado - SC", "42",  "SC", "Santa Catarina", "Sul",
#             "500000", "Município ignorado - MS", "50",  "MS", "Mato Grosso do Sul", "Sul") ) |>
# #Mantém variáveis de interesse
# select(code_muni, name_muni) -> base_munics
# 
# 
# full_join(base_munics, y = homic_preds, by = join_by("code_muni" == "codmunres")) |>
#   filter(is.na(name_muni)) |> count(code_muni) |> arrange(desc(n)) |> view()
#   summarise(x = mean(is.na(name_muni)))





#Salvando base com todos os períodos
save.image(paste0(dirname(getwd()),"/bases/homic_oculto/sim_doext_homic_pred_96_24.RData") )

homic_preds |>
  count(ano,.pred_class) |> view()



