library(tidyverse)
library(janitor)

#Importando base de homicídios
load("C:/Users/gabli/Desktop/r/SIM/Negros/sim_doext_12_22.RData")
year <- 2022 #Vamos considerar somente 2022.


# Importando base com todos os municípios do país.  -----------------------
#Fonte: https://www.ibge.gov.br/explica/codigos-dos-municipios.php
base_munics <- readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/municipios_br.xls",sheet = "munics") |>
  select("Nome_UF", "Código Município Completo","Nome_Município") |> clean_names() |>
  rename(cod_ibge = "codigo_municipio_completo", munic_resd = nome_municipio,uf_resd = nome_uf) |>
  #No microdado do SIM. A partir de 2006 o código do município aparece com 6. Vou deixar todos os municípios em todos os anos com 6 dígitos.
  mutate(cod_ibge = substr(cod_ibge,1,6)) 

#Adicionando município Ignorado ou exterior. 
#A saúde utiliza código de município ingorado. Esses municípios não aparecem em outras bases.
munics_ign <- tribble(~cod_ibge,~munic_resd, ~uf_resd,
                      "000000", "Ignorado ou exterior", "Ignorado ou exterior",
                      "110000", "Município ignorado - RO", "Rondônia",
                      "130000", "Município ignorado - AM", "Amazonas", 
                      "150000", "Município ignorado - PA", "Pará", 
                      "210000", "Município ignorado - MA", "Maranhão",
                      "170000", "Município ignorado - TO", "Tocantins",
                      "240000", "Município ignorado - RN", "Rio Grande do Norte",
                      "260000" ,"Município ignorado - PE", "Pernambuco",
                      "280000", "Município ignorado - SE", "Sergipe",
                      "310000", "Município ignorado - MG", "Minas Gerais",
                      "330000", "Município ignorado - RJ", "Rio de Janeiro",
                      "410000", "Município ignorado - PR", "Paraná",
                      "430000", "Município ignorado - RS", "Rio Grande do Sul",
                      "510000", "Município ignorado - MT", "Mato Grosso",
                      "520000", "Município ignorado - GO", "Goiás",
                      "120000", "Município ignorado - AC", "Acre",        
                      "140000", "Município ignorado - RR", "Roraima",
                      "160000", "Município ignorado - AP", "Amapá",  
                      "220000", "Município ignorado - PI", "Piauí",
                      "230000", "Município ignorado - CE", "Ceará",  
                      "250000", "Município ignorado - PB", "Paraíba",
                      "270000", "Município ignorado - AL", "Alagoas",
                      "290000", "Município ignorado - BA", "Bahia",
                      "320000", "Município ignorado - ES", "Espírito Santo",
                      "350000", "Município ignorado - SP", "São Paulo",
                      "420000", "Município ignorado - SC", "Santa Catarina",
                      "500000", "Município ignorado - MS",  "Mato Grosso do Sul")

#Não estão incluídios os municípios que são bairros presentes no início da série.
#Por exemplo, Rio de Janeiro.

#Bind de municípios conhecidos e ignorados.
bind_rows(base_munics,munics_ign) -> base_munics
rm(munics_ign)


#Contagem de homicídios registrados em cada município.
sim_doext |> 
  #Filtro da intenção e ano de interesse.
  filter(intencao_homic  == "Homicídio" & ano %in% year) |> droplevels() |>
  #Acrescentando raçacor Negra e não negra nos microdado 
  mutate(racacor = case_match(racacor,
                              c("Parda","Preta") ~ "homic_negra",
                              c("Indigena","Branca","Amarela") ~ "homic_nao_negra",
                              .default = racacor) |> 
           #Ordenação do label de racacor
           fct_relevel("homic_negra", "homic_nao_negra", "Ignorado") ) |> 
  #Contagem de homicídios, por raça\cor nos municípios. Alguns municípios não vão registar homic.
  count(uf_resd, codmunres,racacor, name = "homic_raca", .drop = FALSE) |> 
  #Total de homicídios, por municípios Bom para fazer proporções
  add_count(codmunres, wt = homic_raca, name = "homic_geral" ) |>
  #Format wide
  pivot_wider(names_from = racacor, values_from = homic_raca) -> homic
  
#Join de homicídios registrados na base de municípios da saúde.
left_join(x = base_munics, y = homic, by = join_by("cod_ibge" == "codmunres","uf_resd") ) |>
  #Os valores são missing, pois não houve ocorrência de homic registrado em 2022 no município.
  mutate(across ( where(is.numeric),~replace_na(.,0) ) ) -> base
rm(homic,base_munics)

#População total por município
readxl::read_excel("C:/Users/gabli/Dropbox/Ipea/Atlas/Atlas Negros/pop/Tabela 9606.xlsx", skip = 6) |>
  clean_names() |> rename(pop_total = total) |>
  #Transformações
  #Na base em municípios com população zero, aparece -. Estou substituindo - por zero e transformando em numeric  
  mutate( across( c(amarela,indigena), ~ case_when(.x == "-" ~ "0", .default = .x ) |> as.numeric() ), 
  #Código do município com seis dígitos.  
         cod_ibge = substr(cod_ibge,1,6),
         pop_negra = preta + parda,
         pop_nao_negra = branca + amarela + indigena) |>
  #Mantém populações de interesse
  select(cod_ibge, pop_total,pop_negra, pop_nao_negra)  -> pop

#Join base homicídios e base população
left_join(x = base, y = pop, by = join_by("cod_ibge") ) -> base
rm(pop)

#Criando taxas.
base |>
  mutate(
    #Geral
    tx_homic = round( (homic_geral/pop_total)*100000,1),
    #Tx - Negra
    tx_homic_negra = round( (homic_negra/pop_negra)*100000,1),
    #Tx - Não Negra
    tx_homic_nao_negra = round( (homic_nao_negra/pop_nao_negra)*100000,1) ) |>
 #Exportando
 rio::export(x=_,"homic_munic_negros.xlsx")