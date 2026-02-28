library(tidyverse)
library(janitor)

#Rotina para pedido posterior a elaboração da base antiga de PCD.
#Na base antiga (transtorno) a identificação do grupo_viol é diferente da rotina no grupo novo.
#Por exemplo, na rotina antiga não consta a individualização de variáveis. Por exemplo, o provável autor.  
#Então vou importar a base e criar as variáveis indviduas. 

#Importando base
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")

                 PEGAR A ROTINA DE MULHERES E REFAZER!!!
                   sinan_base_nova_mulher_atlas_2025

# Provável autor e reincidência ------------------------------------------------------------
#Vairável com o(s) provávei(s) autore(s) da agressão:
capturar_nomes_sim <- function(x) {
  nomes_sim <- names(x)[x == "Sim"]
  if (length(nomes_sim) == 0) {
    nomes_sim <- "Não/Ignorado/Missing"
  } else {
    nomes_sim <- paste(nomes_sim, collapse = ", ")
  }
  return(nomes_sim)
}

# Adicionar nova variável que captura os nomes das violências que atendem a condição "Sim"
sinan |> 
  select(rel_pai,  rel_mae,  rel_pad, rel_conj, rel_excon, rel_namo, rel_exnam, rel_filho, rel_desco, 
         rel_irmao, rel_conhec, rel_cuida, rel_patrao, rel_inst, rel_pol, rel_propri, rel_outros, rel_mad ) %>%  
  #Aplica a função caputrar_nomes_sim as colunas de violência
  mutate(nome_rel = apply(., 1, capturar_nomes_sim) |> as_factor() ) |> select(nome_rel) |>
  bind_cols(sinan) -> sinan
rm(capturar_nomes_sim)    
gc()


#Provável autor, por aconteceu outra vez - coluna
sinan |> 
  dplyr::filter(def=="Sim" & grupo_viol == "Doméstica" & cs_sexo == "M") |> droplevels() |>
  mutate(
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
      #Colocar primeira letra da string maiúscula
      stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros
    nome_rel = nome_rel |> fct_lump(prop = 0.0045, other_level = "Outras combinações de autoria") ) |>
  
  tabyl(nome_rel,out_vezes)  |> adorn_totals(where = c("row","col") ) %>%
  adorn_percentages(denominator = "col") %>% adorn_pct_formatting(digits = 1) |>
  #adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |>
  #Exportando.
  rio::export(x = _, "autor_reincd_col.xlsx")

#Provável autor, por aconteceu outra vez - linha
sinan |> 
  dplyr::filter(def=="Sim" & grupo_viol == "Doméstica" & cs_sexo == "M") |> droplevels() |>
  mutate( 
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
    str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
    #Colocar primeira letra da string maiúscula
    stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.0045, other_level = "Outras combinações de autoria") ) |>
  
  tabyl(nome_rel,out_vezes)  |> adorn_totals(where = c("row","col") ) %>%
  adorn_percentages(denominator = "row") %>% adorn_pct_formatting(digits = 1) |>
  #adorn_ns(position = "front", format_func = function(x) { format(x, big.mark = ".", decimal.mark = ",") }) |>
  #Exportando.
  rio::export(x = _, "autor_reincd_row.xlsx")

# Provável autor da violência doméstica e idade --------------------------------------------------
sinan |> 
  
  dplyr::filter(def=="Sim" & grupo_viol == "Doméstica" & cs_sexo == "M") |> droplevels() |> 
  
  mutate(
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
      #Colocar primeira letra da string maiúscula
      stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.0045, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) )  |>
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE)  |> 
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) + 
  geom_point( aes(x = idade, y = p_nome_rel, color = nome_rel ) ) +
  
  #Apontações de algumas autoria.
  #Mãe
  annotate("text", x = 0, y = 47, label = "Mãe", size = 2.5, alpha = 0.6) +
  #Namorado
  annotate("text", x = 12.5, y = 40, label = "Namorado", size = 2.5, alpha = 0.6, fontface =2) +
  #Pai
  annotate("text", x = 0, y = 15, label = "Pai", size = 2.5, alpha = 0.6, fontface =2) +
  #Padrasto
  annotate("text", x  = 10,  y = 30, label = "Padrasto", size = 2.5, alpha = 0.6, fontface =2) +
  #Ex-Cônjuge
  annotate("text", x  = 33,  y = 29, label = "Ex-Cônjuge", size = 2.5, alpha = 0.6, fontface =2) +
  #Côjuge
  annotate("text", x  = 33,  y = 58, label = "Cônjuge", size = 2.5, alpha = 0.6, fontface =2) +
  #Filho
  annotate("text", x  = 65,  y = 58, label = "Filho(a)", size = 2.5, alpha = 0.6, fontface =2) +
  
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.43, 0.93), legend.text = element_text(size = 10),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "%de provável autor na idade.", color = "", title = "") 
ggsave(filename ="provável_autor_idade.bmp",width = 9,height = 5,device='bmp', dpi=150)
#ggsave(filename ="provável_autor_idade.eps",width = 8,height = 5, dpi=150)


#Tabela idade e provável autor.
sinan |> 

  dplyr::filter(def=="Sim" & grupo_viol == "Doméstica" & cs_sexo == "M") |> droplevels() |> 
  
  mutate(
    #Removendo rel_ do início da string.
    #Substituição de mae, por Mãe
    nome_rel = nome_rel |> 
      str_replace_all( c("(?i)rel_" = "", "\\bmae\\b" = "mãe",
                         "filho" = "filho(a)", "irmao" = "irmã(ão)") ) |> 
      #Colocar primeira letra da string maiúscula
      stringr::str_to_sentence(),
    
    #Transforma contagens pouco frequentes na categoria outros   
    nome_rel = nome_rel |> fct_lump(prop = 0.025, other_level = "Outros"), 
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) )  |>
  
  #Contagem de tipo de violência por idade
  count(idade,nome_rel, name = "n", .drop = FALSE)  |> 
  
  mutate(
    #Proporção da violência na idade
    p_nome_rel = round( ( n/sum(n) )*100,1) , .by = c(idade) ) |>
  
  pivot_wider(names_from = nome_rel, values_from = c(n,p_nome_rel) ) |>
  
  rio::export(x = _, "provável_autor_idade.xlsx")




# Tipo de violência -------------------------------------------------------
#USA a BASE ANTIGA
#Proporção do tipo de violência no clico de vida.
sinan |> 
  filter(def=="Sim" & grupo_viol == "Doméstica" & cs_sexo == "M" &
         !is.na(idade) & !is.na(t_viol) ) |> droplevels() |>
  mutate(
    #Transforma idade superior a 80 para 80.  
    idade = case_when(idade >= 80 ~ 80, .default = idade) ) |> 
  #Contagem de tipo de violência por idade
  count(idade,t_viol, name = "n", .drop = FALSE) |>
  mutate(
    #Proporção da violência na idade
    p_t_viol = round( ( n/sum(n) )*100,1), .by = c(idade) ) |>  #rio::export(x = _, "t_viol_idade.xlsx")
  #Gráfico
  ggplot() +
  geom_line( aes(x = idade, y = p_t_viol, color = t_viol ) ) + geom_point( aes(x = idade, y = p_t_viol, color = t_viol ) ) +
  scale_x_continuous(breaks = seq(0,80,2) ) + scale_y_continuous(breaks = seq(0,80,5), labels = scales::label_percent(scale = 1) ) +
  guides(color = guide_legend(position = "inside",nrow = 2) ) +
  theme(legend.position.inside = c(0.75, 0.93),
        axis.text.x=element_text(size=6.5),axis.text.y=element_text(size=6.5),
        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8),
        plot.title = element_text(size=9),
        legend.key = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA) ) +
  labs(x="Idade", y = "%de violência na idade.", color = "", title = "") 
ggsave(filename ="Idade x Tipo de Violência.bmp",width = 8,height = 5,device='bmp', dpi=150)
ggsave(filename ="Idade x Tipo de Violência.eps",width = 8,height = 5, dpi=150)




# Mapa das taxas de deficiência -------------------------------------------
library(tidyverse)
library(janitor)


#Importando base SINAN antiga.
load("C:/Users/gabli/Desktop/r/Sinan/sinan_13_2023_preliminar_transtorno.RData")
year <- 2023


#Notificações deficiência auditiva
sinan %>% 
  filter(def_auditi == "Sim" & grupo_viol!="Autoprovocada" & ano_ocor == year) %>%
  count(sg_uf, name = "n_audt") |> 
  
  #Deficiência física
  left_join(x = _ , y = sinan |> 
  filter(def_fisica == "Sim" & grupo_viol!="Autoprovocada" & ano_ocor == year) |>
  count(sg_uf, name = "n_fisica"), by = join_by("sg_uf") ) |>
  
   #Deficiência Intelectual
   left_join(x = _ , y = sinan |> 
   filter(def_mental == "Sim" & grupo_viol!="Autoprovocada" & ano_ocor == year) |>
   count(sg_uf, name = "n_intelc"), by = join_by("sg_uf") ) |>
  
  #Deficiência visual
  left_join(x = _ , y = sinan |> 
  filter(def_visual == "Sim" & grupo_viol!="Autoprovocada" &  ano_ocor == year) |>
  count(sg_uf,  name = "n_visual"), by = join_by("sg_uf") ) |> as_tibble() |>

  #Inclusão do código das UFs
  #Incluindo código das UFs
  mutate(cod_ibge = recode(sg_uf, "Rondônia" = 11,"Acre" = 12, "Amazonas" = 13, "Roraima" = 14, "Pará" = 15, "Amapá" = 16,
                         "Maranhão" = 21, "Piauí" = 22, "Ceará" = 23, "Rio Grande do Norte" = 24, "Paraíba" = 25, 
                         "Pernambuco" = 26, "Alagoas" = 27, "Sergipe" = 28, "Bahia" = 29, "Minas Gerais" = 31,
                         "Espírito Santo" = 32, "Rio de Janeiro" = 33, "São Paulo" = 35, "Paraná" = 41, "Santa Catarina" = 42,
                         "Rio Grande do Sul" = 43, "Mato Grosso do Sul" = 50, "Mato Grosso" = 51, "Goiás" = 52,
                         "Distrito Federal" = 53, "Tocantins" = 17) ) |> relocate(cod_ibge, .before = sg_uf) |>
  #Eliminação de UF de residência missing
  filter(!is.na(sg_uf) ) |>
  rename(uf_resd = sg_uf) -> base


#Importando a população PNADc
pop_pnadc <- 
  readxl::read_excel("D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx", 
  #ler a útlima sheet                   
  sheet = tail(excel_sheets("D:/Dropbox/Ipea/Atlas/Pop_Geral_UFs_PNADc.xlsx"), 1) ) |> 
  #Mantém Variáveis de interesse
  select(uf_resd = UF, pop_pnadc = Pop) |>
#Tratamento base com população PNADc
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
          uf_resd = uf_resd |> as_factor() ) 


#Join base sinan e Pnadc
left_join(x = pop_pnadc, y = base, by = join_by("cod_ibge","uf_resd") ) -> base
rm(pop_pnadc)

#Populações PNS
#Importando percentual da populações deficiente PNS 2013 e fazendo join
#Lista dos arquivos importados
list.files(path = "D:/Dropbox/Ipea/Atlas/Atlas 2025/base/pcd/Bases/pop", 
                    pattern = "\\.xlsx$", full.names = TRUE) |>
  
 map(.x = _, ~ read_excel(path = .x, skip = 5) |> 
       rename(uf = "Unidade da Federação", cod_ibge = "Cód.") ) |>
  
 reduce(left_join, by = c("cod_ibge","uf") )  |>
  
 #Identificando populações com deficiência
  #São percentuais
  rename(pop_audti = ...3.x, pop_fisica = ...3.y, pop_intlec = ...3.x.x, pop_vis = ...3.y.y) |>
  
#Remove última linha. Referência da fonte
filter(!is.na(uf))  |> 
  mutate(cod_ibge = cod_ibge |> as.integer() ) |> 
  
#Join na base com notificações de deficiência
left_join(x = base, y = _, join_by("cod_ibge","uf_resd" == "uf") ) |>
  #Transforma o valor das pop PNS em percentual
  mutate(across( c(starts_with("pop") ), ~ .x/100), 
         #Coloca PNADc em milhar
         pop_pnadc = pop_pnadc/10,
         
  #Criando taxas
  tx_audt = (n_audt/(pop_pnadc*pop_audti) )*10,
  tx_fisc = (n_fisica/(pop_pnadc*pop_fisica) )*10,
  tx_intelec = (n_intelc/(pop_pnadc*pop_intlec) )*10,
  tx_visual = (n_intelc/(pop_pnadc*pop_vis) )*10 ) |> 
  
  select(cod_ibge, uf_resd, starts_with("tx") ) |>
  #Exportando
  rio::export(x = _, "taxas_def_ufs.xlsx")
  


