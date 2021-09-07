# library(tidyverse)
# # 
# base <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_finalv3.xlsx")
# # base %>% write.csv("./data/base.csv")
# # 
# # elecciones <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_elecciones.xlsx")
# # elecciones %>% write.csv("./data/elecciones.csv")
# # 
# # base_organizadores <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_organizadoresv3.xlsx")
# # base_organizadores %>% write.csv("./data/base_organizadores.csv")
# # 
# # base_formatos <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_formatos_longv3.xlsx")
# # base_formatos %>% write.csv("./data/base_formatos.csv")
# # 
# # base_temas <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_temas_longv3.xlsx")
# # base_temas %>% write.csv("./data/base_temas.csv")
# # 
# # base_normativa <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_normativa.xlsx")
# # base_normativa %>% write.csv("./data/base_normativa.csv")
# # 
# 
# 
# codebook <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebookv3.xlsx") %>% 
#    filter(!str_detect(Variable,"longstr_")) %>% 
#      filter(!str_detect(Variable,"comentarios"))
# codebook %>% write.csv("./data/codebook.csv")
# # 
# base_cluster_pais <- read.csv("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_cluster_pais.csv")
# base_cluster_pais %>%
#   rename(ncat_ano_primer_debate = "ncat_año_primer_debate") %>% 
#   write.csv("./data/base_cluster_pais.csv")
# # 
# codebook_cluster_pais <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebook_base_cluster_pais.xlsx") 
# codebook_cluster_pais %>% write.csv("./data/codebook_cluster_pais.csv") 
# # 
# 
# 
# base <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_finalv3.xlsx")
# colorespais <- base %>% 
#   distinct(cat_pais, cols_18)
# base_anual <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_elecciones.xlsx")  %>% 
#   left_join(base %>% 
#               group_by(ncat_eleccion, cat_pais) %>% 
#               summarise(n_debates_ano_pais = n())) %>% 
#   mutate( debates_dico = !is.na(n_debates_ano_pais),
#           n_debates_ano_pais = replace_na(n_debates_ano_pais, 0) ) %>% 
#   left_join(colorespais)
# base_anual %>% write.csv("./data/base_anual.csv")

