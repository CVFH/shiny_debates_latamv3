# library(tidyverse)
# library(shiny)
# library(hrbrthemes)
# library(RColorBrewer)
# library(tidyverse)
# library(amap)
# library(ape)
# library(dendextend)
# library(ggraph)
# library(igraph)
# library(plotly)
# library(bslib)

# lista_paquetes <- c("shiny","tidyverse","amap","RColorBrewer","plotly","bslib")
# 
# # Revisar si paquetes están instalados. Si lo están, cargará los paquetes. Si no lo están, instalará y luego cargará los paquetes.
# nuevos_paquetes <- lista_paquetes[!(lista_paquetes %in% installed.packages()[,"Package"])]
# lapply(nuevos_paquetes, install.packages); lapply(lista_paquetes, require, character.only = TRUE)


# preparacion de datos ##########

# base <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base.csv", stringsAsFactors = F)
# base_años <-  read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_años.csv", stringsAsFactors = F)
# base_organizadores <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_organizadores.csv", stringsAsFactors = F)
# base_formatos <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_formatos.csv", stringsAsFactors = F)
# base_temas <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_temas.csv", stringsAsFactors = F)
# base_normativa <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_normativa.csv", stringsAsFactors = F)
# codebook <-  read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/codebook.csv", stringsAsFactors = F)
# base_cluster_pais <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base_cluster_pais.csv", stringsAsFactors = F)
# codebook_cluster_pais <-  read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/codebook_cluster_pais.csv", stringsAsFactors = F)
# 
# colorespais <- base %>% 
#   distinct(cat_pais, cols_18)
# 
# colorespais2 <- base %>% 
#   distinct(cat_pais, cols_18) %>% 
#   mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 
# 
# coloresformato <- base_formatos %>% 
#   distinct(cat_tipo_formato, colores_formato)


# transitorios #####

# 
# base_n_formatoss2 <- base %>% 
#   group_by(ncat_eleccion) %>% 
#   summarise(n_catformatos = mean(n_catformatos, na.rm = TRUE)) %>% 
#   ggplot() +
#   geom_point((aes(ncat_eleccion,
#                   n_catformatos )))


  