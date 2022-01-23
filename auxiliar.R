library(tidyverse)
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

#base <- read.csv("https://raw.githubusercontent.com/CVFH/shiny_debates_latamv3/master/data/base.csv", stringsAsFactors = F)
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

# armado de mapa ####################
# base <- read.csv("data/base.csv", stringsAsFactors = F)
# colorespais <- base %>% 
#   distinct(cat_pais, cols_18)
# #require(ggrepel)
# 
# require(ggplot2)
# 
# require(maps)
# require(mapdata)
# install.packages(c("maps", "mapdata"))
# 
# ccode <- c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
#                         "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
#                         "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
#                         "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
#                         "Dominica", "Saba") %>% as.tibble()
#colorespais <- readxl::read_xlsx("colorespais.xlsx") %>% 
#  mutate(cat_pais2 = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 
#colorespais %>% write.csv("ccodes.csv")

#colorespais %>% writexl::write_xlsx("colorespais.xlsx") 
# 
# 
# mapa <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
#                                      "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
#                                      "Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
#                                      "Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
#                                      "Dominica", "Saba"), 
#                 fill = "grey70", colour = "black")
# 
# 
# ggplot() + mapa + theme_bw() + xlab("Longitude (decimals)") + ylab("Latitude (decimals)") + 
#   theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank())
# 
# 
# ccodes <-  read.csv("ccodes.csv", stringsAsFactors = F, encoding = "Latin-1")
# base_cluster_pais <- read.csv("data/base_cluster_pais.csv", stringsAsFactors = F)
# 
# mapear <- ccodes %>% 
#   left_join(base_cluster_pais %>%  select(-X)) 
# 
# data_mapa <- map_data("world2", mapear$ccode) %>%
#   rename(ccode = "region") %>% 
#   left_join(mapear)
# 
# mapear %>% 
#   ggplot() +
#   geom_polygon(aes(x = long, 
#                    y = lat, 
#                    group = group, 
#                    fill = ccode, 
#                    alpha = ncat_meancompetencia)) + 
#   scale_colour_manual(breaks = ccodes$ccode,
#                       values = ccodes$cols_18) +
#   coord_fixed(1) +
#   theme_void() +
#   theme(legend.position = "none")
# 
# mapear  <-    map_data("world2", ccodes$ccode) %>%
#   rename(ccode = "region") %>% 
#   left_join(ccodes) %>% 
#   left_join(base_cluster_pais %>%  select(-X))
# 
# mapear %>%  write.csv("mapear.csv")
# 
# 
# 
# # basep <- base %>% 
# #   filter(cat_pais =="Argentina" & ncat_eleccion==1965)
# # 
# # nrow(basep)
# 
# # wordcloud ######
# # 
# # library(wordcloud)
# # library(tidytext)
# # base <- read.csv("data/base.csv", stringsAsFactors = F)
# # 
# # str <-  base %>%
# #   unnest_tokens(word, str_organizador)
# # words <- str %>% count(word, sort=TRUE) %>% 
# #   subset(str_length(word)>2)
# # 
# # wordcloud(words = words$word, freq = words$n, min.freq = 1,           
# #           max.words=200, random.order=FALSE, rot.per=0.35,            
# #           colors=brewer.pal(8, "Dark2"))
# 
# 
# 
# base_subtipos <- base_organizadores %>% 
#   group_by(cat_pais) %>% 
#   mutate(n_debates_pais = n_distinct(id_debate),
#          n_organizadores_pais = n() ) %>% 
#   ungroup() %>% 
#   group_by(cat_pais, cat_tipoorgv2) %>% 
#   mutate(n_tipo_pais = n(),
#          n_debates_tipo_pais = n_distinct(id_debate),
#          pr_debates_tipo_pais = n_debates_tipo_pais/n_debates_pais) %>% 
#   ungroup() %>% 
#   group_by(cat_tipoorgv2, ncat_subtipov2, cat_pais) %>% 
#   summarise(n_subtipo_pais = n(),
#             n_debates_con_subtipo_pais = n_distinct(id_debate),
#             n_debates_pais = mean(n_debates_pais),
#             # n_organizadores_pais = mean(n_organizadores_pais),
#             # n_osc_pais = mean(n_osc_pais), 
#             n_debates_tipo_pais = mean(n_debates_tipo_pais),
#             pr_debates_tipo_sobre_totdebates = round(n_debates_tipo_pais/n_debates_pais, 3)*100,
#             # pr_subtipo_pais_sobre_osc = round(n_subtipo_pais/n_osc_pais, 3)*100,
#             #pr_subtipo_pais_sobre_totorgs = round(n_subtipo_pais/n_organizadores_pais, 3)*100,
#             pr_debates_con_subtipo_pais_sobre_debatestipo = round(n_debates_con_subtipo_pais/n_debates_tipo_pais, 3)*100,
#             pr_debates_con_subtipo_pais_sobre_totdebates = round(n_debates_con_subtipo_pais/n_debates_pais, 3)*100) %>% 
#   arrange(cat_pais, cat_tipoorgv2, ncat_subtipov2) #%>% 
# 
# 
# labs(x = "Año de elección", 
#      y = "n tipos de organizador",
#      caption = "Elaboración propia.
#                  Cada círculo representa un debate, su tamaño, 
#                  la cantidad total de entidades que lo organizaron.")
