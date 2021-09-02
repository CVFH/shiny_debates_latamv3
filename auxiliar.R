library(tidyverse)
library(shiny)
library(hrbrthemes)
library(RColorBrewer)
library(tidyverse)
library(amap)
library(ape)
library(dendextend)
library(ggraph)
library(igraph)
library(plotly)
library(bslib)

# preparacion de datos

base <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_finalv3.xlsx")
base %>% write.csv("base.csv")

elecciones <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_elecciones.xlsx")
elecciones %>% write.csv("elecciones.csv")

base_organizadores <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_organizadoresv3.xlsx")
base_organizadores %>% write.csv("base_organizadores.csv")

base_formatos <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_formatos_longv3.xlsx")
base_formatos %>% write.csv("base_formatos.csv")

base_temas <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_temas_longv3.xlsx")
base_temas %>% write.csv("base_temas.csv")

base_normativa <- readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/datav3/base_normativa.xlsx")
base_normativa %>% write.csv("base_normativa.csv")

codebook <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebookv3.xlsx") %>% 
  filter(!str_detect(Variable,"longstr_")) %>% 
  filter(!str_detect(Variable,"comentarios"))
codebook %>% write.csv("codebook.csv")

base_cluster_pais <- read.csv("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/base_cluster_pais.csv")
base_cluster_pais %>% write.csv("base_cluster_pais.csv")

codebook_cluster_pais <-  readxl::read_xlsx("C:/Users/carof/Documents/INVESTIGACION y BECAS/PROYECTOS R/debates/debates_latam/codebooks/codebook_base_cluster_pais.xlsx") 
codebook_cluster_pais %>% write.csv("codebook_cluster_pais.csv")  

colorespais <- base %>% 
  distinct(cat_pais, cols_18)

colorespais2 <- base %>% 
  distinct(cat_pais, cols_18) %>% 
  mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom.")) 

coloresformato <- base_formatos %>% 
  distinct(cat_tipo_formato, colores_formato)

# 1.1 GRAFICO DE EVOLUCION TEMPORAL #####


debates_año_pais <- base %>% 
  group_by(ncat_eleccion, cat_pais) %>% 
  summarise(n_debates_año_pais = n()) 

colores <- base %>%
  distinct(cat_pais, cols_18) 

base_años <- elecciones %>% 
  left_join(debates_año_pais) %>% 
  mutate( debates_dico = !is.na(n_debates_año_pais),
          n_debates_año_pais = replace_na(n_debates_año_pais, 0) ) %>% 
  left_join(colores)


# transitorios #####



base_n_formatoss2 <- base %>% 
  group_by(ncat_eleccion) %>% 
  summarise(n_catformatos = mean(n_catformatos, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point((aes(ncat_eleccion,
                  n_catformatos )))

# 3.5 ANEXO cuanti ################

 



# version vieja ################


# 
# 
# 
# debates_año_pais <- base %>% 
#   group_by(ncat_eleccion, cat_pais) %>% 
#   summarise(n_debates_año_pais = n())
# 
# base_años <- elecciones %>% 
#   left_join(debates_año_pais) %>% 
#   mutate( debates_dico = !is.na(n_debates_año_pais),
#           n_debates_año_pais = replace_na(n_debates_año_pais, 0) )  %>% 
#   mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom."))
# 
# 
# 
# # evolucion temporal de realizacion por paises 
# cols_18 <- c("#00A5E3","#8DD7BF","#FF96C5","#FF5768", "#FFBF65",
#              "#6C88C4","#E77577", "#F2D4CC", "#FFD872", "#FC6238",
#              "#00CDAC","#FF6F68", "#FFEC59","#FF60A8","#CFF800",
#              "#74737A", "#00B0BA", "#C05780")
# 
# 
# 
# checkboxGroupInput(inputId ="opt_formato", 
#                    label = "Ver variables de formato...",
#                    choices = c("ncat_ppac", "ncat_competencia"),
#                    selected = "ncat_ppac",
#                    inline = TRUE),
# 
# plotOutput("ev_formatos")
# 
# 
# df.filt2 <- reactive({
#   df.filt<- base %>% 
#     filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
#   df.filt
# })
# 
# output$ev_formatos <- renderPlot({
#   
#   df.filt2() %>% 
#     group_by(ncat_eleccion, cat_pais) %>% 
#     mutate(n_debates_anuales = n()) %>% 
#     ungroup() %>% 
#     ggplot(aes_string("ncat_eleccion", input$opt_formato, colour = "cat_pais", size= "n_debates_anuales")) +
#     geom_point() +
#     gghighlight::gghighlight(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais,
#                              unhighlighted_params = list(colour = alpha("grey", 0.01))) +
#     theme_minimal() +
#     theme(legend.position = "none",
#           plot.title = element_text(hjust = 0.5),
#           plot.subtitle = element_text(hjust = 0.5),
#           axis.text.x = element_text(angle = 90),
#           axis.text.y = element_text(colour= cols_18 )) +
#     scale_x_continuous(breaks = seq(1955,2021,5)) 
#   
# })
