#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = bs_theme(version = 4, bootswatch = "sandstone"),
    title = "Debates presidenciales en América Latina",
                 
                 # PANEL DIMENSIONES ##############
                 tabPanel("Análisis por Dimensión", 
                          
                          fluidPage(

                                  # Sidebar with a slider input for number of bins 
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput(inputId = "selec_pais", 
                                                      label = "Seleccione un país", 
                                                      choices = unique(base_años$cat_pais),
                                                      selected = "Argentina", 
                                                      multiple = TRUE),
                                          
                                          sliderInput(inputId ="selec_t", 
                                                      label = "Seleccione un período", 
                                                      min = 1960, 
                                                      max = 2025,
                                                      value = c(min(base$ncat_eleccion), max(base$ncat_eleccion))),
                                          
                                          actionButton("action_dimensiones", 
                                                       "Visualizar selección")
                                      ),
                                      
                                      # Show a plot of the generated distribution
                                      mainPanel(
                                      tabsetPanel(
                                          tabPanel("Arraigo", 
                                                   
                                                   h2("Grado de arraigo de la práctica", align = "center"),
                                                   
                                                   h4("Cantidad de debates realizados"),
                                                   h6("En el tiempo y por país"),
                                                   
                                                   plotlyOutput("ev_anual"),
                                                   
                                                   h4("Índice de ausencias"),
                                                   h6("Promedio en relación a la cantidad de debates en una elección"),
                                                   
                                                   plotOutput("ausencias")

                                          ),
                                          
                                          tabPanel("Organizadores", 
                                                   
                                                   h2("Tipo de organizador de los debates", align = "center"),
                                                   
                                                   h4("Tipo de organizador de los debates"),
                                                   h6("A través del tiempo, por país"),
                                                   
                                                   plotOutput("organizadores"),
                                                   
                                                   h4("Variedad de organizadores de un debate"),
                                                   
                                                   plotlyOutput("alianzas")
                                                   ),
                                          
                                          tabPanel("Formatos", 
                                                   
                                                   h2("Formatos de los debates", align = "center"),
                                                   
                                                   h4("Esquemas de interacción"),
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                   plotOutput("formatos_t"),
                                                   plotOutput("formatos_p")
                                                   ) ,
                                                   
                                                   h6("Conversión ordinal de esquemas de interacción"),
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                       plotOutput("cuanti_c"),
                                                       plotOutput("cuanti_p")
                                                   ) ,
                                                   
                                                   h4("Distribución temática"),
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                       plotOutput("temas_t"),
                                                       plotOutput("temas_p")
                                                   ) 
                                                   ),
                                          
                                          tabPanel("Normativa", 
                                                   
                                                   h2("Normativa en la materia", align = "center"),
                                                   
                                                   h4("Espíritu de la norma"),
                                                   h6("desde el punto de vista de..."),
                                                   
                                                   tableOutput("tabla_cambiosnormativa"),
                                                   
                                                   h4("Detalle"),
                                                   
                                                   tableOutput("texto_normativa")
                                                   
                                                   )
                                      )
                                  )
                              ))),
                 
                 # PANEL INTERDEPENDENCIA ##########
                 tabPanel("Interdependencia", 
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      
                                      selectInput(inputId = "selec_indicadores", 
                                                  label = "Seleccione los indicadores a incluir en el modelo", 
                                                  choices = colnames(base_cluster_pais),
                                                  selected = c("n_indexausentes",
                                                               "n_interrupciones",
                                                               "ncat_meanppac", 
                                                               "ncat_meancompetencia",
                                                               "n_sd_competencia",
                                                               "n_sd_ppac",
                                                               "cat_pais"), 
                                                  multiple = TRUE),
                                      
                                      selectInput(inputId = "selec_link", 
                                                    label = "Elija método de agregación",
                                                    choices = c("complete", "ward"),
                                                    selected = "complete", 
                                                  multiple = FALSE ),
                                      
                                      actionButton("action_interdependencia", 
                                                   "Visualizar selección")
                                      ),
                                  
                                  mainPanel(
                                      
                                      h2("Análisis de clusters", align = "center"),
                                      
                                      plotOutput("plot_cluster"),
                                      tableOutput('tabla_indicadores')
                                  )
                              )
                          )),
                 
                 # PANEL CODEBOOK ##########
                 tabPanel("Codebook", 
                          fluidPage(
                              
                              sidebarLayout(
                                  
                                  sidebarPanel(
                                      
                                      selectInput(inputId = "selec_variable", 
                                                  label = "Seleccione una variable", 
                                                  choices = unique(codebook$Variable),
                                                  selected = "cat_pais", 
                                                  multiple = FALSE),
                                      
                                      actionButton("action_codebook", 
                                                   "Buscar")
                                      
                                  ),
                                  
                                  mainPanel(
                                      
                                      h2("Codebook", align = "center"),
                                      
                                      tableOutput('tabla_codebook'),
                                      plotOutput("hist_codebook"),
                                      tableOutput('tabla_summary')
                                      
                                  )
                              )
                          )))
    
 

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # OUTPUTS DIMENSIONES #########
    
    # arraigo
    df.filt_base_dimensiones <- eventReactive(input$action_dimensiones, {
        df.filt <- base %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] )
    })    
    
    df.filt_base_anos <- eventReactive(input$action_dimensiones, {
        df.filt <- base_años %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] )
    })
    
    output$ev_anual <- renderPlotly({

        ggplotly(
            df.filt_base_anos() %>%  
                     ggplot(aes(ncat_eleccion, 
                                n_debates_año_pais,
                                colour = cat_pais))  +
                     geom_line() + 
                     geom_point(aes(size= n_debates_año_pais, shape = debates_dico, alpha= debates_dico)) +
                     scale_color_manual(breaks= colorespais2$cat_pais,
                                        values=colorespais2$cols_18) +
                     scale_shape_manual(values=c("FALSE" = 4, "TRUE" = 19)) +
                     scale_alpha_manual(values=c("FALSE" = 0.4, "TRUE" = 1)) +
                     theme_minimal() +
                     theme(legend.position = "none",
                           axis.text.x = element_text(angle = 90) ) +  
                     scale_x_continuous(breaks = seq(1955,2021,10)) +
                     labs(x = "", 
                          y = "",
                          caption = "Elaboración propia. 
       El tamaño de los círculos representa la cantidad de debates hechos en una elección.
       Las x representan elecciones sin debates") )
    
        })
    
    output$ausencias <- renderPlot({
       
         df.filt_base_dimensiones() %>% 
        filter(ncat_ronda== 1) %>% 
            group_by(ncat_eleccion, cat_pais, cols_18) %>% 
            summarise(cantidad_debates_ronda = n(),
                      mean_indexausentes = mean(n_indexausentes, na.rm = TRUE)) %>% 
        ggplot() +
        geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = cat_pais, vjust = 0), alpha = 0.6) +
        geom_label(aes(mean_indexausentes, cantidad_debates_ronda, colour = cols_18, label = ncat_eleccion, vjust = 1), alpha = 0.6) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = seq(0,80,5)) +
        scale_y_continuous(breaks = seq(0,30,1)) +
        labs(x = "Índice de ausencias", y = "N debates primera ronda",
             caption = "Elaboración propia.
       El índice de ausencias contempla el procentaje de votos obtenido por los ausentes, multiplicado por la proporcion de ausentes a los debates.
             No se cuentan los debates previos a la segunda vuelta electoral.")
        
    })
    
    # organizadores
    
    df.filt_base_organizadores <- eventReactive(input$action_dimensiones, {
        df.filt <- base_organizadores %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$organizadores <- renderPlot({
        
        df.filt_base_organizadores() %>% 
            group_by(str_organizador, cat_pais, ncat_eleccion, t_fecha, id_debate) %>% 
            mutate(n_catorganizadorv2 = n_distinct(cat_tipoorgv2),
                   n_organizadores = n()) %>% 
            ungroup %>% 
            mutate(n_prppaccatorg = 1/n_catorganizadorv2, # porcentaje de participacion del organizador sobre la variedad de tipos de organizadores de un debate
                   n_prppacorg = 1/n_organizadores) %>% 
            group_by(ncat_eleccion, cat_pais, cat_tipoorgv2) %>% 
            summarise(n_prppaccatorg = sum(n_prppaccatorg, na.rm = T),
                      n_prppacorg = sum(n_prppacorg, na.rm = T) ) %>% 
            mutate(cat_pais = str_replace(cat_pais,"Republica Dominicana", "Rep. Dom."),
                   cat_tipoorgv2 = ifelse(is.na(cat_tipoorgv2), "S/ Datos", cat_tipoorgv2))  %>%    
        ggplot(aes(ncat_eleccion, as.factor(cat_tipoorgv2) %>% 
                       fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos"), 
                   colour = cat_tipoorgv2, 
                   size= n_prppaccatorg,
                   shape= as.factor(cat_tipoorgv2) %>% 
                       fct_relevel("estado", "mmp","osc", "educ", "mmc", "S/ Datos")))  +
            geom_point() +
            facet_wrap( ~ cat_pais) +
            theme_minimal() +
            theme(legend.position = "none",
                  axis.text.x = element_text(angle = 90),
                  panel.border = element_rect(color = "black",
                                                    fill = NA,
                                                    size = 0.5)) + 
            scale_shape_manual(values=c("S/ Datos" = 4, 
                                        "estado" = 19,
                                        "mmp" = 19,
                                        "educ" = 19,
                                        "mmc" = 19,
                                        "osc" = 19)) +
            scale_colour_manual(breaks = c("S/ Datos", "estado", "mmp","osc", "educ", "mmc"),
                                values = c("#C6C5C5","#2CD74F", "#23B0C2","#F1B61B", "#F2E947","#F42AF3")) +
            scale_y_discrete(
                breaks = c("estado", "mmp","osc", "educ", "mmc", "S/ Datos"),
                labels = c( "Estado","M. Públicos","OSCs","Educativo","M. Comerciales", "S/ Datos")) +
            scale_x_continuous(breaks = seq(1950,2021,10)) +
            labs(x = "",
                 y = "Tipo de organizador",
                 caption = "Elaboración propia. 
        El tamaño de los círculos es proporcional a la cantidad de debates que involucraron a cada tipo de organizador en un año dado")
    })
    
    output$alianzas <- renderPlotly({
        
        ggplotly(
        df.filt_base_organizadores()  %>%  
            group_by(id_debate, ncat_eleccion, cat_pais) %>% 
            summarise(n_orgs = n(),
                      n_variedadorgs = n_distinct(cat_tipoorgv2))  %>% 
            ggplot() +
            geom_point(aes(ncat_eleccion, n_variedadorgs, 
                           size = n_orgs,
                           colour = cat_pais),
                       alpha = 0.5 ) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(0,15,1)) +
            scale_x_continuous(breaks = seq(1955,2021,5)) +
            scale_color_manual(breaks= colorespais2$cat_pais,
                               values=colorespais2$cols_18) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            labs(x = "Año de elección", 
                 y = "n tipos de organizador",
                 caption = "Elaboración propia.
                 El tamaño del cìrculo representa la cantidad total de organizadores")
        )
    })
    
    # formatos 
    
    df.filt_formatos <- eventReactive(input$action_dimensiones,{
        df.filt <- base_formatos %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$formatos_t <- renderPlot({
    
        df.filt_formatos() %>% 
            group_by(ncat_eleccion, cat_tipo_formato) %>% 
            summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )  %>% 
            ggplot(aes(ncat_eleccion, fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                                                  "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                                                  "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
                       colour = cat_tipo_formato, size= n_peso_formato_xdebate))  +
            geom_point() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            scale_x_continuous(breaks = seq(1955,2021,5)) +
            scale_color_manual(breaks= coloresformato$cat_tipo_formato,
                               values=coloresformato$colores_formato) +
            scale_y_discrete(
                breaks = c("pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                           "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                           "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"),
                labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Apertura", "Expositivo", "Duelo", "Libre")) +  
            labs(x = "",
                 y = "Tipo de intercambio",
                 title = "En el tiempo",
                 caption = "")  
        
    
    })   
    
    output$formatos_p <- renderPlot({
        
        df.filt_formatos() %>% 
            group_by(cat_pais, cat_tipo_formato) %>% 
            summarise(n_peso_formato_xdebate = sum(n_peso_formato_xdebate, na.rm=TRUE) )%>% 
            ggplot(aes(cat_pais,
                       fct_relevel(cat_tipo_formato, "pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                                   "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                                   "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"), 
                       colour = cat_pais, size= n_peso_formato_xdebate))  +
            geom_point() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            scale_color_manual(breaks= colorespais$cat_pais,
                               values=colorespais$cols_18) +
            scale_y_discrete(
                breaks = c("pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                           "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                           "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"),
                labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Apertura", "Expositivo", "Duelo", "Libre")) +
            labs(x = "",
                 y = "",
                 title = "por país",
                 caption = "")
    })   
    
    df.filt_temas <- eventReactive(input$action_dimensiones,{
        df.filt <- base_temas %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$temas_t <- renderPlot({
        
        df.filt_temas() %>% 
            group_by(ncat_eleccion, cat_tipo_tema) %>% 
            summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) ) %>% 
            ggplot(aes(ncat_eleccion, cat_tipo_tema, 
                       colour = cat_tipo_tema, size= n_peso_tema_xdebate))  +
            geom_point() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            scale_x_continuous(breaks = seq(1955,2021,5))  +
            scale_y_discrete(
                breaks = c("pr_temabloques", "pr_temalibre", "pr_temamonotema", "pr_temapuntuales"),
                labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
            labs(x = "",
                 y = "",
                 title = "En el tiempo",
                 caption = "") 
    }) 
    
    output$temas_p <- renderPlot({
        
        df.filt_temas() %>% 
            group_by(cat_pais, cat_tipo_tema) %>% 
            summarise(n_peso_tema_xdebate = sum(n_peso_tema_xdebate, na.rm=TRUE) ) %>%  
            ggplot(aes(cat_pais, cat_tipo_tema, 
                       colour = cat_pais, size= n_peso_tema_xdebate))  +
            geom_point() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            scale_color_manual(breaks= colorespais$cat_pais,
                               values=colorespais$cols_18) +
            scale_y_discrete(
                breaks = c("pr_temabloques", "pr_temalibre", "pr_temamonotema", "pr_temapuntuales"),
                labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
            labs(x = "",
                 y = "",
                 title = "por país",
                 caption = "")
        
    }) 
    
    output$cuanti_c <- renderPlot({
        df.filt_base_dimensiones() %>% 
            ggplot() +
            geom_boxplot(aes(cat_pais, 
                             as.numeric(ncat_competencia), 
                             fill = cat_pais)) +
            scale_fill_manual(breaks= colorespais$cat_pais,
                              values=colorespais$cols_18) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90))  +
            labs(x = "",
                 y = "",
                 title = "N° de competencia")
    }) 
    
    output$cuanti_p <- renderPlot({
        
        df.filt_base_dimensiones() %>% 
            ggplot() +
            geom_boxplot(aes(cat_pais,
                             as.numeric(ncat_ppac), 
                             fill = cat_pais)) +
            scale_fill_manual(breaks= colorespais$cat_pais,
                              values=colorespais$cols_18) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90))  +
            labs(x = "",
                 y = "",
                 title = "N° de participación")
    }) 
    
    # normativa 
    
    df.filt_normativa <- eventReactive(input$action_dimensiones,{
        df.filt <- base_normativa %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    
    output$tabla_cambiosnormativa <- renderTable( 
        df.filt_normativa() %>%
            group_by(cat_pais,
                     cat_regmedios,
                     cat_regestado , 
                     cat_regcandidatos) %>% 
            summarise(Vigencia_desde = min(ncat_eleccion)) %>% 
            rename(Pais = "cat_pais",
                   Candidatos = "cat_regcandidatos",
                   Estado = "cat_regestado",
                   "Medios u organizadores" = "cat_regmedios") %>% 
            dplyr::arrange(Pais, desc(Vigencia_desde)), 
        hover = T, 
        digits = 0
    )
    
    output$texto_normativa <- renderTable( 
        
        df.filt_normativa() %>% 
            group_by(cat_pais) %>% 
            mutate( max = max(ncat_eleccion)) %>% 
            ungroup() %>% 
            filter(max== ncat_eleccion) %>% 
            select(cat_pais, str_regulacion) %>% 
            rename(Pais = "cat_pais",
            "Detalle ultima disponible" = "str_regulacion"), 
        hover = T, 
        digits = 0
        
    )

    
    # OUTPUTS INTERDEPENDENCIA ########
    
    df.filt.cluster <- eventReactive(input$action_interdependencia, {
        
        rownames(base_cluster_pais) <- base_cluster_pais$cat_pais
        
        df.dend <- base_cluster_pais %>% 
            select(input$selec_indicadores) %>% 
            mutate_if(is.numeric, scale) %>% 
            hcluster(., link= input$selec_link) %>% 
            as.dendrogram()

        df.dend
    })
    
    df.filt.cluster_tabla <- eventReactive(input$action_interdependencia, {
        
        df.filt <- codebook_cluster_pais %>%  
            filter(Indicador %in% input$selec_indicadores)
        df.filt 
        
    })    
    
    output$plot_cluster <- renderPlot({
        df.filt.cluster() %>% 
         set("labels_col", value = c(brewer.pal(n = 9, name = "Paired")), k=9) %>%
         set("branches_lty", 1) %>%
         set("branches_k_color", value =  c(brewer.pal(n = 9, name = "Paired")), k=9) %>% 
        plot(axes = F)
    }) 
    
    output$tabla_indicadores <- renderTable( 
        df.filt.cluster_tabla(), hover = T
    )
    
    # OUTPUTS CODEBOOK ########
    
    df.filt_codebook <- eventReactive(input$action_codebook,{
        df.filt <- codebook %>% 
            filter(Variable == input$selec_variable) 
        df.filt
    })
    
    df.filt_columna <- eventReactive(input$action_codebook,{
        df.filt <- base %>% 
            select(input$selec_variable) %>% 
            as_tibble()
    })
    
    output$tabla_codebook <- renderTable(df.filt_codebook())
    
    output$hist_codebook <- renderPlot({
        
        df.filt_columna() %>% 
            ggplot(aes_string(names(df.filt_columna())[1])) +
            geom_bar() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            labs(x = "",
                 y = "count",
                 title = "Distribución de la variable")
    })
    
    output$tabla_summary <- renderTable(summary(df.filt_columna()),
                                        rownames = F,
                                        colnames = F,
                                        bordered = T)       
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
