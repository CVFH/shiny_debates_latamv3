# paquetes
library(shiny)
library(tidyverse)
library(amap)
library(RColorBrewer)
library(plotly)
library(bslib)
library(dendextend)
library(bslib)
library(maps)
library(bslib)
library(mapdata)
library(wordcloud)
library(tidytext)
library(DT)

# datos
base <- read.csv("data/base.csv", stringsAsFactors = F)
base_anual <-  read.csv("data/base_anual.csv", stringsAsFactors = F)
base_organizadores <- read.csv("data/base_organizadores.csv", stringsAsFactors = F)
base_formatos <- read.csv("data/base_formatos.csv", stringsAsFactors = F)
base_temas <- read.csv("data/base_temas.csv", stringsAsFactors = F)
base_normativa <- read.csv("data/base_normativa.csv", stringsAsFactors = F, encoding = "UTF-8")
codebook <-  read.csv("data/codebook.csv", stringsAsFactors = F, encoding = "UTF-8")
base_cluster_pais <- read.csv("data/base_cluster_pais.csv", stringsAsFactors = F, encoding = "UTF-8") %>% select(-c(X, X.1))
codebook_cluster_pais <-  read.csv("data/codebook_cluster_pais.csv", stringsAsFactors = F, encoding = "UTF-8" )
ccodes <-  read.csv("data/ccodes.csv", stringsAsFactors = F, encoding = "UTF-8" )
mapear <-  read.csv("data/mapear.csv", stringsAsFactors = F, encoding = "UTF-8" )
dims <- read.csv("data/dims.csv", stringsAsFactors = F, encoding = "UTF-8" )

coloresformato <- base_formatos %>% 
    distinct(cat_tipo_formato, colores_formato)

# UI #####################
ui <- navbarPage(
    theme = bs_theme(version = 4, bootswatch = "sandstone"),
    title = "Debates presidenciales en América Latina",
    
    # PRESENTACION
    tabPanel("About",
             h2("Acerca de", align = "center"),
             h4("Acerca de esta app"),
             p("Este sitio se propone ser el anexo interactivo de una investigación
                                                     que examina la trayectoria de los debates presidenciales televisados
                                                     en América Latina. "),
             p("Aquí entendemos que las pantallas de estos debates son una ventana: 
                                                     hacia las relaciones que entablan distintos actores en el ámbito 
                                                     de la comunicación política 
                                                     de las democracias actuales.
                                                     Por eso, nos preguntamos ", em("cómo "), "–quién, cuándo, dónde, de qué maneras– 
                                                     se han organizado encuentros entre candidatos a la presidencia en televisión."),
             p("Para responder a esa pregunta, recopilamos información sobre la práctica en 
                                                     18 países latinos para todas las elecciones desde la llegada de la televisión.
                                                     A lo largo y ancho del continente, reconstruimos historias diversas."), 
             p("Organizamos la variedad hallada en cuatro ejes o dimensiones: 
                                                     la penetración, los organizadores, los formatos y la normativa de los debates. 
                                                     En la primera pestaña de este sitio, exponemos visualmente la distribución
                                                     de los casos estudidados en varios indicadores para cada una de estas cuatro dimensiones."),
             p("Además buscamos agregar los resultados obtenidos de modo de obtener una lectura del conjunto.
                                                     Exponemos parte de estos esfuerzos en la segunda pestaña de este sitio,
                                                     que permite al usuario ejecutar un análisis de clusters."),
             p("Para facilitar el entendimiento de la información disponible y el uso del sitio,
                                                     por último,
                                                     la última pestaña contiene el ", em("codebook "), 
               "que sistematiza la base de datos producto y materia prima de nuestro trabajo."),
             h4("Sobre su autora"),
             p("Este sitio y la investigación que le da origen son de la autoría y responsabilidad de Carolina Franco. 
               Soy Lic. en Cs. de la Comunicación social de la Universidad de Buenos Aires. 
               Actualmente soy becaria doctoral del IIDYPCA-CONICET. En este marco, 
               estoy completando mis estudios de posgrado en Ciencia Política en la Universidad Torcuato di Tella.")
    ),
    
                 
    
                 # PANEL DIMENSIONES ##############
                 tabPanel("Análisis por Dimensión", 
                          
                          fluidPage(
                                  sidebarLayout(
                                      sidebarPanel(
                                          selectInput(inputId = "selec_pais", 
                                                      label = "Seleccione un país", 
                                                      choices = unique(base_anual$cat_pais),
                                                      selected = "Argentina", 
                                                      multiple = TRUE),
                                          
                                          uiOutput("slider"),
                                
                                          actionButton("action_dimensiones", 
                                                       "Visualizar selección"),
                                          br(),
                                          br(),
                                          span(textOutput("text_error"), style="color:red")
                                          
                                      ),
                                      
                                      mainPanel(
                                          
                                          p("Nuestra propuesta examina los variables modos en que 
                                          los debates presidenciales televisados 
                                          se realizan en  en América Latina. 
                                            Organizamos las diferencias a lo largo de cuatro dimensiones."),
                                          br(),
                                          
                                      tabsetPanel(
                                          tabPanel("Presentación",
                                                   
                                          p(""),
                                          
                                          p("Esta tesis entiende por ", em("debate presidencial televisado"), " a 
                                          todo encuentro entre dos o más candidatos a la presidencia que sea transmitido en vivo y en directo por un canal o medio audiovisual, 
                                            durante el período de campaña electoral."),
                                          
                                          p("Como dijimos, 
                                            para estudiar la variación en los modos en los que los debates han sido llevados a cabo
                                             en América Latina,
                                             nos ha resultado conveniente sistematizar cuatro dimensiones. Estas son 
                                            (1) el grado de arraigo o penetración, 
                                            (2) el tipo de organizador, 
                                            (3) los formatos, 
                                            y (4) la regulación de la práctica."),
                                          
                                          p(em("En la tabla que sigue, adelantamos una síntesis de las variables bajo estudio,
                                                a cada una de las cuales dedicamos una pestaña respectiva dentro de esta sección.")),
                                          
                                          dataTableOutput("presentaciondims")

                                            
                                          ),
                                          
                                          tabPanel("Arraigo", 
                                                   
                                                   h2("Grado de arraigo de la práctica", align = "center"),
                                                   
                                                   p("La primera dimensión de variación estudiada es ",
                                                   em("la penetración de la práctica"), 
                                                     ": el lugar que los debates ocupan para las elecciones locales"),
                                                   
                                                   p("Esta, a su vez, es apreciable en dos subdimensiones:
                                                     la intensidad de los debates, la primera, 
                                                     su importancia, la segunda"),
                                                   
                                                   
                                                   p(
                                                       "La ", strong("intensidad "), "es una variable cuantitativa que nos permite aproximarnos a
                                                       qué tan predecible o esperables son los debates, es decir,
                                                       qué tan acostumbradas están las personas de un país a participar de, 
                                                       organizar y mirar debates entre candidatos a la presidencia en televisión.
                                                       Tal intensidad se aprecia en la antigüedad de la práctica, 
                                                       su constancia o inconstancia a través del tiempo 
                                                       y la cantidad de debates que tienen lugar en un proceso electoral."
                                                   ),
                                                   
                                                   p("Es interesante notar que cada vez se realizan más debates y en más países,
                                                     pero, a su vez, estos últimos varían en qué tan temprano o tarde han adoptado la práctica, con qué constancia 
                                                     y en cuántos debates suelen realizarse en los períodos electorales contemporáneos."),
                                                   
                                                   h4("Cantidad de debates realizados"),
                                                   h5("En el tiempo y por país"),
                                                   
                                                   p("Encontramos diferencias en qué tan rutinarios son los debates, 
                                                     en el tiempo y entre países."),
                                                   
                                                   plotlyOutput("ev_anual"),
                                                   
                                                   br(),
                                                   p("Al margen de qué tan intensamente son realizados, no todos los debates
                                                     tienen la misma ",strong("importancia"), ", no siempre acaparan la misma atención.
                                                     Para medir esta subdimensión del grado de penetración de la práctica,
                                                     podríamos comparar niveles de audiencias o de cobertura mediática. 
                                                     Lamentablemente, hay pocos datos comparables para los casos de nuestra muestra.
                                                     También podemos apreciar qué tan recordados son los debates a lo largo de la historia.
                                                     Algunos son olvidados, otros impregnan la memoria colectiva. 
                                                     De nuevo, esta medida es difícil de representar gráficamente."),
                                                   p("Alternativamente, vale la pena considarar que algunos debates son desestimados por los candidatos: 
                                                   estos se ausentan, y al hacerlo, sobre todo si son muy populares, 
                                                     parece mermar el interés y la magnitud del público que convoca una emisión, es decir, en su relevancia.
                                                     De todos modos, no está clara la relación entre las ausencias y la importancia de un evento:
                                                     ¿los debates pasan desapercibidos como consecuencia de la falta de los prinicipales candidatos,
                                                     o los últimos deciden no ir porque saben que los primeros no son muy relevantes?"),
                                                   
                                                   p("Al margen de cuánto sirva para dar cuenta o no de la importancia de los encuentros, 
                                                   en cualquier caso, 
                                                     no está de más conocer la distribución de esta medida, es decir, 
                                                     qué tanto han logrado o no los debates presidenciales televisados convocar a los principales contendientes en la carrera electoral.
                                                     A continuación exponemos una representación gráfica de estas ausencias, calculadas a nivel promedio por período electoral (por campaña en un determinado año y país).
                                                     Se observa que algunas campañas comportan un nivel medio de ausencias. Esto ocurre cuando se hacen muchos debates, produciéndose una jerarquización entre ellos:
                                                     se vuelve difícil que todos los candidatos asistan a todos los encuentros, y, por lo tanto, tiende a haber un par de declinaciones a debatir. 
                                                     En cambio, en campañas en las que se realiza un único o pocos duelos televisados, los candidatos 
                                                     o bien otorgan prioridad a estos eventos, 
                                                     y entonces nos encontramos con muy pocas ausencias, 
                                                     o bien hallamos importantes 'faltazos' que, 
                                                     muchas veces, terminan hechando por la borda el encuentro."),
                                                   
                                                   h4("Índice de ausencias"),
                                                   h5("Promedio en relación a la cantidad de debates en una elección"),
                                                   
                                                   plotOutput("ausencias")

                                          ),
                                          
                                          tabPanel("Organizadores", 
                                                   
                                                   h2("Tipo de organizador de los debates", align = "center"),
                                                   
                                                   p("La segunda dimensión analizada es ",
                                                     em("el tipo de organizador de los debates"), ". 
                                                     Es interesante ver quién toma a su cargo la realización de estos encuentros,
                                                     ya que suele tener que arbitrar las conflictivas negociaciones entre candidatos,
                                                     y asegurar que se cumplan los compromisos asumidos."),
                                                   p("Medios privados, medios públicos, Estado, organizaciones de la sociedad civil y del ámbito educativo 
                                                     son los actores que han intervenido en la historia latina de la práctica. 
                                                     De modo general, mientras que en sus comienzos los debates eran traccionados en su mayoría 
                                                     por canales de televisión privados, hoy asistimos a una mayor diversidad de organizadores. 
                                                     Aún así, los países tienden a diferenciarse conforme al tipo de organizador predominante."),
                                                   p(em("
                                                     A continuación, podemos ver cómo ha variado el carácter de los organizadores de los debates
                                                     a lo largo del tiempo y por país")),
                                                   
                                                   h4("Tipo de organizador de los debates"),
                                                   h5("A través del tiempo, por país"),
                                                   
                                                   plotOutput("organizadores"),
                                                   
                                                   h4("Profundizando en la variedad de organizadores de un debate"),
                                                   
                                                   p("Algo importante a tener en cuenta es que muchos debates son el resultado de la labor 
                                                   de más de un organizador. En este sentido, notamos que también al interior de cada debate 
                                                   ha aumentado la diversidad de organizadores; es decir, 
                                                     de manera cada vez más frecuente, los eventos son el producto de alianzas entre más de un actor 
                                                     y, en particular, 
                                                     de acuerdos entre entidades de diversa índole."),
                                                   p(em("El gráfico que sigue permite visualizar la diversidad de estas alianzas en el tiempo")),
                                                   
                                                   plotlyOutput("alianzas"),
                                                   
                                                   p("Finalmente, vale la pena considerar más específicamente el carácter de los organizadores de los debates.
                                                      De hecho, la que presentamos aquí es una categorización cuya base es inductiva: hemos primero identificado 
                                                     a las entidades que participan de la realización de un evento de manera relativamente acotada, 
                                                     y luego hemos procedido a agrupar la prularidad de etiquetas halladas en las cinco categorías 
                                                     anunciadas más atrás."),
                                                   p("De esta manera, como expone la tabla que sigue, podemos comparar los ", 
                                                     em("subtipos de organizadores que con mayor frecuencia organizan debates en cada país.")),
                                                   
                                                   tableOutput('subtipos')
                                                   ),
                                          
                                          tabPanel("Formatos", 
                                                   
                                                   h2("Formatos de los debates", align = "center"),
                                             
                                                   p("En su máxima amplitud, el ", em("formato "), "de un debate implica una combinación de múltiples decisiones estéticas, técnicas y, crucialmente, relativas a los lineamientos de la discusión.
                                                   Todas ellas pueden afectar la manera en que es emitido y receptado el encuentro y, por este motivo, 
                                                   suelen ser objeto de intensas disputas entre los equipos de campaña, periodistas y los organizadores."),
                                                   
                                                   p("De este abanico, nos parece interesante sistematizar la variación en dos subdimensiones: 
                                                     los patrones de interacción e intercambio, la una, 
                                                     la disposición temática, la otra."),
                                                   
                                                   h4("Esquemas de interacción"),
                                                   p(" "),
                                                   p("Por patrones de interacción e intercambio, nos referimos a los modos en los que se acuerda la participación de los candidatos, esto es, a los diálogos o exposiciones que se espera que estos entablen con base en las reglas negociadas de manera previa al encuentro.
                                                     En pocas palabras, esta subdimensión contempla si hay preguntas y, en caso afirmativo, quién las hace y cómo se espera que los candidatos hablen o respondan. "),

                                                   p("Para cubrir los debates examinados, construimos una variable categórica con 10 niveles, que pueden convivir o no dentro de una misma emisión. 
                                                   En primer lugar, se examina el tipo de intercambio propuesto entre los candidatos entre sí: puede haber “duelos” en tiempos y órdenes de interacción rígidos y pactados de antemano; o puede haber discusión “libre”."),
                                                   
                                                   img(src = 'duelo.png', height = 140, width = 290),
                                                   img(src = "libre.png", height = 140, width = 290),
                                                   p(" "),
                                                   p("En segundo lugar, observamos las interacciones propuestas entre los candidatos y distintos tipos de actores, si las hubiera. 
                                                    A veces preguntan los “moderadores” del evento"), 
                                                   
                                                   img(src = "moderador.png", height = 140, width = 290),
                                                   p(" "),
                                                   p("Puede haber un panel de “periodistas”, uno de “expertos” (destacados por sus credenciales académicas), 
                                                       o uno con representantes de “sectores” de la sociedad civil."),
                                                   
                                                   img(src = "periodistas.png", height = 140, width = 290),
                                                   img(src = "sectores.png", height = 140, width = 290),
                                                   p(" "),
                                                   p("Alternativamente, se autorizan a veces preguntas por parte del público, entendido como la masa indiferenciada de ciudadanos, 
                                                   sea de manera “virtual”, o sea encarnada en algunos individuos “presentes” en el piso."), 
                                                   
                                                   img(src = "virtuales.png", height = 140, width = 290),
                                                   img(src = "presentes.png", height = 140, width = 290),
                                                   p(" "),
                                                   p("Finalmente, existen debates que no proponen diálogo strictu sensu alguno, formato que calificamos de “expositivo”."),
                                               
                                                   h5("Variación en los esquemas de interacción"),
                                                 
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                   plotOutput("formatos_t"),
                                                   plotOutput("formatos_p")
                                                   ) ,
                                                   
                                                   h5("Conversión ordinal de esquemas de interacción"),
                                                   
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                       plotOutput("cuanti_c"),
                                                       plotOutput("cuanti_p")
                                                   ) ,
                                                   
                                                   h4("Distribución temática"),
                                                   
                                                   p("la disposición temática remite al modo en que la discusión y/o las exposiciones de los candidatos son encauzadas (o no) hacia uno o varios tópicos o problemáticas particulares. 
                                                     No interesa tanto cuáles son los temas, sino cómo se decide sobre qué hablarán los candidatos. 
                                                     Encontramos cuatro posibilidades: 
                                                    (1) los candidatos pueden hablar sobre lo que quieran –el tema es “libre”–, 
                                                     (2) la discusión se organiza en algunos “bloques” temáticos muy generales, 
                                                     o (3) incluso versa sobre un único tema –en un debate “monotemático”–, 
                                                     o, por último, (4) lo dicho por los candidatos puede pautarse mediante la realización de “preguntas” muy específicas por parte de terceros a los candidatos."),
                                                   
                                                   h5("Variación en la estructuración temática de los debates"),
                                                   
                                                   
                                                   splitLayout(
                                                       cellArgs = list(style = "padding: 6px"),
                                                       plotOutput("temas_t"),
                                                       plotOutput("temas_p")
                                                   ) 
                                                   ),
                                          
                                          tabPanel("Normativa", 
                                                   
                                                   h2("Normativa en la materia", align = "center"),
                                                   
                                                   p("Si bien durante años los debates fueron librados
                                                     al acuerdo entre las partes -candidatos, organizadores, medios-,
                                                     al día de hoy muchos países han sancionado regulaciones. Así, "
                                                     , em("la normativa"), "constituye otra dimensión en la cual estudiar la variación."),
                                                   p("En esta sección exponemos la información recabada a este respecto"),
                                                   h4("Espíritu de la norma"),
                                                   h5("desde el punto de vista de..."),
                                                   
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
                            h2("Análisis de clusters", align = "center"),
                            
                            tags$div(
                                tags$p("El análisis de clusters se utiliza para agrupar “objetos” 
                                conforme a sus características. 
                                Es es una familia de técnicas cuyo proceder se basa en algún algoritmo 
                                que, en etapas sucesivas, 
                            compara alguna medida de la similitud de las observaciones 
                            a lo largo de más de una variable y une a las que más se parecen, 
                            repitiendo el procedimiento hasta arribar a algún número predeterminado de grupos (“clusters”)."),
                                
                                tags$p("En nuestro caso, se trata de comparar, clasificar y reunir a los países de nuestra muestra 
                            conforme a los modos en que en ellos se han realizado los debates presidenciales televisados.
                            Hemos optado por seguir un método jerárquico de agregación de los resultados."),  
                            
                                tags$p("Aquí se puede expermientar con distintas especificaciones del modelo: 
                              seleccionar tanto diferentes indicadores 
                              con base en los cuales comparar los países, 
                              y utilizar dos abordajes diferentes 
                              para medir la similitud entre los sucesivos grupos.") ),
                        
                            sidebarLayout(
                                sidebarPanel(
                                    h4("Selección de indicadores"),
                                    p("En el mapa y la tabla a continuación se puede contrastar
                               la información que proveen distintos indicadores agregados"),
                                    selectInput(inputId = "selector",
                                                label = "Vizualizar indicador...", 
                                                choices = colnames(base_cluster_pais %>% select(-cat_pais)),
                                                selected = "n_indexausentes", 
                                                multiple = F),
                                    
                                    actionButton("action_interdependencia2", 
                                                 "Explorar indicador")
                                    ),
                                    mainPanel(
                                    plotOutput("plot_mapa"),
                                    tableOutput('tabla_indicadores')
                                    )
                                 ),
                            
                            sidebarLayout(
                                sidebarPanel(
                                    h4("Especificación del modelo"),
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
                                                 "Ejecutar análisis")
                                        ),
                                    mainPanel(
                                    h4("Resultado del análisis de clusters"),
                                    p("El dendograma a continuación muestra el resultado de la 
                                      especificación elegida:"),
                                    plotOutput("plot_cluster")
                            )
                            )
                          )
                          ),
                 
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
                                      
                                      dataTableOutput('tabla_codebook'),
                                      plotOutput("hist_codebook"),
                                      dataTableOutput('tabla_summary')
                                      
                                  )
                              )
                          )))
    
 

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # OUTPUTS DIMENSIONES #########
    
    # presentacion
  
    output$presentaciondims <- renderDataTable(
      
      dims %>% 
        rename("Nivel / Unidad de observación" = "Unidad",
               "Indicadores y categorías" = "Indicadores",
               "Escala de medición" = "Escala",
               "Sub-dimensiones" = "Sub.dimensiones")
    )
    
    # arraigo
    output$slider <- renderUI({
        
        df_slider <- base %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais)
        sliderInput("selec_t","Elija un período", 
                    min = min(df_slider$ncat_eleccion), 
                    max = max(df_slider$ncat_eleccion),
                    value = c(min(df_slider$ncat_eleccion),     
                              max(df_slider$ncat_eleccion)),
                    step = 1,
                    sep = "")
        })
    
    df.filt_base_dimensiones <- eventReactive(input$action_dimensiones, {
        df.filt <- base %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] )
    })    
    
    df.filt_base_anual <- eventReactive(input$action_dimensiones, {
        df.filt <- base_anual %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] )
    })
    
    # mensajito de error 
     
     output$text_error <- renderText({ 
         
         if(nrow(df.filt_base_dimensiones())==0){
         
             expr = "Ups! Parece que no hay datos. No hay debates en el período para los países seleccionados."
         }
         else{}
         })

    output$ev_anual <- renderPlotly({
        
        if(nrow(df.filt_base_anual())>0) {

        ggplotly(
            df.filt_base_anual() %>%  
                     ggplot(aes(ncat_eleccion, 
                                n_debates_ano_pais,
                                colour = cat_pais))  +
                     geom_line() + 
                     geom_point(aes(size= n_debates_ano_pais, shape = debates_dico, alpha= debates_dico)) +
                     scale_color_manual(breaks= ccodes$cat_pais2,
                                        values= ccodes$cols_18) +
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
        
            }
        else{}
    
        })
    
    output$ausencias <- renderPlot({
       
        if(nrow(df.filt_base_dimensiones())>0) {
            
         df.filt_base_dimensiones() %>% 
        filter(ncat_ronda == 1) %>% 
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
        }
        else{}
        
    })
    
    # organizadores
    
    df.filt_base_organizadores <- eventReactive(input$action_dimensiones, {
        df.filt <- base_organizadores %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$organizadores <- renderPlot({
        
        if(nrow(df.filt_base_organizadores())>0) {
            
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
        }
        else{}
            
   })
    
    output$alianzas <- renderPlotly({
        
        if(nrow(df.filt_base_organizadores())>0) {
            
        ggplotly(
        df.filt_base_organizadores()  %>%  
            group_by(id_debate, ncat_eleccion, cat_pais) %>% 
            summarise(n_orgs = n(),
                      n_variedadorgs = n_distinct(cat_tipoorgv2))  %>% 
            ggplot() +
            geom_point(aes(ncat_eleccion, n_variedadorgs, 
                           size = n_orgs,
                           colour = cat_pais),
                       alpha = 0.6 ) +
            theme_minimal() +
            scale_y_continuous(breaks = seq(0,15,1)) +
            scale_x_continuous(breaks = seq(1955,2021,5)) +
            scale_color_manual(breaks= ccodes$cat_pais2,
                               values= ccodes$cols_18) +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
          labs(x = "Año de elección", 
               y = "n tipos de organizador")) %>%
            layout(title = list(text = paste0("Variedad de organizadores por debate",
                                              '<br>',
                                              '<sup>',
                                              "en el tiempo",
                                              '</sup>')),
                   margin = list(b=220), 
                   annotations = list(x = 0.95, y = -0.75, #position of text adjust as needed 
                   text = "Elaboración propia.
                 Cada círculo representa un debate, su tamaño, 
                 la cantidad total de entidades que lo organizaron.",
                                      showarrow = F, xref='paper', yref='paper', 
                                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                      font=list(size=10, color="grey")))

        }
        else{}
    })
    
    output$subtipos <- renderTable({
      
      if(nrow(df.filt_base_organizadores())>0) {
        
        df.filt_base_organizadores()  %>%  
          group_by(cat_pais) %>% 
          mutate(n_debates_pais = n_distinct(id_debate),
                 n_organizadores_pais = n() ) %>% 
          ungroup() %>% 
          group_by(cat_pais, cat_tipoorgv2) %>% 
          mutate(n_tipo_pais = n(),
                 n_debates_tipo_pais = n_distinct(id_debate),
                 pr_debates_tipo_pais = n_debates_tipo_pais/n_debates_pais) %>% 
          ungroup() %>% 
          group_by(cat_tipoorgv2, ncat_subtipov2, cat_pais) %>% 
          summarise(#n_subtipo_pais = n(),
                    n_debates_con_subtipo_pais = n_distinct(id_debate),
                    n_debates_tipo_pais = mean(n_debates_tipo_pais),
                    n_debates_pais = mean(n_debates_pais),
                    pr_debates_tipo_sobre_totdebates = round(n_debates_tipo_pais/n_debates_pais, 3)*100,
                    pr_debates_con_subtipo_pais_sobre_debatestipo = round(n_debates_con_subtipo_pais/n_debates_tipo_pais, 3)*100,
                    pr_debates_con_subtipo_pais_sobre_totdebates = round(n_debates_con_subtipo_pais/n_debates_pais, 3)*100) %>% 
          arrange(cat_pais, cat_tipoorgv2, ncat_subtipov2) %>% 
          rename(Pais = "cat_pais",
                 "Tipo de organizador" = "cat_tipoorgv2",
                 "Subtipo de orgnizador"= "ncat_subtipov2",
            "n debates con subtipo en país" = "n_debates_con_subtipo_pais",
            "n debates con tipo en país" = "n_debates_tipo_pais",
            "total debates país" = "n_debates_pais",
            "% debates tipo / total" = "pr_debates_tipo_sobre_totdebates",
            "% debates subtipo / tipo" = "pr_debates_con_subtipo_pais_sobre_debatestipo",
            "% debates subtipo / total" = "pr_debates_con_subtipo_pais_sobre_totdebates")
      }
      else{}
    },         
    hover = T, 
    digits = 0 )  
    
    # formatos 
    
    df.filt_formatos <- eventReactive(input$action_dimensiones,{
        df.filt <- base_formatos %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$formatos_t <- renderPlot({
        
        if(nrow(df.filt_formatos())>0) {
            
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
        }
        else{}
    
    })   
    
    output$formatos_p <- renderPlot({
        
        if(nrow(df.filt_formatos())>0) {
            
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
            scale_color_manual(breaks= ccodes$cat_pais,
                               values= ccodes$cols_18) +
            scale_y_discrete(
                breaks = c("pr_formatopresentes", "pr_formatovirtuales", "pr_formatosectores",
                           "pr_formatoexpertos", "pr_formatomoderadores","pr_formatoperiodistas", "pr_formatoapertura",
                           "pr_formatoexpositivo", "pr_formatoduelo", "pr_formatolibre"),
                labels = c("Público-presente", "Público-virtual", "Panel-Sectores", "Panel-Expertos", "Moderadores", "Periodistas", "Apertura", "Expositivo", "Duelo", "Libre")) +
            labs(x = "",
                 y = "",
                 title = "por país",
                 caption = "")
        }
        else{}
    })   
    
    df.filt_temas <- eventReactive(input$action_dimensiones,{
        df.filt <- base_temas %>% 
            filter(cat_pais == input$selec_pais | cat_pais %in% input$selec_pais) %>% 
            filter(ncat_eleccion >= input$selec_t[1] & ncat_eleccion <= input$selec_t[2] ) 
        df.filt
    }) 
    
    output$temas_t <- renderPlot({
        
        if(nrow(df.filt_temas())>0) {
            
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
        }
        else{}
    }) 
    
    output$temas_p <- renderPlot({
        
        if(nrow(df.filt_temas())>0) {
            
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
            scale_color_manual(breaks= ccodes$cat_pais,
                               values= ccodes$cols_18) +
            scale_y_discrete(
                breaks = c("pr_temabloques", "pr_temalibre", "pr_temamonotema", "pr_temapuntuales"),
                labels = c("En bloques", "Libre", "Monotemático", "Interrogantes puntuales")) +
            labs(x = "",
                 y = "",
                 title = "por país",
                 caption = "")
        }
        else{}
        
    }) 
    
    output$cuanti_c <- renderPlot({
        
        if(nrow(df.filt_base_dimensiones())>0) {
            
        df.filt_base_dimensiones() %>% 
            ggplot() +
            geom_boxplot(aes(cat_pais, 
                             as.numeric(ncat_competencia), 
                             fill = cat_pais)) +
            scale_fill_manual(breaks= ccodes$cat_pais,
                              values= ccodes$cols_18) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90))  +
            labs(x = "",
                 y = "",
                 title = "N° de competencia")
        }
        else{}
    }) 
    
    output$cuanti_p <- renderPlot({
        
        if(nrow(df.filt_base_dimensiones())>0) {
            
        df.filt_base_dimensiones() %>% 
            ggplot() +
            geom_boxplot(aes(cat_pais,
                             as.numeric(ncat_ppac), 
                             fill = cat_pais)) +
            scale_fill_manual(breaks= ccodes$cat_pais,
                              values= ccodes$cols_18) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90))  +
            labs(x = "",
                 y = "",
                 title = "N° de participación")
        }
        else{}
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
    
    df.filt.mapa <- eventReactive(input$action_interdependencia2, {
        
        df.filt <- mapear %>%
            select(c(long, lat, group, ccode, cat_pais, input$selector)) %>% 
            rename(plotear = input$selector)
        df.filt 
        
    })  
    
    output$plot_mapa <- renderPlot({
    
        df.filt.mapa() %>% 
        ggplot() +
        geom_polygon(aes(x = long, 
                         y = lat, 
                         group = group, 
                         colour = ccode, 
                         alpha = plotear),
                     fill = "black") + 
            scale_colour_manual(breaks = ccodes$ccode,
                                values = ccodes$cols_18) +
         coord_fixed(1) +
        theme_void() +
        theme(legend.position = "none")
        
    })
    
    df.filt.cluster_tabla <- eventReactive(input$action_interdependencia2, {
        
        df.filt <- codebook_cluster_pais %>%  
            filter(Indicador == input$selector)
        df.filt 
        
    })  
    
    output$tabla_indicadores <- renderTable( 
        df.filt.cluster_tabla(), hover = T
    )
    
    output$plot_cluster <- renderPlot({
        df.filt.cluster() %>% 
            set("labels_col", value = c(brewer.pal(n = 9, name = "Paired")), k=9) %>%
            set("branches_lty", 1) %>%
            set("branches_k_color", value =  c(brewer.pal(n = 9, name = "Paired")), k=9) %>% 
            plot(axes = F)
    }) 
    
    # OUTPUTS CODEBOOK ########
    
    df.filt_codebook <- eventReactive(input$action_codebook,{
        df.filt <- codebook %>% 
            filter(Variable == input$selec_variable) 
        df.filt
    })
    
    df.filt_columna <- eventReactive(input$action_codebook,{
        df.filt <- isolate(base %>% 
            select(input$selec_variable) %>% 
            as_tibble())
    })
    
    output$tabla_codebook <- renderDataTable(df.filt_codebook())
    
    output$hist_codebook <- renderPlot({
        
        input$action_codebook
        isolate(
        if (!startsWith(colnames(df.filt_columna())[1],"str")){
        df.filt_columna() %>% 
            ggplot(aes_string(names(df.filt_columna())[1])) +
            geom_bar() +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  axis.text.x = element_text(angle = 90)) +
            labs(x = "",
                 y = "count",
                 title = "Distribución de la variable")}
        else{
            str <-  df.filt_columna() %>%
                unnest_tokens(word, input$selec_variable)
            words <- str %>% count(word, sort=TRUE) %>% 
                subset(str_length(word)>2)
            
            wordcloud(words = words$word, freq = words$n, min.freq = 1,           
                      max.words=200, random.order=FALSE, rot.per=0.35,            
                      colors=brewer.pal(8, "Dark2"))
            })
    })
    
    output$tabla_summary <- renderDataTable(as.data.frame(apply(df.filt_columna(),2,summary))
                                                )       
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
