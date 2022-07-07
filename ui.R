#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




# Define UI for application that draws a histogram

shinyUI(dashboardPage(skin = 'purple',
                      dashboardHeader(
                          titleWidth = 360,
                          title = "Vigilancia Genómica SARS-CoV-2"),
                      # Sidebar ----
                      dashboardSidebar(
                        ## Filtros ----
                        ### Fecha ----
                          dateRangeInput("fechaReporte", 
                                         "Fecha", 
                                         start = "2020-04-15", 
                                         end = Sys.Date(),
                                         min="2020-04-15", 
                                         max = Sys.Date(),
                                         format = "dd/mm/yyyy", 
                                         language = "es",
                                         separator = "a"),
                        ### Area de salud ----
                          selectInput("DASFilter",
                                      "Dirección de Área de Salud",
                                      multiple = TRUE,
                                      selected = 'TODOS',
                                      choices = c("TODOS", 
                                                  "ALTA VERAPAZ",
                                                  "BAJA VERAPAZ",
                                                  "CHIMALTENANGO",
                                                  "CHIQUIMULA",
                                                  "EL PROGRESO",
                                                  "ESCUINTLA",
                                                  "GUATEMALA CENTRAL",
                                                  "GUATEMALA NOR-OCCIDENTE",
                                                  "GUATEMALA NOR-ORIENTE",
                                                  "GUATEMALA SUR",
                                                  "HUEHUETENANGO",
                                                  "IXCÁN",
                                                  "IXIL",
                                                  "IZABAL",
                                                  "JALAPA",
                                                  "JUTIAPA",
                                                  "PETÉN NORTE",
                                                  "PETÉN SUR OCCIDENTE",
                                                  "PETÉN SUR ORIENTE",
                                                  "QUETZALTENANGO",
                                                  "QUICHÉ",
                                                  "RETALHULEU",
                                                  "SACATEPÉQUEZ",
                                                  "SAN MARCOS",
                                                  "SANTA ROSA",
                                                  "SOLOLÁ",
                                                  "SUCHITEPÉQUEZ",
                                                  "TOTONICAPÁN",
                                                  "ZACAPA" )
                          ),
                        ### Unidad notificadora ----
                          selectInput("DMSFilter",
                                      "Unidad notificadora",
                                      multiple = TRUE,
                                      selected = 'TODOS',
                                      choices = 'TODOS'),
                        ### Variante detectada ----
                          selectInput("VarianteFilter",
                                      multiple = TRUE,
                                      selected = 'TODOS',
                                      "Variante detectada",
                                      choices = c("TODOS")
                          ),
                        ## Secciones ----
                          sidebarMenu(
                              menuItem("Vigilancia", tabName = "vigilancia", icon=icon("head-side-virus")),
                              menuItem("Información", tabName = "informacion", icon=icon("info")) 
                              )
                      ),
                      # Dashboard ----
                      dashboardBody(
                          tabItems(
                            ## Vigilancia ----
                              tabItem(
                                  tabName = "vigilancia",
                                  HTML('<center><img src="mspas.png", height = "140"><img src="lns.png", height = "140"></center>'),
                                  #img(src = "lns.png", height = 140, align = 'center'),
                                  h2("Vigilancia genómica de SARS-CoV-2 en Guatemala"),
                                  fluidRow(
                                    ### Semana epidemiológica ----
                                      tabBox(
                                          width = 12,
                                          height = 600,
                                          #tabPanel(
                                          #    title = 'Variantes por semana epidemiológica',
                                          #    withLoader(plotlyOutput('variantesPorSemanaEpidemiologica'), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Cuadro de Datos',
                                          #    withLoader(DT::dataTableOutput("variantesPorSemanaEpidemiologicaDB"), type = 'html', loader = 'loader5')
                                          #),
                                          tabPanel(
                                            title = 'Variantes por fecha de toma de muestra',
                                            withLoader(plotlyOutput('variantesPorSemanaEpidemiologicaLNSToma'), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorSemanaEpidemiologicaDBLNSToma"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Variantes por fecha de ingreso de muestra',
                                            withLoader(plotlyOutput('variantesPorSemanaEpidemiologicaLNS'), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorSemanaEpidemiologicaDBLNSIngreso"), type = 'html', loader = 'loader5')
                                          ),
                                      )
                                  ),
                                  ### Distribución proporcional ----
                                  fluidRow(
                                    tabBox(
                                      width = 12,
                                      height = 600,
                                      tabPanel(
                                        title = 'Distribución proporcional de variantes',
                                        withLoader(plotlyOutput('variantesDistribucionProporcional'), type = 'html', loader = 'loader5')
                                      ),
                                      tabPanel(
                                        title = 'Cuadro de datos',
                                        withLoader(DT::dataTableOutput('variantesDistribucionProporcionalDB'), type = 'html', loader = 'loader5')
                                      ),
                                    )
                                  ),
                                  ### Frecuencia de variantes ----
                                  fluidRow(
                                      tabBox(
                                          width = 12,
                                          #tabPanel(
                                          #    title = 'Variantes del SARS-CoV-2 identificadas',
                                          #    withLoader(plotlyOutput("variantesPangolin"), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Cuadro de Datos',
                                          #    withLoader(DT::dataTableOutput("variantesPangolinDB"), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Variantes de preocupación e interes de SARS-CoV-2 identificadas',
                                          #    withLoader(plotlyOutput("variantesOMS"), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Cuadro de Datos',
                                          #    withLoader(DT::dataTableOutput("variantesOMSDB"), type = 'html', loader = 'loader5')
                                          #),
                                          tabPanel(
                                            title = 'Variantes de SARS-CoV-2 identificadas',
                                            withLoader(plotlyOutput("variantesPangolinLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPangolinDBLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Variantes de preocupación e interes de SARS-CoV-2 identificadas',
                                            withLoader(plotlyOutput("variantesOMSLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesOMSDBLNS"), type = 'html', loader = 'loader5')
                                          ),
                                      )
                                  ),
                                  ### Ubicaciones geográficas ----
                                  fluidRow(
                                      tabBox(
                                          width = 12,
                                          #tabPanel(
                                          #    title = 'Mapa de muestras por departamento',
                                          #    withLoader(leafletOutput("mapaVariantesPorDepartamento", height = 800), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Muestras por departamento',
                                          #    withLoader(plotlyOutput("variantesPorDepartamento"), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Cuadro de Datos',
                                          #    withLoader(DT::dataTableOutput("variantesPorDepartamentoDB"), type = 'html', loader = 'loader5')
                                          #),
                                          tabPanel(
                                            title = 'Mapa de muestras por área de salud',
                                            withLoader(leafletOutput("mapaVariantesPorDASLNS", height = 800), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Mapa de muestras por área de salud con cantidades',
                                            withLoader(leafletOutput("mapaVariantesPorDASLNSNumero", height = 800), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Muestras por área de salud',
                                            withLoader(plotlyOutput("variantesPorDASLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorDASDBLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Mapa de muestras por departamento',
                                            withLoader(leafletOutput("mapaVariantesPorDepartamentoLNS", height = 800), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Mapa de muestras por departamento con cantidades',
                                            withLoader(leafletOutput("mapaVariantesPorDepartamentoLNSNumero", height = 800), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Muestras por departamento',
                                            withLoader(plotlyOutput("variantesPorDepartamentoLNS"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorDepartamentoDBLNS"), type = 'html', loader = 'loader5')
                                          ),
                                      )
                                  ),
                                  ### Grupo etario ----
                                  fluidRow(
                                      tabBox(
                                          width = 12,
                                          #tabPanel(
                                          #    title = 'Variantes por grupo etario',
                                          #    withLoader(plotOutput('variantesPorGrupoEtario'), type = 'html', loader = 'loader5')
                                          #),
                                          #tabPanel(
                                          #    title = 'Cuadro de datos',
                                          #    withLoader(DT::dataTableOutput('variantesPorGrupoEtarioDB'), type = 'html', loader = 'loader5')
                                          #),
                                          tabPanel(
                                            title = 'Variantes por grupo etario',
                                            withLoader(plotOutput('variantesPorGrupoEtarioLNS'), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de datos',
                                            withLoader(DT::dataTableOutput('variantesPorGrupoEtarioDBLNS'), type = 'html', loader = 'loader5')
                                          ),
                                      )
                                  ),
                                #   fluidRow(
                                #       box(
                                #           width = 12,
                                #           tabPanel(
                                #               title = 'Filogenia',
                                #               withLoader(plotOutput('filogenia', height = '600px'), type = 'html', loader = 'loader5')
                                #           )
                                #       )
                                #   )
                                  
                              ),
                            ## Información ----
                              tabItem(
                                tabName = "informacion",
                                HTML('<center><img src="mspas.png", height = "140"><img src="lns.png", height = "140"></center>'),
                                #img(src = "lns.png", height = 140, align = 'center'),
                                h2("Acerca del Tablero de Vigilancia genómica en Guatemala"),
                                fluidRow(
                                  ### Definiciones ----
                                  box(
                                    width = 12,
                                    title = 'Definiciones',
                                    tags$p(tags$b('Genoma:'), 'secuencia de ADN que posee un individuo (humano, bacteria, virus, etc)'),
                                    tags$p(tags$b('Mutación:'), 'los virus evolucionan y cambian constantemente. Cada vez que un virus se replica (es decir, hace copias de sí mismo), existe la posibilidad de que ocurra uno o varios cambios en su material genético. Cada uno de estos cambios es una "mutación".'),
                                    tags$p(tags$b('Linaje:'), 'agrupación genética del virus en relación a un ancestro en común.'),
                                    tags$p(tags$b('Variante:'), 'un virus con una o más mutaciones ya caracterizadas se denomina "variante" del virus original.'),
                                    tags$p(tags$b('Variante de Interés (VOI):'), 'variantes en las cuales se han identificado mutaciones que pueden estar asociados a cambios importantes en el comportamiento del virus (transmisión, gravedad de síntomas escape de la inmunidad, entre otros) o que fue categorizada como tal por la OMS.'),
                                    tags$p(tags$b('Variante de Preocupación (VOC):'), 'tiene las mismas características que una VOI, pero con evidencia científica del cambio en el comportamiento del virus (transmisión, gravedad de síntomas, escape de la inmunidad, entre otras) o que fuer categorizada como tal por la OMS.'),
                                    tags$p(tags$b('Secuenciación genética:'), 'La secuenciación genética permite leer el genoma de un virus.'),
                                    tags$p(tags$b('Vigilancia genómica:'), 'facilita el seguimiento y evaluación de mutaciones que puedan influir en el poder patógeno o de transmisión del virus.'),
                                    tags$p(tags$b('Semana epidemiológica:'), 'es un calendario para estandarizar la variable tiempo en semanas con fines de vigilancia epidemiológica.'),
                                    tags$br(),
                                    tags$p('Información obtenida de:'), 
                                    tags$ul(
                                      tags$li(tags$a(href='https://iris.paho.org/bitstream/handle/10665.2/53239/EpiUpdate26January2021_spa.pdf?sequence=2&isAllowed=y', 'Actualización epidemiológica: Variantes de SARS-CoV-2 en las Américas.')),
                                      tags$li(tags$a(href='https://apps.who.int/iris/bitstream/handle/10665/338892/WHO-2019-nCoV-genomic_sequencing-2021.1-spa.pdf?sequence=1&isAllowed=y', 'Secuenciación del genoma del SARS-CoV-2 con fines de salud pública.')),
                                      tags$li(tags$a(href='https://www.paho.org/es/variantes-sars-cov-2-covid-19-preguntas-frecuentes', 'Variantes del SARS-CoV-2 (COVID-19) - Preguntas frecuentes.')),
                                    )
                                  ),
                                  ### Estructura ----
                                  box(
                                    width = 12,
                                    title = "Estructura del tablero",
                                    tags$p("Cada una de las gráficas esta complementada por un cuadro de datos, con la opción de ser descargado;
                                           el tablero cuenta con 5 secciones:"),
                                    tags$ul(
                                      tags$li(
                                        tags$b('Variantes por semana epidemiológica:'),
                                        'se presentan los datos de las variantes detectadas en el país a lo largo del tiempo.', 
                                          tags$ul(
                                            tags$li(tags$b('Variantes por fecha de toma de muestra:'), 'variantes detectadas de acuerdo a la fecha en la que se le tomó muestra al paciente.'),
                                            tags$li(tags$b('Variantes por fecha de ingreso:'), 'variantes detectadas de acuerdo a la fecha de ingreso de la muestra al Laboratorio Nacional de Salud.')
                                          )
                                      ),
                                      tags$li(tags$b('Distribución proporcional de variantes:'), 'esta gráfica muestra la relación proporcional de las variantes detectadas a lo largo del tiempo.'),
                                      tags$li(
                                        tags$b('Frecuencia de variantes de SARS-CoV-2 detectadas en Guatemala:'),
                                        tags$ul(
                                          tags$li(tags$b('Variantes de SARS-CoV-2:'), 'el gráfico permite visualizar los 20 linajes con mayor frecuencia que se han identificado en el país.'),
                                          tags$li(tags$b('Variantes de preocupacion (VOC) e interes (VOI) del SARS-CoV-2:'), 'el gráfico agrupa las variantes identificadas de acuerdo a la nomenclatura propuesta por la Organización Mundial de la Salud.'),
                                        )
                                      ),
                                      tags$li(
                                        tags$b('Variantes por ubicación geográfica:'), 'se presenta la información en base a la distribución geográfica de detección de variantes. Los datos se han agrupado por Área de Salud (división administrativa del MSPAS) y por departamento (división política del país).',
                                        tags$ul(
                                          tags$li(tags$b('Mapa:'), 'este mapa presenta por medio de un gráfico de pie la proporción de variantes identificadas en cada Área de salud o departamento.'),
                                          tags$li(tags$b('Mapa con cantidades:'), 'este mapa presenta por medio de un gráfico de pi y con una leyenda el numero de variantes identificadas en cada Área de salud o departamento.'),
                                          tags$li(tags$b('Frecuencias:'), 'este gráfico muestra la frecuencia de variantes identificadas en cada Área de salud o departamento.'),
                                        )
                                      ),
                                      tags$li(tags$b("Distribución de variantes detectadas en Guatemala por sexo y grupo etario:"), 'distribución de las variantes de acuerdo a grupos de edad y sexo de las personas en donde se identificaron las variantes.')
                                    )
                                  ),
                                  ### Filtros ----
                                  box(
                                    width = 12,
                                    title = 'Información de los filtros',
                                    tags$p("En este tablero informativo se pueden aplicar 4 filtros distintos."),
                                    tags$ul(
                                      tags$li(tags$b('Fechas:'), 'permite visualizar los datos por diferentes períodos temporales.'),
                                      tags$li(tags$b('Dirección de Área de Salud (DAS):'), 'permite seleccionar una o muchas 
                                              Direcciones de Área de Salud que envían muestras para secuenciación de SARS-CoV-2.'),
                                      tags$li(tags$b('Distrito Municipal de Salud (DMS):'), 'permite seleccionar uno o muchos
                                               Distritos Municipales de Salud que envían muestras para secuenciación de SARS-CoV-2.'),
                                      tags$li(tags$b('Variante detectada:'), 'este filtro nos permite visualizar los datos de una o más
                                              variantes del SARS-CoV-2 identificadas en el país.'),
                                    ),
                                    tags$p(tags$b('IMPORTANTE:'), 'para aplicar múltiples filtros DAS, DMS o Variantes debe seguir los siguientes pasos:'),
                                    tags$ol(
                                      tags$li('Seleccionar una opción de las DAS, DMS o Variante que desea visualizar'),
                                      tags$li('Esperar a que el tablero se actualice'),
                                      tags$li('Si se desea agregar otra opción', 
                                              tags$ul( 
                                                tags$li('Repetir el paso 1')
                                              )
                                      ),
                                      tags$li('Si ya no se desea agregar más opciones',
                                              tags$ul(
                                                tags$li('Seleccionar la opción TODOS que aparece por defecto'),
                                                tags$li('Eliminarla con la tecla de borrar')
                                              )
                                      ),
                                      tags$li('Si se desean revertir los filtros',
                                              tags$ul(
                                                tags$li('Agregar la opción TODOS como en el paso 1 y 2'),
                                              )
                                      ),
                                      
                                    )
                                  ),
                                  
                                )
                              )
                              #NEXT TAB CONTENT STARTS HERE
                          )
                      ),
                      
                      
                      
))
