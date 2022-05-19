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
                      dashboardSidebar(
                          dateRangeInput("fechaReporte", 
                                         "Fechas de reporte por día de creación", 
                                         start = "2020-01-01", 
                                         end = Sys.Date(),
                                         min="2020-01-01", 
                                         max = Sys.Date(),
                                         format = "dd/mm/yyyy", 
                                         language = "es",
                                         separator = "a"),
                          
                          selectInput("DASFilter",
                                      "DAS",
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
                          selectInput("DMSFilter",
                                      "DMS",
                                      multiple = TRUE,
                                      selected = 'TODOS',
                                      choices = 'TODOS'),
                          selectInput("VarianteFilter",
                                      multiple = TRUE,
                                      selected = 'TODOS',
                                      "Variante detectada",
                                      choices = c("TODOS")
                          ),
                          sidebarMenu(
                              menuItem("Vigilancia", tabName = "vigilancia", icon=icon("head-side-virus")),
                              menuItem("Reporte", tabName = "reporte", icon=icon("file")) 
                              )
                      ),
                      dashboardBody(
                          tabItems(
                              tabItem(
                                  tabName = "vigilancia",
                                  HTML('<center><img src="mspas.png", height = "140"><img src="lns.png", height = "140"></center>'),
                                  #img(src = "lns.png", height = 140, align = 'center'),
                                  h2("Vigilancia genómica de SARS-CoV-2 en Guatemala"),
                                  fluidRow(
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
                                            title = 'Variantes por fecha de ingreso de muestra',
                                            withLoader(plotlyOutput('variantesPorSemanaEpidemiologicaLNS'), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorSemanaEpidemiologicaDBLNSIngreso"), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Variantes por fecha de toma de muestra',
                                            withLoader(plotlyOutput('variantesPorSemanaEpidemiologicaLNSToma'), type = 'html', loader = 'loader5')
                                          ),
                                          tabPanel(
                                            title = 'Cuadro de Datos',
                                            withLoader(DT::dataTableOutput("variantesPorSemanaEpidemiologicaDBLNSToma"), type = 'html', loader = 'loader5')
                                          )
                                      )
                                  ),
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
                                            title = 'Variantes del SARS-CoV-2 identificadas',
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
                                  
                              )
                              #NEXT TAB CONTENT STARTS HERE
                          )
                      ),
                      
                      
                      
))
