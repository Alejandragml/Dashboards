library(RODBC)
library(tidyverse)
library(rsconnect)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(flexdashboard)
library(shinydashboardPlus)
library(data.table)
library(DT)


load("query.RData")


ui <-dashboardPage( skin = "blue",
                    
                    dashboardHeader(title="Tablero de Indicadores Comerciales",titleWidth = 450),
                    
                    dashboardSidebar( 
                      tags$head( tags$style (HTML (".sidebar .sidebar-menu .treeview-menu> li> a> .fa { display: none;}"))),
                      sidebarMenu(id = "Impust",
                          menuItem("Seleccionar Datos a Consultar",       
                                selectInput(inputId = "Cate",label = "Seleccione una Categoria:",choices = levels(query$CATEGORIA), width = '100%'),
                                selectInput(inputId = "Fam",label = "Seleccione una Familia:",choices = levels(query$FAMILIA), width = '100%'),
                                selectInput(inputId = "Ofi",label = "Seleccione una Oficina:",choices = levels(query$OFICINA_VENTAS), width = '100%'),
                                selectInput(inputId = "Can",label = "Seleccione un Canal:",choices = levels(query$CANAL), width = '100%'),
                                selectInput(inputId = "Mar",label = "Seleccione una Marca:",choices = levels(query$MARCA), width = '100%'),
                                selectInput(inputId = "Ven",label = "Seleccione un Vendedor:",choices = levels(query$VENDEDOR), width = '100%'),
                                selectInput(inputId = "Tip",label = "Seleccione un Tipo de Ventas:",choices = levels(query$TIPO_VENTAS), width = '100%')
                          )
                       )
                    ),
                    
                    dashboardBody(
                    
                      navbarPage(
                                 tags$head(tags$style(HTML(".tab-panel{ background-color: red; color: white}"))),
                                 tabPanel(strong("Indice"),icon = icon("folder"),
                                        box( align="center",width = 1000, tags$p( style = "font-size: 220%;"),
                                         h4(p(strong("Resumen General de Indicadores:"))),
                                         p("- CcC, RC, TP, Frec"),
                                         p("-  Mix de Familia y Profundidad"),
                                         br(),
                                         h4(p(strong("Indicadores:"))),
                                         p("-Clientes con Compra (CcC): Número de clientes que compraron en un determinado mes."),
                                         p("- Recompra (CcR): Porcentaje de clientes que han comprado más de 1 vez dentro del mismo mes sobre la base de CcC."),
                                         p("- Ticket promedio (TP): Compra promedio en $ por pedido/desembolso en el mes."),
                                         p("- Frecuencia de compra (Frec.): N° de veces que en promedio compran los clientes en el mismo mes."),
                                         br(),
                                         h4(p(strong("Mis de Familias y Profundidad:"))),
                                         p("- Mix de Familia:  Número de FAMILIAS que en promedio tiene un PDV atendido por un canal."),
                                         p("- Profundidad: Número de SKU's que en promedio tiene un PDV atendido por un canal."))
                                         
                                         ),
                                 tabPanel(strong("Resumen"),icon = icon("chart-bar"),
                                          fluidRow( 
                                            tags$head(tags$style(HTML(".fa { font-size: 25px; }"))),
                                            tags$head(tags$style(HTML(".fa{color:#848484}"))),
                                            tags$head(tags$style(HTML(".small-box {height: 90px}"))),
                                            flexdashboard::valueBoxOutput(outputId = "compra",width = 2),
                                            flexdashboard::valueBoxOutput(outputId = "recompra",width = 2),
                                            flexdashboard::valueBoxOutput(outputId = "frecuencia",width = 2),
                                            flexdashboard::valueBoxOutput(outputId = "tp",width = 2),
                                            flexdashboard::valueBoxOutput(outputId = "amp",width = 2),
                                            flexdashboard::valueBoxOutput(outputId = "pro",width = 2)
                                          ),
                                          fluidRow(class = "text-center",align="center",
                                                  dataTableOutput(outputId = "table")
                                          ),
                                          fluidRow( align="center",  h3("Variaciones Vs. el Mes Anterior"),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_ccc",width = 150)),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_ccr",width = 150)),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_frec",width = 150)),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_tp",width = 150)),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_amp",width = 150)),
                                                    column(2,align="center",flexdashboard::gaugeOutput("var_prof",width = 150))
                                          )
                                 ),
                                 tabPanel(strong("Indicadores"),icon = icon("chart-bar"),align="center",  h3("CcC, CcR, FREC y TP"),
                                          fluidRow(class = "text-center",
                                                  plotlyOutput(outputId = "grafico_ccc",height = 150)
                                          ),
                                          fluidRow(class = "text-center",
                                                   plotlyOutput(outputId = "grafico_ccr",height = 150)
                                          ),
                                          fluidRow(class = "text-center",
                                                   plotlyOutput(outputId = "grafico_frec",height = 150)
                                          ),
                                          fluidRow(class = "text-center",
                                                   plotlyOutput(outputId = "grafico_tp",height = 150)
                                          )
                                 ),
                                 tabPanel(strong("Mix de Familias y Profundidad"),icon = icon("chart-bar"), align="center",  h3("Amplitud y Profundidad"),
                                          fluidRow(class = "text-center",
                                                   box(tags$p("Mix de Familias", style = "font-size: 120%;"),plotlyOutput(outputId = "grafico_amp",height = 160), width =1200 )
                                          ),
                                          fluidRow(class = "text-center",
                                                   box(tags$p("Profundidad", style = "font-size: 120%;"),plotlyOutput(outputId = "grafico_pro",height =160),width = 1200),
                                                  
                                                   
                                          )      
                                 )
                                 
                      )
                    )
                    
)


