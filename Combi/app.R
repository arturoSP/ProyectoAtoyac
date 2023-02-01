library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(broom)
library(Kendall)
library(DT)
library(stringr)
source("data.R")
source("utils.R")

# User interface ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "journal"),
  waiter::use_waiter(),
  
  #título de app ----
  titlePanel("Tasa bruta de mortalidad por ECNT en la Cuenca del Alto Atoyac"),
  
  # Sidebar en dónde se ponen los inputs ----
  sidebarLayout(
    sidebarPanel(
      textInput("Causa1", label=("Letra de CIE10"), value="N"),
      numericInput("Causa2Min", label=("Número de CIE10"), value=18, min= 0, max=99,step=1),
      #numericInput("Causa2Min", label=("MaxdelRango"), value=18, min= 0, max=99,step=1),
      selectInput("EdadObj", label=("Edad"), 
                  choices=list("0-9", "10-19", "20-24",
                               "25-59", "60+"), selected="20-24", multiple = T),
      checkboxInput("juntos", "¿Juntos?", value = F, width = "100%"),
      actionBttn("update", "Actualizar", icon = icon("sync"), 
                 style = "simple", color = "danger", block = T)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Introducción",
                 fluidRow(column(12,
                                 includeHTML("./www/Instrucciones.html"))),
                 fluidRow(column(12,
                                 h3("Búsqueda de código CIE10"))),
                 fluidRow(column(4,
                                 textInput("ECNT", 
                                           label = NULL, 
                                           placeholder = "Nombre de la enfermedad")),
                          column(8,
                                 dataTableOutput("TablaCIE10"))),
                 fluidRow(column(12,
                                 br(),
                                 h6("Esta aplicación web fue desarrollada en Shiny y R. Diseño 
                                        y código por Arturo Sánchez Porras y Aline Romero Natale.")))),
        tabPanel("Serie", 
                 fluidRow(column(12, 
                                 withSpinner(dataTableOutput("Tabla0"), 
                                             type = 6, color = "red"))),
                 br(),
                 fluidRow(column(9),
                          column(3, downloadBttn("descargaSr", "Descargar serie de tiempo", icon = icon("download"),
                                                 style = "simple", color = "primary", block = T)))),
        tabPanel("Promedios", 
                 fluidRow(column(12,
                                 withSpinner(dataTableOutput("Tabla1"), 
                                             type = 6, color = "red"))),
                 br(),
                 fluidRow(column(9),
                          column(3, downloadBttn("descargaMn", "Descargar promedios", icon = icon("download"),
                                                 style = "simple", color = "primary", block = T)))),
        tabPanel("Medianas", 
                 fluidRow(column(12,
                                 withSpinner(dataTableOutput("Tabla2"), 
                                             type = 6, color = "red"))),
                 br(),
                 fluidRow(column(9),
                          column(3, downloadBttn("descargaMd", "Descargar medianas", icon = icon("download"),
                                                 style = "simple", color = "primary", block = T)))),
        tabPanel("Pendientes", 
                 fluidRow(column(12,
                                 withSpinner(dataTableOutput("Tabla3"), 
                                             type = 6, color = "red"))),
                 br(),
                 fluidRow(column(9),
                          column(3, downloadBttn("descargaPd", "Descargar pendientes", icon = icon("download"),
                                                 style = "simple", color = "primary", block = T)))),
        tabPanel("Mann-Kendall", 
                 fluidRow(column(12,
                                 withSpinner(dataTableOutput("Tabla4"), 
                                             type = 6, color = "red"))),
                 br(),
                 fluidRow(column(9),
                          column(3, downloadBttn("descargaMK", "Descargar Mann-Kendall", icon = icon("download"),
                                                 style = "simple", color = "primary", block = T))))
      )
    )
  )
)

server <- function(input, output, session) {
  # Hoja cero, tabla de códigos CIE10 ----
  output$TablaCIE10 <- renderDT(codigo %>%
                                  mutate(ubicado = str_detect(Grupo, 
                                                              regex(input$ECNT, 
                                                                    ignore_case = T))) %>%
                                  filter(ubicado == T) %>%
                                  select(CIE10, Grupo))
  
  # Primera hoja, serie de tiempo ----
  Serie1 <- eventReactive(input$update,
                           if(input$juntos == F) {
                             ValSeparados(input$Causa1, input$Causa2Min, input$Causa2Min, input$EdadObj)
                           } else {
                             ValJuntos(input$Causa1, input$Causa2Min, input$Causa2Min, input$EdadObj)
                           })
  output$Tabla0 <- renderDT(Serie1() %>%
                              pivot_wider(names_from = "Anio", values_from = "TBM"),
                            options = list(pageLength = 10,
                                           lengthMenu = c(10, 30, 67),
                                           scrollX = T))
  output$descargaSr <- downloadHandler(
    filename = function(){
      paste0("Serie_Edades_", input$Causa1, input$Causa2Min, "-", input$Causa2Min, ".csv")
    },
    content = function(file){
      write.csv(Serie1(), file)
    }
  )
  
  # Hoja 2, promedios ----
  TablaMn <- eventReactive(input$update,
                           if(input$juntos == F){
                             PromSeparados(Serie1())
                           } else{
                             PromJuntos(Serie1())
                           })
  output$Tabla1 <- renderDT(TablaMn(),
                            options = list(pageLength = 10,
                                           lengthMenu = c(10, 30, 67),
                                           scrollX = T))
  output$descargaMn <- downloadHandler(
    filename = function(){
      paste0("Promedio_Edades_", input$Causa1, input$Causa2Min, "-", input$Causa2Min, ".csv")
    },
    content = function(file){
      write.csv(TablaMn(), file)
    }
  )
  
  # Hoja 3, medianas ----
  TablaMd <- eventReactive(input$update,
                           if(input$juntos == F){
                             MedianSeparados(Serie1())
                           } else{
                             MedianJuntos(Serie1())
                           })
  output$Tabla2 <- renderDT(TablaMd(),
                            options = list(pageLength = 10,
                                           lengthMenu = c(10, 30, 67),
                                           scrollX = T))
  output$descargaMd <- downloadHandler(
    filename = function(){
      paste0("Mediana_Edades_", input$Causa1, input$Causa2Min, "-", input$Causa2Min, ".csv")
    },
    content = function(file){
      write.csv(TablaMd(), file)
    }
  )
  
  # Hoja 4, pendientes ----
  TablaPd <- eventReactive(input$update,
                           if(input$juntos == F){
                             PendienteSeparados(Serie1())
                           } else{
                             PendienteJuntos(Serie1())
                           })
  output$Tabla3 <- renderDT(TablaPd(),
                            options = list(pageLength = 10,
                                           lengthMenu = c(10,30, 67),
                                           scrollX = T))
  output$descargaPd <- downloadHandler(
    filename = function(){
      paste0("Pendiente_Edades_", input$Causa1, input$Causa2Min, "-", input$Causa2Min, ".csv")
    },
    content = function(file){
      write.csv(TablaPd(), file)
    }
  )
  
  # Hoja5, Mann-Kendall ----
  TablaMK <- eventReactive(input$update,
                           if(input$juntos == F){
                             MannKendallSeparados(Serie1())
                           } else{
                             MannKendallJuntos(Serie1())
                           })
  output$Tabla4 <- renderDT(TablaMK(),
                            options = list(pageLength = 10,
                                           lengthMenu = c(10, 30, 67),
                                           scrollX = T))
  output$descargaMK <- downloadHandler(
    filename = function(){
      paste0("MannKendall_Edades_", input$Causa1, input$Causa2Min, "-", input$Causa2Min, ".csv")
    },
    content = function(file){
      write.csv(TablaMK(), file)
    }
  )
}


# Corre la aplicación ----
shinyApp(ui = ui, server = server)
  