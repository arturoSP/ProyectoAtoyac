library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(plotly)
source("utils.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Indicadores de pobreza"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Seleccion",
                  label = "Escoge un nivel espacial",
                  choices = c("Municipios", "Microcuencas")),
      radioButtons("AnioSel",
                   label = "Selecciona un año",
                   choices = c(2010, 2015, 2020),
                   selected = 2020,
                   inline = T),
      selectInput("Indicador",
                  label = "Escoge un indicador",
                  choices = IndicOptions),
      actionButton("Update", "Actualizar")
      ),
    
    mainPanel(
      fluidRow(column(width = 12,
                      h3(textOutput("Subtitulo")))),
      fluidRow(column(width = 6,
                      leafletOutput("Mapa"),
                      imageOutput("Leyenda")),
               column(width = 6,
                      tabsetPanel(type = "tabs",
                                  tabPanel("Boxplot", 
                                           plotlyOutput("SerieTiempo", 
                                                        width = "100%", 
                                                        height = "90%")),
                                  tabPanel("Heatmap", 
                                           plotlyOutput("Pastel", 
                                                        width = "100%", 
                                                        height = "90%")))
                      )
               )
    )
  )
  
)

# Define server logic 
server <- function(input, output) {

  Seleccionado <- reactive({
    ListaIndicadores %>%
      filter(Etiqueta == input$Indicador) %>%
      select(Indicador) %>%
      as.character()
  })

  AnioSele <- reactive({input$AnioSel})
  
  Espacial <- reactive({input$Seleccion})

  # Gráfica temporal  
  SerieT <- eventReactive(input$Update,{
    if(Espacial() == "Municipios") {
      F_GrafSerie(Seleccionado())
    } else {
      F_GrafSerie_2(Seleccionado())
    }
    
  })

  output$SerieTiempo <- renderPlotly(SerieT())
  
  # Gráfica comparativa
  PastelG <- eventReactive(input$Update,{
    if(Espacial() == "Municipios") {
      F_GrafPastel(Seleccionado())
    } else {
      F_GrafPastel_2(Seleccionado())
    }
    
  })
  
  output$Pastel <- renderPlotly(PastelG())
  
  # Mapa interactivo
  MapaG <- eventReactive(input$Update, {
    if(Espacial() == "Municipios") {
      F_Mapa(Seleccionado(), AnioSele())
    } else {
      F_Mapa_2(Seleccionado(), AnioSele())
    }
  })
  
  output$Mapa <- renderLeaflet(MapaG())
  
  # Helpers

  Subtitulos <- eventReactive(input$Update, {
    input$Indicador
  })
  
  output$Subtitulo <- renderText(Subtitulos())
  
  Leyendas <- eventReactive(input$Update, {
    F_Leyenda(Seleccionado(), Espacial())
  })
  
  output$Leyenda <- renderImage({
    outfile <- Leyendas()
  }, deleteFile = F)
}
# A temp file to save the output. It will be deleted after renderImage
  # sends it, because deleteFile=TRUE.
# output$plot1 <- renderImage({
#   outfile <- tempfile(fileext='.png')
#   
#   png(outfile, width=400, height=400)
#   hist(rnorm(input$n))
#   dev.off()
#   
#   list(src = outfile,
#        alt = "This is alternate text")
# }, deleteFile = TRUE)


# Run the application 
shinyApp(ui = ui, server = server)
