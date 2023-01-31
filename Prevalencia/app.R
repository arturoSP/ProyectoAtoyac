# Análisis de prevalencias ----

# librerías -----

library(shiny)
library(tidyverse)
library(plotly)

# Definición de la interfaz de usuario ----
ui <- fluidPage(
  
  # título de aplicación
  titlePanel("Prevalencia"),
  
  # sidebar con espacio para poner inputs ----
  sidebarLayout(
    sidebarPanel (
      # selectInput("EdadObj", label=("Edad"), 
      #             choices=list("0-9","10-19","20-24","25-59","60+"), selected="20-24"),
      helpText("Puede seleccionar una o más enfermedades y a continuación hacer click en el botón \"Actualizar\", 
               para visualizar los valores de prevalencia para las distintas edades.",br(),"En la leyenda del gráfico se
               puede deseleccionar grupos de edad que no se quieran tener presentes."),
      actionButton("update", "Actualizar", icon("sync")),
      downloadButton("descargar", "Descargar tabla"),
      checkboxGroupInput("ECNT", "Enfermedad", selected = "Enfermedad renal", inline = TRUE,
                  choices = list("Enfermedad renal", "Leucemia",  
                                 "Tumor o cáncer en el sistema nervioso central: cerebro, encéfalo, médula espinal",
                                 "Malformación congénita", "Enfermedad del corazón", "Púrpura trombocitopénica",
                                 "Anemia", "Enfermedad de la tiroides",
                                 "Enfermedades en otras glándulas (suprarrenales, pituitarias)",
                                 "Pancreatitis", "Cirrosis hepática", "Hepatitis crónica no viral",
                                 "Enfisema", "Asma", "Bronquitis crónica (no contagiosa)",
                                 "Parkinson", "Alzheimer", "Esclerosis", "Daño en el cerebro",
                                 "Enfermedad cerebrovascular", "Demencia", "Autismo",
                                 "Linfoma de Hodgkin", "Linfoma de No-Hodgkin", "Melanomas (cáncer de piel)",
                                 "Tumor o cáncer en Órganos digestivos:esófago, estómago, intestinos, ano",
                                 "Tumor o cáncer en genitales masculinos",
                                 "Tumor o cáncer en órganos reproductivos de la mujer: ovarios, útero, vagina, vulva, cuello uterino",
                                 "Mama o seno", "Tumor o cáncer en Tiroides", "Tumor o cáncer en pulmón",
                                 "Tumor o cáncer en vías respiratorias: tráquea, faringe o garganta o laringe",
                                 "Tumor o cáncer en riñón", "Tumor o cáncer en vías urinarias: vejiga, uréter, uretra",
                                 "Tumor o cáncer en hígado", "Tumor o cáncer en páncreas",
                                 "Tumor o cáncer en ojo", "Tumor o cáncer en boca", "Otro tipo de cáncer o enfermedad",
                                 "Aneurisma", "Artritis", "CUCI", "Tumor en el cerebro", "Embolia", "Enfermedad de Meniere",
                                 "Epilepsia", "Epoc", "Escoliosis", "Esquizofrenia", "Fibromialgia", "Fibrosis pulmonar",
                                 "Glomerulonefritis", "Hemofilia", "Hepatosplenomegalia", "Hidrocefalia", "Hígado graso",
                                 "Lupus", "Neuromielitis óptica / Enfemedad devic", "Parálisis cerebral",
                                 "Parálisis corporal", "Poliposis múltiple coronaria", "Psoriasis", "Síndrome de Turner",
                                 "Sordera", "Trombosis", "Retinosis pigmentaria", "Osteoporosis", "Colitis",
                                 "Sarcoma Sinovial", "Síndrome de Down", "Malformación degenerativa de los huesos",
                                 "Cáncer en extremidades", "Síndrome de Guillain-Barré", "Ceguera", "Anginas crónicas",
                                 "Cáncer de piel", "Problemas del habla", "Tumor de células gigantes", "Osteomielitis",
                                 "Leucoma corneal", "Tumor en las glándulas del crecimiento", "Ciática", "Bradicardia",
                                 "Displasia broncopulmonar", "Cáncer (no recuerda)"))
    ),
    # muestra el panel principal con resultados
    mainPanel(
      plotlyOutput("Plot"),
      dataTableOutput("Tabla")
    )
  )
)

# Define el server para calcular tablas y gráficos
server <- function(input, output){
  #Datos ----
  # lectura de datos de municipios
  Mpios <- read_csv("./Repositorio/Mun_Microcuencas.csv", 
                    col_names = TRUE, col_types = "cccc") %>%
    mutate(CVE_MUN = str_pad(CVE_MUN, width = 3, 
                             side = "left", pad = "0")) %>%
    mutate(CVE = paste(CVE_ENT, CVE_MUN)) %>%
    arrange(CVE_ENT, CVE_MUN)
  
  # lectura de datos de población para 2020
  Pob2020 <- read_csv("./Repositorio/Pob2020.csv", 
                      col_names = TRUE, 
                      col_types = "ccddddddddddddddddddddd") %>%
    mutate(CVE_MUN = str_pad(CVE_MUN, width = 3, side = "left", pad = "0")) %>%
    right_join(Mpios, by = c("CVE_ENT", "CVE_MUN")) %>%
    select(1:23) %>%
    pivot_longer(cols = starts_with("Pob_"), values_to = "Poblacion",
                 names_to = "Edad", names_prefix = "Pob_") %>%
    mutate(Sexo = str_sub(Edad, -1),
           Edad = str_extract(Edad, "[:alpha:]*(?=_)")) %>%
    filter(Sexo == "T") %>%
    select(!Sexo) %>%
    mutate(Edad = factor(Edad,
                         levels = c("PInf", "Inf", "Pub", "Ado",
                                    "Juv", "Adu", "Vej"),
                         labels = c("0-9", "0-9", "10-19", "10-19",
                                    "20-24", "25-59", "60+"),
                         ordered = is.ordered(.$Edad))) %>%
    group_by(CVE_ENT, CVE_MUN, Edad) %>%
    summarise(Poblacion = sum(Poblacion))
  
  # lectura de datos de casos reportados en el censo
  Casos <- read_csv("./Repositorio/Prevalencia_Repositorio.csv", col_names = TRUE, col_types = "dccfffcddd") %>%
    select(1:8) %>%
    mutate(Edad = factor(Edad,
                         levels = c("1a infancia", "Infancia", "Pubertad", "Adolescencia",
                                    "Juventud", "Adultez", "Vejez"),
                         labels = c("0-9", "0-9", "10-19", "10-19",
                                    "20-24", "25-59", "60+"),
                         ordered = is.ordered(.$Edad))) %>%
    group_by(Anio, CVE_ENT, CVE_MUN, Edad, ECNT) %>%
    summarise(Casos = sum(Casos)) 
  
  #ListaECNT <- read_csv("./Repositorio/Lista_enfermedades_parametria.csv")
  
  #Funciones
  # Se asigna enfermedades seleccionadas en ListaECNT
  # Se asigna número de casos, población y prevalencia a variable temporal
  Valores <- function(){
    ListaECNT <- as_tibble(ListaECNT) %>%
      `colnames<-`("ECNT")
    tempPrev <<- Casos %>%
      #filter(Edad == iEdad) %>%
      right_join(ListaECNT, by = "ECNT") %>% # se usa el join como filtro para la enfermedad deseada
      group_by(CVE_ENT, CVE_MUN, Edad) %>%
      summarise(Casos = sum(Casos)) %>% # se calcula número de casos
      right_join(Pob2020) %>% # se añade total de población por tratamiento
      replace_na(list(Casos = 0)) %>% # se rellena con cero los valores faltantes
      mutate(Prevalencia = Casos / Poblacion *1000) %>% # se calcula prevalencia
      arrange(CVE_ENT, CVE_MUN, Edad)
  }
  

  
  # Inputs y outputs ----
  # se lee la lista de enfermedades a revisar y se mete a la función principal
  Button1 <- eventReactive(input$update, {
    ListaECNT <<- input$ECNT
    #iEdad <<- input$EdadObj
    Valores()
  })
  
  # Tabla con valores por grupo de edad ----
  # Se presentan los valores de prevalencia 
  Tablareact <- reactive({
    Button1()
    tempPrev %>%
      left_join(Mpios) %>%
      select(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, Prevalencia) %>%
      pivot_wider(names_from = Edad, values_from = Prevalencia)
  })
  
  # GráfiButton1ca ----
  # se presenta la gráfica de prevalencia por grupos de edad y municipio
  Plotreact <- reactive({
    Button1() %>%
      mutate(CVE = paste(CVE_ENT, CVE_MUN)) %>%
      left_join(Mpios) %>%
      mutate(Municipio = NOM_MUN) %>%
      ggplot(aes(x = Municipio, y = Prevalencia, color = Edad))+
      geom_point()+
      theme_bw()+
      theme(axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_blank()
            )+
      xlab("Municipio")+
      ylab("Casos por cada 1,000 habitantes")
  })
  
  output$Plot <- renderPlotly(
    Plotreact()
  )
  
  output$Tabla <- renderDataTable(
    Tablareact())
  
  output$descargar<-downloadHandler(
    filename = function(){
      paste0("Descarga-", Sys.Date(),".csv")
    },
    content = function(file){
      write.csv(Tablareact(), file)
    }
  )
}

# Corre la aplicación
shinyApp(ui, server)
