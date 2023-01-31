library(shiny)
library(tidyverse)
library(broom)
library(Kendall)


ui <- fluidPage(
  
  titlePanel("Incidencia acumulada"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "Enfermedad", label = ("Enfermedad"),
                   choices = list(
                    "Tumor maligno del cuello del útero",
                    "Displasia cervical severa y cacu in situ",
                    "Tumor maligno de la mama",
                    "Bocio endémico",
                    "Diabetes mellitus tipo II",
                    "Diabetes mellitus tipo I",
                    "Obesidad",
                    "Hipertensión arterial",
                    "Enfermedad isquémica del corazón",
                    "Enfermedad cerebrovascular",
                    "Encefalocele",
                    "Espina bífida",
                    "Labio y paladar hendido",
                    "Microcefalia",
                    "Anencefalia", 
                    "Enfermedad de Parkinson",
                    "Enfermedad de Alzheimer",
                    "Asma",
                    "Intoxicación por plaguicida",
                    "Úlceras, gastritis y duodenitis"), selected = "Asma"), 
                 selectInput(
                   "Edad", label = ("Edad"),
                   choices = list(
                     "0-9", "10-19", "20-24", "25-59", "60+"
                     ), selected = "20-24"
                 ),
                 actionButton("update", "Actualizar", icon("sync")),
                 downloadButton("DescargarS", label = "Descargar serie de tiempo"),
                 downloadButton("DescargarMn", label = "Descargar promedio"),
                 downloadButton("DescargarMd", label = "Descargar medianas"),
                 downloadButton("DescargarP", label = "Descargar pendientes"),
                 downloadButton("DescargarMK", label = "Descargar MannKendall")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Serie", dataTableOutput("Tabla1")),
                   tabPanel("Promedios", dataTableOutput("Tabla2")),
                   tabPanel("Medianas", dataTableOutput("Tabla3")),
                   tabPanel("Pendientes", dataTableOutput("Tabla4")),
                   tabPanel("Mannkendall", dataTableOutput("Tabla5"))
                 )
               )
               
             )
             
  
)

server <- function(input, output) {
  #Datos----
  
  PobAnios <- read_csv("./Repositorio/Pobl0019_Repositorio.csv", col_names = T, col_types = "dffdddddddddddddddddddddd")%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, Pob_PInf_T, Pob_Inf_T, Pob_Pub_T, Pob_Ado_T,
           Pob_Juv_T, Pob_Adu_T, Pob_Vej_T)%>%
    pivot_longer(cols = starts_with("Pob"),
                 names_prefix = "Pob_", 
                 names_to = "Edad",
                 values_to = "Poblacion")%>%
    mutate(Edad= factor(Edad, 
                        levels = c("PInf_T", "Inf_T", "Pub_T", "Ado_T", 
                                   "Juv_T", "Adu_T", "Vej_T"),
                        labels = c("0-9", "0-9", "10-19", "10-19",
                                   "20-24", "25-59", "60+"),
                        ordered = is.ordered(.$Edad)))%>%
    group_by(Anio, CVE, Edad)%>%
    summarise(Poblacion=sum(Poblacion))%>%
    ungroup()
  
  Mpios <- read_csv("./Repositorio/Mun_Microcuencas.csv", col_names = T, col_types = "cccc")%>%
    mutate(CVE_MUN= str_pad(CVE_MUN, width = 3, side = "left", pad = "0"),
           CVE=paste(CVE_ENT, CVE_MUN))
  
  Casos <- read_csv("./Repositorio/SUIVE0019.csv", col_names = T, col_types = "dccccd")%>%
    mutate(Edad= factor(Edad, 
                        levels = c("PInf", "Inf", "Pub", "Ado", 
                                   "Juv", "Adu", "Vej"),
                        labels = c("0-9", "0-9", "10-19", "10-19",
                                   "20-24", "25-59", "60+"),
                        ordered = is.ordered(.$Edad)))%>%
    left_join(read_csv("./Repositorio/GruposECNT.csv", col_names= T, col_types="cccc"), 
              by=c("ECNT"="Clave_EPI"))%>%
    filter(Sexo=="T")%>%
    select(Anio, CVE, Edad, Enfermedades, Casos)%>%
    group_by(Anio, CVE, Edad, Enfermedades)%>%
    summarise(Casos=sum(Casos))%>%
    ungroup()%>%
    left_join(Mpios, by="CVE")
  
  
  #Funciones----
  
  Valores20 <- function(iCausa, iEdad){
    tempSerie <<-Casos%>%
      filter(Enfermedades==iCausa)%>%
      filter(Edad==iEdad)%>%
      left_join(PobAnios)%>%
      mutate(IA= Casos/Poblacion*1000)%>%
      select(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, IA)
  }
  
  Valores0009 <-function(iCausa, iEdad){
    tempSerie <<-Casos%>%
      filter(Anio<=2009)%>%
      filter(Enfermedades==iCausa)%>%
      filter(Edad==iEdad)%>%
      left_join(PobAnios)%>%
      mutate(IA= Casos/Poblacion*1000)%>%
      select(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, IA)
  }
  
  Valores1019 <-function(iCausa, iEdad){
    tempSerie <<-Casos%>%
      filter(Anio>=2010)%>%
      filter(Enfermedades==iCausa)%>%
      filter(Edad==iEdad)%>%
      left_join(PobAnios)%>%
      mutate(IA= Casos/Poblacion*1000)%>%
      select(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, IA)
  }
  
  #Entradas y salidas----
  
  Boton1 <- eventReactive(input$update, {
    Serie1<<- Valores20(input$Enfermedad, input$Edad)
    Serie2<<- Valores0009(input$Enfermedad, input$Edad)
    Serie3<<- Valores1019(input$Enfermedad, input$Edad)
    
  })
  
  #Serie de tiempo----
  
  TablaS <- reactive({
    Boton1()
    Serie1%>%
      pivot_wider(names_from = Anio, values_from = IA)
    
  })
  
  output$Tabla1 <-renderDataTable({
    TablaS()
  })
  
  output$DescargarS<-downloadHandler(
    filename = function(){
      paste0("Serie_",input$Edad, "_", input$Enfermedad,".csv")
    }, 
    content = function(file){
      write.csv(TablaS(), file)
    }
  )
  
 #Promedios----
TablaMn <- reactive({
  Boton1()
  temp1<-Serie1%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Avg0019=mean(IA))
  temp2<-Serie2%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Avg0009=mean(IA))
  temp3<- Serie3%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Avg1019=mean(IA))
  
  temp2%>%
    left_join(temp3)%>%
    left_join(temp1)%>%
    arrange(CVE)%>%
    left_join(Mpios)%>%
    transmute(CVE_ENT=str_sub(CVE,1,2), 
              CVE_MUN=str_sub(CVE,-3),
              NOM_MUN= NOM_MUN,
              Microcuenca,
              Avg0009, Avg1019, Avg0019)
    
    
  
})

output$Tabla2<- renderDataTable({
  TablaMn()
})

output$DescargarMn<-downloadHandler(
  filename = function(){
    paste0("Promedio",input$Edad, "_", input$Enfermedad,".csv")
  }, 
  content = function(file){
    write.csv(TablaMn(), file)
  }
)

#Median----
TablaMd <- reactive({
  Boton1()
  temp1<-Serie1%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Md0019=median(IA))
  temp2<-Serie2%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Md0009=median(IA))
  temp3<- Serie3%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    group_by(CVE)%>%
    summarise(Md1019=median(IA))
  
  temp2%>%
    left_join(temp3)%>%
    left_join(temp1)%>%
    arrange(CVE)%>%
    left_join(Mpios)%>%
    transmute(CVE_ENT=str_sub(CVE,1,2), 
              CVE_MUN=str_sub(CVE,-3),
              NOM_MUN= NOM_MUN,
              Microcuenca,
              Md0009, Md1019, Md0019)
  
  
  
})

output$Tabla3<- renderDataTable({
  TablaMd()
})

output$DescargarMd<-downloadHandler(
  filename = function(){
    paste0("Mediana",input$Edad, "_", input$Enfermedad,".csv")
  }, 
  content = function(file){
    write.csv(TablaMd(), file)
  }
  
)

#Pendientes----
TablaPd<-reactive({
  Boton1()
  tempSerie2<-Serie1%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempLineal<- tibble(CVE=character(), Pd0019=numeric())
  for (i in tempNames) {
    tempL1<- tibble(CVE=character(), Pd0019=numeric())
    tempL<- tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)
    
    tempL1[1,2]<- lm(data = tempL, IA ~ Anio)$coeff[2]
    tempL1[1,1]<- i
    tempLineal<- bind_rows(tempLineal, tempL1)
    
  }
  
  tempLineal20<- tempLineal

  
  
  
  tempSerie2<-Serie2%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempLineal<- tibble(CVE=character(), Pd0009=numeric())
  for (i in tempNames) {
    tempL1<- tibble(CVE=character(), Pd0009=numeric())
    tempL<- tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)
    
    tempL1[1,2]<- lm(data = tempL, IA ~ Anio)$coeff[2]
    tempL1[1,1]<- i
    tempLineal<- bind_rows(tempLineal, tempL1)
    
  }
  
  tempLineal0009<- tempLineal  
  
  
  
  
  tempSerie2<-Serie3%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempLineal<- tibble(CVE=character(), Pd1019=numeric())
  for (i in tempNames) {
    tempL1<- tibble(CVE=character(), Pd1019=numeric())
    tempL<- tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)
    
    tempL1[1,2]<- lm(data = tempL, IA ~ Anio)$coeff[2]
    tempL1[1,1]<- i
    tempLineal<- bind_rows(tempLineal, tempL1)
    
  }
  
  tempLineal1019<- tempLineal
 
  
  
tempLineal0009%>%
  left_join(tempLineal1019)%>%
  left_join(tempLineal20)%>%
  left_join(Mpios)%>%
  transmute(CVE_ENT=str_sub(CVE,1,2), 
            CVE_MUN=str_sub(CVE,-3),
            NOM_MUN= NOM_MUN,
            Microcuenca,
            Pd0009, Pd1019, Pd0019)
})

output$Tabla4 <- renderDataTable({
  TablaPd()
})

output$DescargarP<-downloadHandler(
  filename = function(){
    paste0("Pendiente",input$Edad, "_", input$Enfermedad,".csv")
  }, 
  content = function(file){
    write.csv(TablaPd(), file)
  }
  
)

#Mannkendall----

TablaMk<-reactive({
  Boton1 ()
  tempSerie2<-Serie1%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempkendall<-tibble(p.value=numeric(), CVE=character())
  
  for (i in tempNames) {
    tempk<-tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)%>%
      pull(IA)%>%
      MannKendall()%>%
      tidy()%>%
      select(p.value)
    tempk$CVE=i
    tempkendall<-bind_rows(tempkendall, tempk)
    
  }
  
  tempkendall20<- tempkendall
  
 
  
  
  
  tempSerie2<-Serie2%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempkendall<-tibble(p.value=numeric(), CVE=character())
  
  for (i in tempNames) {
    tempk<-tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)%>%
      pull(IA)%>%
      MannKendall()%>%
      tidy()%>%
      select(p.value)
    tempk$CVE=i
    tempkendall<-bind_rows(tempkendall, tempk)
    
  }
  
  tempkendall0009<- tempkendall 
  
  
  
  
  tempSerie2<-Serie3%>%
    mutate(CVE=paste(CVE_ENT, CVE_MUN))%>%
    select(Anio, CVE, IA)
  
  tempNames<- levels(as.factor(tempSerie2$CVE))
  
  tempkendall<-tibble(p.value=numeric(), CVE=character())
  
  for (i in tempNames) {
    tempk<-tempSerie2%>%
      filter(CVE==i)%>%
      arrange(Anio)%>%
      pull(IA)%>%
      MannKendall()%>%
      tidy()%>%
      select(p.value)
    tempk$CVE=i
    tempkendall<-bind_rows(tempkendall, tempk)
    
  }
  
  tempkendall1019<- tempkendall
  
  tempMk<<- tempkendall0009%>%
    left_join(tempkendall1019, by="CVE")%>%
    left_join(tempkendall20, by="CVE")%>%
    left_join(Mpios)%>%
    transmute(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca,
              p.value0009=p.value.x,p.value1019=p.value.y, p.value0019=p.value)%>%
    arrange(CVE_ENT, CVE_MUN)
})

output$Tabla5<- renderDataTable({
  TablaMk()
})

output$DescargarMK<-downloadHandler(
  filename = function(){
    paste0("Mannkendall",input$Edad, "_", input$Enfermedad,".csv")
  }, 
  content = function(file){
    write.csv(TablaMk(), file)
  }
  
)
}
shinyApp(ui, server)