#########
# Combinando funciones
#########

library(shiny)
library(tidyverse)
library(broom)
library(Kendall)

# Define la UI para la aplicación
ui <- fluidPage(
  # título de la aplicación
  titlePanel("Tasas de mortalidad y sus métricas"),
  
  # Sidebar en dónde se ponen los inputs ----
  sidebarLayout(
    sidebarPanel(
      textInput("Causa1", label=("Grupo de enfermedades"), value="N"),
      numericInput("Causa2Min", label=("MindelRango"), value=18, min= 0, max=99,step=1),
      numericInput("Causa2Max", label=("MaxdelRango"), value=18, min= 0, max=99,step=1),
      selectInput("EdadObj", label=("Edad"), 
                  choices=list("0-9", "10-19", "20-24",
                               "25-59", "60+"), selected="20-24"),
      actionButton("update", "Actualizar", icon("sync")),
      downloadButton("descargaSr", "Descargar serie de tiempo"),
      downloadButton("descargaMn", "Descargar promedios"),
      downloadButton("descargaMd", "Descargar medianas"),
      downloadButton("descargaPd", "Descargar pendientes"),
      downloadButton("descargaMK", "Descargar Mann-Kendall"),
      downloadButton("descargaTBM", "Descargar TBM total")
    ),
    
    # Muestra la tabla generada ----
    mainPanel(
      tabsetPanel(
        tabPanel("Serie", dataTableOutput("Tabla0")),
        tabPanel("Promedios", dataTableOutput("Tabla")),
        tabPanel("Medianas", dataTableOutput("Tabla2")),
        tabPanel("Pendientes", dataTableOutput("Tabla3")),
        tabPanel("Mann-Kendall", dataTableOutput("Tabla4")),
        tabPanel("TBM total", dataTableOutput("Tabla5"))
      )
    )
  )
)

# Definir la función server
server <- function(input, output) {
  
  # Datos ----
  # lectura de datos de población en 20 años por grupos de edad y municipio
  PobAños <- read_csv("./Repositorio/Pobl0019_Repositorio.csv", 
                      col_names = TRUE, col_types = "dffdddddddddddddddddddddd") %>%
    mutate(CVE = paste(CVE_ENT, CVE_MUN)) %>%
    select(Anio, CVE, 
           Pob_PInf_T, Pob_Inf_T, Pob_Pub_T, Pob_Ado_T,
           Pob_Juv_T, Pob_Adu_T, Pob_Vej_T) %>%
    pivot_longer(cols = starts_with("Pob"),
                 names_prefix = "Pob_",
                 names_to = "Edad",
                 values_to = "Poblacion") %>%
    mutate(Edad = factor(Edad, 
                         levels = c("PInf_T", "Inf_T", "Pub_T", "Ado_T", 
                                    "Juv_T", "Adu_T", "Vej_T"),
                         labels = c("0-9", "0-9", "10-19", "10-19",
                                    "20-24", "25-59", "60+"),
                         ordered = is.ordered(.$Edad)
    )) %>%
    group_by(Anio, CVE, Edad) %>%
    summarise(Poblacion = sum(Poblacion))
  
  # lectura de la lista de municipios, claves y microcuencas
  Mpios<-read_csv("./Repositorio/Mun_Microcuencas.csv", col_names = TRUE, col_types = "cccc")%>%
    mutate(CVE_MUN=str_pad(CVE_MUN, width = 3, side= "left", pad = "0"),
           CVE=paste(CVE_ENT, CVE_MUN))
  
  #lectura de la data de defunciones por causa, edad y municipio
  Defun<- read_csv("./Repositorio/DefunCausa_Repositorio_3.csv", col_names = TRUE, col_types= "dccfffcd")%>%
    mutate(CVE_MUN=str_pad(CVE_MUN, width = 3, side= "left", pad = "0"),
           Edad = factor(Edad, 
                         levels = c("1a infancia", "Infancia", "Pubertad", "Adolescencia",
                                    "Juventud", "Adultez", "Vejez"),
                         labels = c("0-9", "0-9", "10-19", "10-19",
                                    "20-24", "25-59", "60+"),
                         ordered = is.ordered(.$Edad)))%>%
    group_by(Anio, CVE_ENT, CVE_MUN, Edad, CIE10) %>%
    summarise(Defunciones = sum(Defunciones)) %>%
    left_join(Mpios, by=c("CVE_ENT", "CVE_MUN"))%>%
    mutate(Causa1=str_sub(CIE10,1,1), 
           Causa2=as.numeric(str_sub(CIE10,2,3)))
  
  # Funciones ----
  # Esta función calcula las tasas de mortalidad para una o un grupo
  # de enfermedades al tomar el número de defunciones y dividirlo por la
  # población en ese municipio y año y multiplicarlo por 1000
  Valores20<-function(iCausa1, iCausa2Min, iCausa2Max, iEdad){
    TempSerie<<-Defun%>%
      filter(Causa1==iCausa1)%>% # filtro de letra
      filter(Causa2>=iCausa2Min&Causa2<=iCausa2Max)%>% #filtro de números
      filter(Edad==iEdad)%>% # filtro de edad
      group_by(Anio, CVE, NOM_MUN, Microcuenca)%>%
      summarise(Defunciones=sum(Defunciones))%>% # cálculo de número de defunciones
      right_join(PobAños%>%
                   filter(Edad==iEdad), by=c("Anio", "CVE"))%>% # se añade población
      replace_na(list(Defunciones=0))%>% # rellena con 0 los municipios sin defunciones
      ungroup()%>%
      mutate(TBM=Defunciones/Poblacion*1000) # calcula tasa de mortalidad
  }
  
  # función que trabaja igual a la anterior pero se enfoca en datos de años
  # del 2000 a 2009
  Valores1<-function(iCausa1, iCausa2Min, iCausa2Max, iEdad){
    TempSerie<<-Defun%>%
      filter(Anio<2010)%>%
      filter(Causa1==iCausa1)%>%
      filter(Causa2>=iCausa2Min&Causa2<=iCausa2Max)%>%
      filter(Edad==iEdad)%>%
      group_by(Anio, CVE, NOM_MUN, Microcuenca)%>%
      summarise(Defunciones=sum(Defunciones))%>%
      right_join(PobAños%>%
                   filter(Anio<2010)%>%
                   filter(Edad==iEdad), by=c("Anio", "CVE"))%>%
      replace_na(list(Defunciones=0))%>%
      ungroup()%>%
      mutate(TBM=Defunciones/Poblacion*1000)
  }
  
  # función que trabaja igual a la anterior pero se enfoca en datos de años
  # de 2010 a 2019
  Valores2<-function(iCausa1, iCausa2Min, iCausa2Max, iEdad){
    TempSerie<<-Defun%>%
      filter(Anio>=2010)%>%
      filter(Causa1==iCausa1)%>%
      filter(Causa2>=iCausa2Min&Causa2<=iCausa2Max)%>%
      filter(Edad==iEdad)%>%
      group_by(Anio, CVE, NOM_MUN, Microcuenca)%>%
      summarise(Defunciones=sum(Defunciones))%>%
      right_join(PobAños%>%
                   filter(Anio>=2010)%>%
                   filter(Edad==iEdad), by=c("Anio", "CVE"))%>%
      replace_na(list(Defunciones=0))%>%
      ungroup()%>%
      mutate(TBM=Defunciones/Poblacion*1000)
  }
  
  # Entradas y salidas ----
  # al presionar el botón de "actualizar" se calculan las 3 series de tiempo
  # (i.e. 2000-2019, 2000-2009, 2010, 2019)
  # se hace asignación global <<- para poder usar la misma variable en diferentes
  # lugares de la aplicación
  Button1 <- eventReactive(input$update, {
    Serie1 <<- Valores20(input$Causa1, input$Causa2Min, input$Causa2Max, input$EdadObj)
    Serie2 <<- Valores1(input$Causa1, input$Causa2Min, input$Causa2Max, input$EdadObj)
    Serie3 <<- Valores2(input$Causa1, input$Causa2Min, input$Causa2Max, input$EdadObj)
  })
  
  # Serie de tiempo ----
  # se presenta la tabla completa de serie de tiempo de tasa de mortalidad 
  # años 2000 a 2019
  TablaSr <- reactive({
    Button1()
    Serie1 %>%
      select(Anio, CVE, TBM) %>%
      pivot_wider(names_from = Anio, values_from = TBM) %>%
      left_join(Mpios) %>%
      select(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, starts_with("20")) %>%
      arrange(CVE_ENT, CVE_MUN)
  })
  
  output$Tabla0 <- renderDataTable({
    TablaSr()
  })
  
  # se configura el botón de descarga de serie de tiempo
  output$descargaSr <- downloadHandler(
    filename = function(){
      paste0("Serie_", input$EdadObj,"_",input$Causa1,input$Causa2Min,"-",input$Causa2Max, ".csv")
    },
    content = function(file){
      write.csv(TablaSr(), file)
    }
  )
  
  # Promedios ----
  # se calculan promedios por década y promedio del periodo completo
  TablaMn <- reactive({
    Button1()
    TempMn <- Serie2 %>%
      group_by(CVE)%>%
      summarise(Avg0009 = mean(TBM))%>% # promedio de serie completa
      left_join(Mpios, by="CVE")%>%
      select(CVE, NOM_MUN, Avg0009, Microcuenca) %>%
      left_join(Serie3 %>%
                  group_by(CVE) %>%
                  summarise(Avg1019 = mean(TBM)), # promedio de segunda década (2010-2019)
                by = "CVE") %>%
      left_join(Serie1 %>%
                  group_by(CVE) %>%
                  summarise(Avg0019 = mean(TBM)), # promedio de primera década (2000-2009)
                by = "CVE") %>%
      arrange(CVE) %>%
      transmute(CVE_ENT = str_sub(CVE,1,2),
                CVE_MUN = str_sub(CVE, -3),
                NOM_MUN,
                Microcuenca,
                Avg0009, Avg1019, Avg0019) # se acomoda y presenta la información
    TempMn
  })
  
  output$Tabla <- renderDataTable({
    TablaMn()
  })
  
  # configuración del botón de descarga
  output$descargaMn <- downloadHandler(
    filename = function(){
      paste0("Promedio_", input$EdadObj,"_",input$Causa1,input$Causa2Min,"-",input$Causa2Max, ".csv")
    },
    content = function(file){
      write.csv(TablaMn(), file)
    }
  )
  
  # Medianas ----
  # se calculan medianas por década y promedio del periodo completo
  TablaMd <- reactive({
    Button1()
    TempMd <- Serie2 %>%
      group_by(CVE)%>%
      summarise(Med0009 = median(TBM)) %>%
      left_join(Mpios, by= "CVE")%>%
      select(CVE, NOM_MUN, Med0009, Microcuenca) %>% # mediana de serie completa
      left_join(Serie3 %>%
                  group_by(CVE) %>%
                  summarise(Med1019 = median(TBM)), # mediana de segunda década (2010-2019)
                by = "CVE") %>%
      left_join(Serie1 %>%
                  group_by(CVE) %>%
                  summarise(Med0019 = median(TBM)), # mediana de primera década (2000-2009)
                by = "CVE") %>%
      arrange(CVE) %>%
      transmute(CVE_ENT = str_sub(CVE,1,2),
                CVE_MUN = str_sub(CVE, -3),
                NOM_MUN,
                Microcuenca,
                Med0009, Med1019, Med0019) # se acomoda y presenta la información
    TempMd
  })
  
  output$Tabla2 <- renderDataTable({
    TablaMd()
  })
  
  # configuración del botón de descarga
  output$descargaMd <- downloadHandler(
    filename = function(){
      paste0("Mediana_", input$EdadObj,"_",input$Causa1,input$Causa2Min,"-",input$Causa2Max, ".csv")
    },
    content = function(file){
      write.csv(TablaMd(), file)
    }
  )
  
  # Pendientes ----
  # se calcula la pendiente linealizada para las series de tiempo completa y por décadas
  # Primero se crea una variable temporal vacía que contendrá clave de municipio y pendiente
  # Dicha variable se irá llenando con el coeficiente de pendiente calculado por 
  # lm() y su respectiva clave de municipio
  # Se repite este cálculo para los 67 municipios y para los 3 periodos que se están
  # analizando (2000-2019, 2000-2009, 2010-2019)
  TablaPd <- reactive({
    Button1()
    TempSerie2 <- Serie1 %>%
      select(Anio, CVE, TBM)
    
    tempNames <- levels(as.factor(TempSerie2$CVE))
      
    TempLineal<-tibble(CVE=character(), Pend0019=numeric())
    for(i in tempNames){
      tempL1 <- tibble(CVE=character(), Pend0019=numeric())
      TempL <- TempSerie2%>%
        filter(CVE == i)%>%
        arrange(Anio)

      tempL1[1,2] <- lm(data = TempL, TBM ~ Anio)$coeff[2]
      tempL1$CVE <- i
      TempLineal <- bind_rows(TempLineal,tempL1)
    }
    TempLineal20 <- TempLineal

    TempSerie2 <- Serie2 %>%
      select(Anio, CVE, TBM)
    
    tempNames <- levels(as.factor(TempSerie2$CVE))

    TempLineal<-tibble(CVE=character(), Pend0009=numeric())
    for(i in tempNames){
      tempL1 <- tibble(CVE=character(), Pend0009=numeric())
      TempL <- TempSerie2 %>%
        filter(CVE == i) %>%
        arrange(Anio)

      tempL1[1,2] <- lm(data = TempL, TBM ~ as.numeric(Anio))$coeff[2]
      tempL1$CVE <- i
      TempLineal <- bind_rows(TempLineal,tempL1)
    }
    TempLineal01 <- TempLineal

    TempSerie2 <- Serie3 %>%
      select(Anio, CVE, TBM)
    
    tempNames <- levels(as.factor(TempSerie2$CVE))

    TempLineal<-tibble(CVE=character(), Pend1019=numeric())
    for(i in tempNames){
      tempL1 <- tibble(CVE=character(), Pend1019=numeric())
      TempL <- TempSerie2%>%
        filter(CVE == i)%>%
        arrange(Anio)

      tempL1[1,2] <- lm(data = TempL, TBM ~ as.numeric(Anio))$coeff[2]
      tempL1$CVE <- i
      TempLineal <- bind_rows(TempLineal,tempL1)
    }
    TempLineal02 <- TempLineal

    tempPd <<- TempLineal01 %>%
      left_join(TempLineal02, by = "CVE")%>%
      left_join(TempLineal20, by = "CVE")%>%
      left_join(Mpios, by = "CVE")%>%
      transmute(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca,
                Pend0009, Pend1019, Pend0019) %>%
      arrange(CVE_ENT, CVE_MUN)

    tempPd
  })

  output$Tabla3 <- renderDataTable({
    TablaPd()
  })

  output$descargaPd <- downloadHandler(
    filename = function(){
      paste0("Pendientes_", input$EdadObj,"_",input$Causa1,input$Causa2Min,"-",input$Causa2Max, ".csv")
    },
    content = function(file){
      write.csv(TablaPd(), file)
    }
  )
  
  # Mann-Kendall ----
  # se calcula el p-value para la prueba Mann-Kendall aplicada a las series de tiempo
  # completa y por décadas
  # Primero se crea una variable temporal vacía que contendrá clave de municipio y pendiente
  # Dicha variable se irá llenando con el p-value calculado por 
  # Kendall::MannKendall() y su respectiva clave de municipio
  # Se repite este cálculo para los 67 municipios y para los 3 periodos que se están
  # analizando (2000-2019, 2000-2009, 2010-2019)
  TablaMK <- reactive({
    Button1()
    TempSerie2 <- Serie1 %>%
      select(Anio, CVE, TBM)
    
    tempNames <- levels(as.factor(TempSerie2$CVE))
    
    tempKendall <- tibble(p.value = numeric(), CVE = character())
    for(i in tempNames){
      tempK <- TempSerie2 %>%
        filter(CVE == i) %>%
        arrange(Anio) %>%
        pull(TBM) %>%
        MannKendall() %>%
        tidy() %>%
        select(p.value)
      tempK$CVE = i
      tempKendall <- bind_rows(tempKendall, tempK)
    }
    tempKendall20 <- tempKendall
    
    TempSerie2 <- Serie2 %>%
      select(Anio, CVE, TBM)
    
    tempNames <- levels(as.factor(TempSerie2$CVE))
    
    tempKendall <- tibble(p.value = numeric(), CVE = character())
    for(i in tempNames){
      tempK <- TempSerie2 %>%
        filter(CVE == i) %>%
        arrange(Anio) %>%
        pull(TBM) %>%
        MannKendall() %>%
        tidy() %>%
        select(p.value)
      tempK$CVE = i
      tempKendall <- bind_rows(tempKendall, tempK)
    }
    tempKendall0009 <- tempKendall
    
    TempSerie2 <- Serie3 %>%
      select(Anio, CVE, TBM)
    tempNames <- levels(as.factor(TempSerie2$CVE))
    
    tempKendall <- tibble(p.value = numeric(), CVE = character())
    for(i in tempNames){
      tempK <- TempSerie2 %>%
        filter(CVE == i) %>%
        arrange(Anio) %>%
        pull(TBM) %>%
        MannKendall() %>%
        tidy() %>%
        select(p.value)
      tempK$CVE = i
      tempKendall <- bind_rows(tempKendall, tempK)
    }
    tempKendall1019<- tempKendall
    
    tempMK <<- tempKendall0009%>%
      left_join(tempKendall1019, by="CVE")%>%
      left_join(tempKendall20, by="CVE")%>%
      left_join(Mpios, by = "CVE")%>%
      transmute(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, 
                p.value0009 = p.value.x,
                p.value1019 = p.value.y,
                p.value0019 = p.value) %>%
      arrange(CVE_ENT, CVE_MUN)
    
    tempMK
  })
  
  output$Tabla4 <- renderDataTable({
    TablaMK()
  })
  
  output$descargaMK <- downloadHandler(
    filename = function(){
      paste0("MannKendall_", input$EdadObj,"_",input$Causa1,input$Causa2Min,"-",input$Causa2Max, ".csv")
    },
    content = function(file){
      write.csv(TablaMK(), file)
    }
  )
  
  # TBM total ----
  # se presenta tasa de mortalidad total incluyendo todas las causas de defunción 
  # consideradas y los tamaños de población en cada año. Este valor después se resume
  # con el uso de medianas debido a que se evita la influencia excesiva de los ceros en
  # la matriz. 
  # Se presenta mediana para cada grupo de edad en 3 periodos (i.e. 2000-2019, 2000-2009, 2010-2019)
  TablaTBM <- reactive({t1 <- Defun %>%
    group_by(Anio, CVE, Edad) %>%
    summarise(Defunciones = sum(Defunciones)) %>% 
    right_join(PobAños) %>%
    replace_na(list(Defunciones = 0)) %>%
    transmute(Anio,
              CVE,
              Edad,
              TBM = Defunciones / Poblacion * 1000) %>%
    ungroup() %>%
    group_by(CVE, Edad) %>%
    summarise(TBM = median(TBM)) %>%
    pivot_wider(names_from = Edad, values_from = TBM) %>%
    `colnames<-`(c("CVE", "0-9_0019", "10-19_0019", 
                   "20-24_0019", "25-59_0019", "60+_0019"))
  
  t2 <- Defun %>%
    group_by(Anio, CVE, Edad) %>%
    summarise(Defunciones = sum(Defunciones)) %>% 
    right_join(PobAños) %>%
    replace_na(list(Defunciones = 0)) %>%
    transmute(Anio,
              CVE,
              Edad,
              TBM = Defunciones / Poblacion * 1000) %>%
    ungroup() %>%
    filter(Anio < 2010) %>%
    group_by(CVE, Edad) %>%
    summarise(TBM = median(TBM)) %>%
    pivot_wider(names_from = Edad, values_from = TBM) %>%
    `colnames<-`(c("CVE", "0-9_0009", "10-19_0009", 
                   "20-24_0009", "25-59_0009", "60+_0009"))
  
  t3 <- Defun %>%
    group_by(Anio, CVE, Edad) %>%
    summarise(Defunciones = sum(Defunciones)) %>% 
    right_join(PobAños) %>%
    replace_na(list(Defunciones = 0)) %>%
    transmute(Anio,
              CVE,
              Edad,
              TBM = Defunciones / Poblacion * 1000) %>%
    ungroup() %>%
    filter(Anio >= 2010) %>%
    group_by(CVE, Edad) %>%
    summarise(TBM = median(TBM)) %>%
    pivot_wider(names_from = Edad, values_from = TBM) %>%
    `colnames<-`(c("CVE", "0-9_1019", "10-19_1019", 
                   "20-24_1019", "25-59_1019", "60+_1019"))
  
  tempTot <- t1 %>%
    left_join(t2) %>%
    left_join(t3) %>%
    left_join(Mpios) %>%
    ungroup()%>%
    select(CVE_ENT:Microcuenca, `0-9_0019`:`60+_1019`)
  
  tempTot
  })
  
  output$Tabla5 <- renderDataTable({
    TablaTBM()
  })
  
  output$descargaTBM <- downloadHandler(
    filename = function(){
      paste0("TBMTotal_Medianas.csv")
    },
    content = function(file){
      write.csv(TablaTBM(), file)
    }
  )

}

# Corre la aplicación ----
shinyApp(ui = ui, server = server)