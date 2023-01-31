# Lectura de datos ----
PobrMun <- read_xls("./Concentrado_indicadores_de_pobreza_2020_Puebla-Tlaxcala.xls", sheet = "CAA") %>%
  pivot_longer(cols = 6:ncol(.), 
               names_to = "Parametro",
               values_to = "Valor",
               values_transform = as.numeric,
               values_drop_na = FALSE) %>%
  mutate(Anio = as.double(str_extract(string = Parametro, pattern = "20[:digit:]*")),
         Parametro = str_sub(Parametro, 1, -5)) %>%
  pivot_wider(names_from = "Parametro", values_from = "Valor") %>%
  mutate(CVE = paste(CVE_ENT, CVE_MUN, Anio),
         Anio = as.character(Anio)) %>%
  filter(CVE != "29 048 2020") %>%
  select(!CVE)
#write_csv(PobrMun, "./IndicPobr_2010_2020.csv")

tempMicro_1 <- PobrMun %>%
  select(Microcuenca, Anio, !ends_with("Pers")) %>%
  group_by(Microcuenca, Anio) %>%
  summarise(across(Pobr_Porc:PoblIngInfLineaPobrExtr_CareProm, ~mean(.x, na.rm = T)))
tempMicro_2 <- PobrMun %>%
  select(Microcuenca, Anio, ends_with("Pers")) %>%
  group_by(Microcuenca, Anio) %>%
  summarise(across(Pobr_Pers:PoblIngInfLineaPobrExtr_Pers, ~sum(.x, na.rm = T)))
PobrMicro <- tempMicro_1 %>%
  left_join(tempMicro_2) %>%
  mutate(Microcuenca = as.character(Microcuenca),
         Anio = as.character(Anio))

# PobrMicro %>%
#   pivot_wider(names_from = Anio, values_from = c(Pobr_Porc:PoblIngInfLineaPobrExtr_Pers),
#               names_sep = "") %>%
#   write_csv("./IndicPobr_Micro.csv")

Mpios <- read_csv("./Mun_Microcuencas.csv")

ListaIndicadores <- read_csv("./listaIndicadores.csv") %>%
  group_by(Indicador, Etiqueta) %>%
  summarise(Etiqueta = unique(Etiqueta)) %>%
  ungroup()

IndicOptions <- ListaIndicadores$Etiqueta

MpiosCAA <- rgdal::readOGR(dsn = "./MpiosCAA.gpkg", stringsAsFactors = F)
MicroCAA <- rgdal::readOGR(dsn = "./MicrocuencasCAA.gpkg", stringsAsFactors = F)

# Funciones ----
F_GrafSerie <- function(Seleccionado){
  Graf <- plot_ly(data = PobrMun,
                  x = ~Anio,
                  y = ~get(Seleccionado),
                  type = "box",
                  showlegend = F) %>%
    add_trace(type = "scatter",
              mode = "markers",
              text = ~paste(NOM_MUN, ": ", get(Seleccionado)),
              color = ~NOM_MUN,
              alpha = 0.5,
              showlegend = F) 
  return(Graf)
}

F_GrafSerie_2 <- function(Seleccionado){
  Graf <- plot_ly(data = PobrMicro,
                  x = ~Anio,
                  y = ~get(Seleccionado),
                  type = "box",
                  showlegend = F) %>%
    add_trace(type = "scatter",
              mode = "markers",
              text = ~paste(Microcuenca, ": ", get(Seleccionado)),
              color = ~Microcuenca,
              alpha = 0.5,
              showlegend = F)
  return(Graf)
}

F_GrafPastel <- function(Seleccionado){
  Graf <- plot_ly(data = PobrMun, 
                  x = ~Anio,
                  y = ~NOM_MUN, 
                  z = ~get(Seleccionado), 
                  type = "heatmap",
                  showlegend = F)
  return(Graf)
}

F_GrafPastel_2 <- function(Seleccionado){
  Graf <- plot_ly(data = PobrMicro, 
                  x = ~Anio,
                  y = ~Microcuenca, 
                  z = ~get(Seleccionado), 
                  type = "heatmap",
                  showlegend = F)
  return(Graf)
}

AnioSele = 2020
Seleccionado = "Pobr_Porc"

F_Mapa <- function(Seleccionado, AnioSele){
  Buscado <- paste0(Seleccionado, AnioSele)
  MpiosCAA[[Buscado]] <- as.numeric(MpiosCAA[[Buscado]])
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    MpiosCAA$NOM_MUN, MpiosCAA[[Buscado]]
  ) %>%
    lapply(htmltools::HTML)
  
  pal <- if(str_sub(Buscado, -8, -5) == "Pers") {
    pal <- colorBin("BuGn",
                    domain = c(0,100000),
                    bins = 5)
  } else if(str_sub(Buscado, -8, -5) == "Porc") {
    pal <- colorBin("BuGn",
                    domain = c(0,100),
                    bins = 5)
  } else if(str_sub(Buscado, -8, -5) == "Prom") {
    pal <- colorBin("BuGn",
                    domain = c(0,4),
                    bins = 4)
  }

  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 19.284239, lng = -98.214841, zoom = 9) %>%
    addPolygons(data = MpiosCAA,
                weight = 2,
                opacity = 1,
                fillOpacity = 0.8,
                color = "black",
                fillColor = ~pal(get(Buscado)),
                highlightOptions = highlightOptions(weight = 5,
                                                    color = "#666",
                                                    bringToFront = T
                                                    ),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")
                )
  m
}

F_Mapa_2 <- function(Seleccionado, AnioSele){
  Buscado <- paste0(Seleccionado, AnioSele)
  MicroCAA[[Buscado]] <- as.numeric(MicroCAA[[Buscado]])
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g",
    MicroCAA$Microcuenca, MicroCAA[[Buscado]]
  ) %>%
    lapply(htmltools::HTML)
  
  pal <- if(str_sub(Buscado, -8, -5) == "Pers") {
    pal <- colorBin("BuGn",
                    domain = c(0,500000),
                    bins = 5)
  } else if(str_sub(Buscado, -8, -5) == "Porc") {
    pal <- colorBin("BuGn",
                    domain = c(0,100),
                    bins = 5)
  } else if(str_sub(Buscado, -8, -5) == "Prom") {
    pal <- colorBin("BuGn",
                    domain = c(0,4),
                    bins = 4)
  }
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lat = 19.284239, lng = -98.214841, zoom = 9) %>%
    addPolygons(data = MicroCAA,
                weight = 2,
                opacity = 1,
                fillOpacity = 0.8,
                color = "black",
                fillColor = ~pal(get(Buscado)),
                highlightOptions = highlightOptions(weight = 5,
                                                    color = "#666",
                                                    bringToFront = T
                ),
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                         padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto")
    )
  m
}

F_Leyenda <- function(Seleccionado, Espacial){
  if(str_sub(Seleccionado, -4) == "Porc") {
    lgnd <- list(src = "./www/escalaPorc.png",
                 alt = "Escala 0 a 100%")
  } else if(str_sub(Seleccionado, -4) == "Prom"){
    lgnd <- list(src = "./www/escalaCare.png",
                 alt = "Escala 0 a 4")
  } else if(Espacial == "Municipios") {
    lgnd <- list(src = "./www/escalaPersMpio.png",
                 alt = "Escala 0 a 100,000")
  } else {
    lgnd <- list(src = "./www/escalaPersMicro.png",
                 alt = "Escala 0 a 500,000")
  }
  lgnd
}

# 
# tribble(~Pocentaje, ~cuenta,
#         "0-20", 10,
#         "21-40", 30,
#         "41-60", 50,
#         "61-80", 70,
#         "81-100", NA,
#         "NA", NA) %>%
#   ggplot(aes(y = cuenta, fill = Pocentaje))+
#   geom_bar()+
#   scale_fill_brewer(palette = "BuGn", direction = 1,
#                     na.value = "grey50")+
#   theme(legend.direction = "horizontal",
#         legend.key.size = unit(2, "cm"))
#   
#   colorBin("BuGn",
#            domain = c(0,100),
#            bins = 5)
