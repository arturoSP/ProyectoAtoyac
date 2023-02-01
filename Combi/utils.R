# test values ----
iCausa1 = "N"
iCausa2Min = 18
iCausa2Max = 18
iEdad = c("20-24")
#Serie <- ValJuntos("N", 18, 18, iEdad)
#Serie <- ValSeparados("N", 18, 18, iEdad)

# Función para calcular tasas de mortalidad en una enfermedad o rango de enfermedades ----

ValJuntos <- function(iCausa1, iCausa2Min, iCausa2Max, iEdad){
  valEdad <- iEdad %>%
    as_tibble() %>%
    arrange(value)
  
  Defun %>%
    filter(Causa1 == iCausa1) %>%
    filter(Causa2 >= iCausa2Min & Causa2 <= iCausa2Max) %>%
    right_join(valEdad, by = c("Edad" = "value")) %>%
    group_by(Anio, CVE_ENT, CVE_MUN) %>%
    summarise(Defunciones = sum(Defunciones)) %>%
    right_join(PobAños %>%
                 right_join(valEdad, 
                            by = c("Edad" = "value")) %>%
                 group_by(Anio, CVE_ENT, CVE_MUN) %>%
                 summarise(Poblacion = sum(Poblacion))) %>% 
    replace_na(list(Defunciones = 0)) %>%
    ungroup() %>%
    mutate(TBM = Defunciones / Poblacion * 1000) %>%
    left_join(Mpios) %>%
    select(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, TBM) %>%
    arrange(NOM_MUN, Anio) %>% 
    return()
}

ValSeparados <- function(iCausa1, iCausa2Min, iCausa2Max, iEdad){
  valEdad <- iEdad %>%
    as_tibble() %>%
    arrange(value)
  
  Defun %>%
    filter(Causa1 == iCausa1) %>%
    filter(Causa2 >= iCausa2Min & Causa2 <= iCausa2Max) %>%
    right_join(valEdad, by = c("Edad" = "value")) %>%
    group_by(Anio, CVE_ENT, CVE_MUN, Edad) %>%
    summarise(Defunciones = sum(Defunciones)) %>%
    right_join(PobAños %>%
                 right_join(valEdad, 
                            by = c("Edad" = "value"))) %>% 
    replace_na(list(Defunciones = 0)) %>%
    ungroup() %>%
    mutate(TBM = Defunciones / Poblacion * 1000) %>%
    left_join(Mpios) %>%
    select(Anio, CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, TBM) %>%
    arrange(NOM_MUN, Anio, Edad) %>% 
    return()
}

# función para calcular promedio de la serie de tiempo, total y por década ----
PromJuntos <- function(Serie){
  Serie %>%
    mutate(Periodo = ifelse(Anio < 2010, "2000-2009", "2010-2019")) %>% 
    group_by(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Periodo) %>%
    summarise(TBM = mean(TBM)) %>%
    pivot_wider(names_from = "Periodo", values_from = "TBM") %>%
    left_join(Serie %>%
                group_by(NOM_MUN) %>%
                summarise(`2000-2019` = mean(TBM))) %>%
    arrange(NOM_MUN) %>%
    ungroup() %>%
    return()
}

PromSeparados <- function(Serie){
  Serie %>%
    mutate(Periodo = ifelse(Anio < 2010, "2000-2009", "2010-2019")) %>% 
    group_by(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, Periodo) %>%
    summarise(TBM = mean(TBM)) %>%
    pivot_wider(names_from = "Periodo", values_from = "TBM") %>%
    left_join(Serie %>%
                group_by(NOM_MUN, Edad) %>%
                summarise(`2000-2019` = mean(TBM))) %>%
    arrange(NOM_MUN) %>%
    ungroup() %>%
    return()
}

# función para calcular medianas de la serie de tiempo, total y por década ----
MedianJuntos <- function(Serie){
  Serie %>%
    mutate(Periodo = ifelse(Anio < 2010, "2000-2009", "2010-2019")) %>% 
    group_by(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Periodo) %>%
    summarise(TBM = median(TBM)) %>%
    pivot_wider(names_from = "Periodo", values_from = "TBM") %>%
    left_join(Serie %>%
                group_by(NOM_MUN) %>%
                summarise(`2000-2019` = median(TBM))) %>%
    arrange(NOM_MUN) %>%
    return()
}

MedianSeparados <- function(Serie){
  Serie %>%
    mutate(Periodo = ifelse(Anio < 2010, "2000-2009", "2010-2019")) %>% 
    group_by(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, Periodo) %>%
    summarise(TBM = median(TBM)) %>%
    pivot_wider(names_from = "Periodo", values_from = "TBM") %>%
    left_join(Serie %>%
                group_by(NOM_MUN, Edad) %>%
                summarise(`2000-2019` = median(TBM))) %>%
    arrange(NOM_MUN) %>%
    return()
}

# función para calcular pendientes de la serie de tiempo total y por década ----
PendienteJuntos <- function(Serie){
  cve <- Serie %>%
    transmute(CVE = paste(CVE_ENT, CVE_MUN)) %>%
    unique() %>%
    pull()
  
  Serie <- Serie %>% 
    mutate(CVE = paste(CVE_ENT, CVE_MUN))
  
  tLineal <- tibble(CVE = character(), `2000-2019` = numeric(), 
                    `2000-2009` = numeric(), `2010-2019` = numeric()) %>%
    bind_rows(as_tibble_col(cve, column_name = "CVE")) %>%
    column_to_rownames(var = "CVE")
  
  for(i in cve){
    t1 <- Serie %>%
      filter(CVE == i) %>%
      arrange(Anio)
    
    tLineal[i,1] <- lm(data = t1, TBM ~ Anio)$coeff[2]
    tLineal[i,2] <- lm(data = t1 %>% filter(Anio < 2010), TBM ~ Anio)$coeff[2]
    tLineal[i,3] <- lm(data = t1 %>% filter(Anio >= 2010), TBM ~ Anio)$coeff[2]
  }
  tLineal %>%
    rownames_to_column(var = "CVE") %>%
    mutate(CVE_ENT = str_sub(CVE, 1,2),
           CVE_MUN = str_sub(CVE,-3)) %>%
    left_join(Mpios) %>%
    select(CVE_ENT:Microcuenca, 3, 4, 2) %>%
    return()
}

PendienteSeparados <- function(Serie){
  cve <- Serie %>%
    transmute(CVE = paste(CVE_ENT, CVE_MUN, Edad)) %>%
    unique() %>%
    pull()
  
  Serie <- Serie %>% 
    mutate(CVE = paste(CVE_ENT, CVE_MUN, Edad))
  
  tLineal <- tibble(CVE = character(), `2000-2019` = numeric(), 
                    `2000-2009` = numeric(), `2010-2019` = numeric()) %>%
    bind_rows(as_tibble_col(cve, column_name = "CVE")) %>%
    column_to_rownames(var = "CVE")
  
  for(i in cve){
    t1 <- Serie %>%
      filter(CVE == i) %>%
      arrange(Anio)
    
    tLineal[i,1] <- lm(data = t1, TBM ~ Anio)$coeff[2]
    tLineal[i,2] <- lm(data = t1 %>% filter(Anio < 2010), TBM ~ Anio)$coeff[2]
    tLineal[i,3] <- lm(data = t1 %>% filter(Anio >= 2010), TBM ~ Anio)$coeff[2]
  }
  tLineal %>%
    rownames_to_column(var = "CVE") %>%
    mutate(CVE_ENT = str_sub(CVE, 1,2),
           CVE_MUN = str_sub(CVE, 4,6),
           Edad = str_sub(CVE, 8)) %>%
    left_join(Mpios) %>%
    select(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, 3, 4, 2) %>%
    return()
}

# función para calcular prueba de Mann-Kendall ----
MannKendallJuntos <- function(Serie){
  cve <- Serie %>%
    transmute(CVE = paste(CVE_ENT, CVE_MUN)) %>%
    unique() %>%
    pull()
  
  Serie <- Serie %>% 
    mutate(CVE = paste(CVE_ENT, CVE_MUN))
  
  tKendall <- tibble(CVE = character(), `2000-2019` = numeric(), 
                     `2000-2009` = numeric(), `2010-2019` = numeric()) %>%
    bind_rows(as_tibble_col(cve, column_name = "CVE")) %>%
    column_to_rownames(var = "CVE")
  
  for(i in cve){
    t1 <- Serie %>%
      filter(CVE == i) %>%
      arrange(Anio)
    
    tKendall[i,1] <- MannKendall(t1$TBM) %>%
      tidy() %>%
      select(p.value)
    tKendall[i,2] <- MannKendall(t1 %>% 
                                   filter(Anio < 2010) %>%
                                   pull(TBM)) %>%
      tidy() %>%
      select(p.value)
    tKendall[i,3] <- MannKendall(t1 %>% 
                                   filter(Anio >= 2010) %>%
                                   pull(TBM)) %>%
      tidy() %>%
      select(p.value)
  }
  
  tKendall %>%
    rownames_to_column(var = "CVE") %>%
    mutate(CVE_ENT = str_sub(CVE, 1,2),
           CVE_MUN = str_sub(CVE,-3)) %>%
    left_join(Mpios) %>%
    select(CVE_ENT:Microcuenca, 3, 4, 2) %>%
    return()
}

MannKendallSeparados <- function(Serie){
  cve <- Serie %>%
    transmute(CVE = paste(CVE_ENT, CVE_MUN, Edad)) %>%
    unique() %>%
    pull()
  
  Serie <- Serie %>% 
    mutate(CVE = paste(CVE_ENT, CVE_MUN, Edad))
  
  tKendall <- tibble(CVE = character(), `2000-2019` = numeric(), 
                     `2000-2009` = numeric(), `2010-2019` = numeric()) %>%
    bind_rows(as_tibble_col(cve, column_name = "CVE")) %>%
    column_to_rownames(var = "CVE")
  
  for(i in cve){
    t1 <- Serie %>%
      filter(CVE == i) %>%
      arrange(Anio)
    
    tKendall[i,1] <- MannKendall(t1$TBM) %>%
      tidy() %>%
      select(p.value)
    tKendall[i,2] <- MannKendall(t1 %>% 
                                   filter(Anio < 2010) %>%
                                   pull(TBM)) %>%
      tidy() %>%
      select(p.value)
    tKendall[i,3] <- MannKendall(t1 %>% 
                                   filter(Anio >= 2010) %>%
                                   pull(TBM)) %>%
      tidy() %>%
      select(p.value)
  }
  
  tKendall %>%
    rownames_to_column(var = "CVE") %>%
    mutate(CVE_ENT = str_sub(CVE, 1,2),
           CVE_MUN = str_sub(CVE, 4,6),
           Edad = str_sub(CVE, 8)) %>%
    left_join(Mpios) %>%
    select(CVE_ENT, CVE_MUN, NOM_MUN, Microcuenca, Edad, 3, 4, 2) %>%
    return()
}

# playground ----
# library(ggplot2)
# Defun %>%
#   filter(Causa1 == iCausa1) %>%
#   filter(Causa2 >= iCausa2Min & Causa2 <= iCausa2Max) %>%
#   right_join(valEdad, by = c("Edad" = "value")) %>%
#   group_by(Anio, CVE_ENT, Sexo) %>%
#   summarise(Defunciones = sum(Defunciones)) %>%
#   right_join(PobAñosSexo %>%
#                right_join(valEdad, 
#                           by = c("Edad" = "value")) %>%
#                group_by(Anio, CVE_ENT, Sexo) %>%
#                summarise(Poblacion = sum(Poblacion))) %>% 
#   replace_na(list(Defunciones = 0)) %>%
#   ungroup() %>%
#   mutate(TBM = Defunciones / Poblacion * 1000) %>% 
#   group_by(CVE_ENT, Sexo) %>%
#   summarise(Media = mean(TBM, na.rm = T)) %>% view()
# 
# Defun %>%
#   filter(Causa1 == iCausa1) %>%
#   filter(Causa2 >= iCausa2Min & Causa2 <= iCausa2Max) %>%
#   right_join(valEdad, by = c("Edad" = "value")) %>%
#   group_by(Anio, Sexo) %>%
#   summarise(Defunciones = sum(Defunciones)) %>%
#   right_join(PobAñosSexo %>%
#                right_join(valEdad, 
#                           by = c("Edad" = "value")) %>%
#                group_by(Anio, Sexo) %>%
#                summarise(Poblacion = sum(Poblacion))) %>% 
#   replace_na(list(Defunciones = 0)) %>%
#   ungroup() %>%
#   mutate(TBM = Defunciones / Poblacion * 1000) %>% 
#   group_by(Sexo) %>%
#   summarise(Media = mean(TBM, na.rm = T)) %>% view()
