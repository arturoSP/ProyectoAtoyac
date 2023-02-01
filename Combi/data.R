# lectura de datos base ----

# población por municipio y grupos de edad ----
PobAños <- readr::read_csv("./www/Pobl0019_Repositorio.csv", 
                    col_names = TRUE, col_types = "dffdddddddddddddddddddddd") %>%
  select(Anio, CVE_ENT, CVE_MUN, 
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
  group_by(Anio, CVE_ENT, CVE_MUN, Edad) %>%
  summarise(Poblacion = sum(Poblacion))

# lista de municipios ----
Mpios <- readr::read_csv("./www/Mun_Microcuencas.csv", col_names = TRUE, col_types = "cccc")%>%
  mutate(CVE_MUN = stringr::str_pad(CVE_MUN, width = 3, side= "left", pad = "0"))

# defunciones por municipio y grupos de edad ----
Defun <- readr::read_csv("./www/DefunCausa_Repositorio_3.csv", col_names = TRUE, col_types= "dccfffcd")%>%
  mutate(CVE_MUN = stringr::str_pad(CVE_MUN, width = 3, side= "left", pad = "0"),
         Edad = factor(Edad, 
                       levels = c("1a infancia", "Infancia", "Pubertad", "Adolescencia",
                                  "Juventud", "Adultez", "Vejez"),
                       labels = c("0-9", "0-9", "10-19", "10-19",
                                  "20-24", "25-59", "60+"),
                       ordered = is.ordered(.$Edad)))%>%
  group_by(Anio, CVE_ENT, CVE_MUN, Edad, CIE10) %>%
  summarise(Defunciones = sum(Defunciones)) %>%
  mutate(Causa1 = stringr::str_sub(CIE10,1,1), 
         Causa2 = as.numeric(stringr::str_sub(CIE10,2,3)))

# tabla con datos de cie10 y nombres
codigo <- readr::read_csv("./www/CausaCIE.csv", col_names = c("CIE10", "Grupo"), skip = 1)
