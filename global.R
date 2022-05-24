library(readr)
library(ggplot2)
library(tidyr)
library(data.table)
library(knitr)
library(DT)
library(stringi)
library(kableExtra)
library(apyramid)
library(incidence2)
library(lubridate)
library(ggplotify)
library(openxlsx)
# library(ggtree)
# library(ape)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(shinydashboard)
library(shinycustomloader)
library(shiny)
library(plotly)
library(dplyr)


## Carga de datos
### Datos para mapas de departamentos
departamentos_shapes = st_read("data/departamentos_gtm")
departamentos_shapes = st_transform(departamentos_shapes, "+proj=longlat +datum=WGS84") %>%
  rename(departamento = nombre)
  
departamentosGeo = read_csv('data/departamentosGeo.csv')
departamentosGeo = departamentosGeo %>%
  mutate(
    departamento = tolower(as.character(departamento)),
    departamento = gsub('á', 'a', departamento),
    departamento = gsub('é', 'e', departamento),
    departamento = gsub('í', 'i', departamento),
    departamento = gsub('ó', 'o', departamento),
    departamento = gsub('ú', 'u', departamento),
    departamento = toupper(departamento)
  )

departamentos = departamentosGeo %>% 
  filter(departamento != 'TODOS') %>% 
  select(departamento) %>%
  unique()

### Datos para mapas de DAS



areas_shapes = st_read('data/DAS_shapes')
areas_shapes = st_transform(areas_shapes, "+proj=longlat +datum=WGS84")
areas_shapes = areas_shapes %>%
  rename(
    `ÁREA DE SALUD` = NAME_AREA
  ) %>%
  mutate(
    `ÁREA DE SALUD` = gsub('á', 'a', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('é', 'e', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('í', 'i', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('ó', 'o', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('ú', 'u', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = toupper(`ÁREA DE SALUD`),
    `ÁREA DE SALUD` = case_when(
      `ÁREA DE SALUD` == "PETEN SUR ORIENTAL" ~ "PETEN SUR ORIENTE",
      `ÁREA DE SALUD` == "PETEN SUR OCCIDENTAL" ~ "PETEN SUR OCCIDENTE",
      `ÁREA DE SALUD` == "EL QUICHE" ~ "QUICHE",
      T ~ `ÁREA DE SALUD`
    )
  )
areasGeo = read_csv('data/dasGeo.csv') %>%
  rename(
    `ÁREA DE SALUD` = DAS
  ) %>%
  mutate(
    `ÁREA DE SALUD` = tolower(`ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('á', 'a', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('é', 'e', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('í', 'i', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('ó', 'o', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = gsub('ú', 'u', `ÁREA DE SALUD`),
    `ÁREA DE SALUD` = toupper(`ÁREA DE SALUD`),
    `ÁREA DE SALUD` = case_when(
      `ÁREA DE SALUD` == "PETEN SUR ORIENTAL" ~ "PETEN SUR ORIENTE",
      `ÁREA DE SALUD` == "PETEN SUR OCCIDENTAL" ~ "PETEN SUR OCCIDENTE",
      `ÁREA DE SALUD` == "EL QUICHE" ~ "QUICHE",
      T ~ `ÁREA DE SALUD`
    )
  )

areas = areasGeo %>% 
  filter(`ÁREA DE SALUD` != 'TODOS') %>% 
  select(`ÁREA DE SALUD`) %>%
  unique()

### Variantes y VOC
variants = read_csv('data/pangoVariants.csv', guess_max = 5000)


#tree <- ape::read.tree("gisaid.mafft.fasta.parstree")


### DATOS DEL LNS
lns2022 = read.xlsx('data/BASE SEQ 2022.xlsx', sheet = 2, sep.names = ' ') %>% dplyr::filter( !is.na(`RESULTADO FINAL`))

lns20y21 = read_csv('data/lns20y21.csv', guess_max = 5000) 
lns20y21 = lns20y21 %>%
  mutate(
    AÑOS = as.numeric(AÑOS),
    `FECHA DE TOMA DE MUESTRA` = as.Date(`FECHA DE TOMA DE MUESTRA`, '%Y-%m-%d'),
    `FECHA INGRESO DE MUESTRA` = as.Date(`FECHA INGRESO DE MUESTRA`, '%Y-%m-%d')
  )

lns2022 = lns2022 %>%
  filter(
    !is.na(`RESULTADO FINAL`),
    `RESULTADO FINAL` != "CALIDAD DE MUESTRA NO APTA PARA SECUENCIAR",
    `RESULTADO FINAL` != "NO APLICA"
  ) %>%
  select(
    `FECHA DE NACIMIENTO`,
    `ID COVID`,
    `ÁREA DE SALUD`,
    `DISTRITO/HOSPITAL`,
    SEXO,
    AÑOS,
    `FECHA DE TOMA DE MUESTRA`,
    `FECHA INGRESO DE MUESTRA`,
    VACUNA,
    `RESULTADO FINAL`,
    `DENOMINACIÓN OMS`
  ) %>%
  mutate(
    AÑOS = as.numeric(AÑOS),
    `FECHA DE TOMA DE MUESTRA` = as.Date(as.numeric(`FECHA DE TOMA DE MUESTRA`), origin = "1899-12-30"),
    `FECHA INGRESO DE MUESTRA` = as.Date(as.numeric(`FECHA INGRESO DE MUESTRA`), origin = "1899-12-30")
  )

lns = rbind(lns20y21, lns2022)

lns = lns %>%
  filter(
    !is.na(`RESULTADO FINAL`),
    `RESULTADO FINAL` != "CALIDAD DE MUESTRA NO APTA PARA SECUENCIAR",
    `RESULTADO FINAL` != "NO APLICA",
    `RESULTADO FINAL` != "DESCONOCIDO"
  ) %>%
  mutate(
    `ÁREA DE SALUD` = case_when(is.na(`ÁREA DE SALUD`) ~ 'NO REFIERE',
                                `ÁREA DE SALUD` == 'GuateMALA NOR ORIENTE' ~'GUATEMALA NOR ORIENTE',
                                `ÁREA DE SALUD` == 'QUICHE ' ~'QUICHE',
                                T ~ `ÁREA DE SALUD`),
    `RESULTADO FINAL` = case_when(
      is.na(`RESULTADO FINAL`) ~ 'DESCONOCIDO',
      #`RESULTADO FINAL` == 'NO APLICA' ~ 'DESCONOCIDO',
      `RESULTADO FINAL` == "CALIDAD DE MUESTRA NO APTA PARA SECUENCIAR" ~ 'NO APLICA',
      T ~ `RESULTADO FINAL`
    ),
    pangolin = case_when(`RESULTADO FINAL` %like% 'AY' ~ 'AY',
                         `RESULTADO FINAL` %like% 'BA' ~ 'BA',
                         `RESULTADO FINAL` %like% 'B.1.351' ~ 'B.1.351',
                         `RESULTADO FINAL` %like% 'P.1' ~ 'P.1',
                         `RESULTADO FINAL` %like% 'Q' ~ 'Q',
                         T ~ `RESULTADO FINAL`),
    grupo_etario = case_when(AÑOS < 10 ~ "0-9 años",
                             AÑOS >= 10 & AÑOS < 20 ~ "10-19 años",
                             AÑOS >= 20 & AÑOS < 30 ~ "20-29 años",
                             AÑOS >= 30 & AÑOS < 40 ~ "30-39 años",
                             AÑOS >= 40 & AÑOS < 50 ~ "40-49 años",
                             AÑOS >= 50 & AÑOS < 60 ~ "50-59 años",
                             AÑOS >= 60 & AÑOS < 70 ~ "60-69 años",
                             AÑOS >= 70 & AÑOS < 80 ~ "70-79 años",
                             AÑOS >= 80 ~ "80+ años",
                             is.na(AÑOS) ~"SIN DATOS"),
    grupo_etario = factor(grupo_etario,
                          levels = c("0-9 años","10-19 años","20-29 años","30-39 años",
                                     "40-49 años","50-59 años","60-69 años","70-79 años",
                                     "80+ años","SIN DATOS")),
    departamento = case_when(`ÁREA DE SALUD` %like% 'GUATEMALA' ~ 'GUATEMALA',
                             `ÁREA DE SALUD` %like% 'PETEN' ~ 'PETEN',
                             `ÁREA DE SALUD` == 'IXCAN' ~ 'QUICHE',
                             `ÁREA DE SALUD` == 'IXIL' ~ 'QUICHE',
                             T ~ `ÁREA DE SALUD`)
  )

lns = dplyr::left_join(lns, variants)  %>%
  dplyr::rename(Variante = variante) %>%
  mutate(
    Variante = case_when(is.na(Variante) ~ case_when(`RESULTADO FINAL` == 'NO APLICA' ~ 'Desconocido',
                                                     `RESULTADO FINAL` == 'DESCONOCIDO' ~ 'Desconocido',
                                                     T ~ 'Otro'
    ), 
    T ~ Variante),
    Variante = factor(Variante,
                      levels=c('Omicron', 'Delta', 'Gamma', 'Beta', 'Alpha', 'Iota', 'Epsilon', 'Eta', 'Mu', 'Otro', 'Desconocido')),
    pangoVoc = paste(`RESULTADO FINAL`, Variante),
    SEXO = case_when(
      is.na(SEXO) ~ 'DESCONOCIDO',
      T ~ SEXO
    )
  )



## Colores para las gráficas
colorVariants = c("Alpha" = "#FEE08B", 
                  "Beta" = "#FDAE61", 
                  "Gamma" = "#F46D43", 
                  "Epsilon" = "#E6F598", 
                  "Eta" = "#3288BD", 
                  "Iota" = "#FFFFBF", 
                  "Kappa" = "#3288BD", 
                  "Mu" = "#ABDDA4", 
                  "Zeta" = "#66C2A5", 
                  "Delta" = "#D53E4F", 
                  "Omicron" = "#9E0142",
                  "Otro" = "#176BA0",
                  "Desconocido" = "#142459")




# options(repos = BiocManager::repositories())


# library(BiocManager)


Sys.setenv(TZ='America/Guatemala')

