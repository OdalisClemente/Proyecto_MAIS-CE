###########################################################################
###########################################################################
# Manual Integral de Salud en Contextos Educativos ------------------------
###########################################################################
###########################################################################


# Pquetes -----------------------------------------------------------------

# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)

# Datos ---------------------------------------------------------------------

# MAIS-CE

Mais <- read_excel("Mais.xlsx") %>% 
  clean_names()  

# INEVAL 
used_evalu <- read_excel("2015.xlsx") %>% 
  clean_names()


used_evalu %>% glimpse() # con esta revision identifuicamos en los valores con errores en la base


# Reemplazar comas por puntos y convertir a numérico
used_evalu <- used_evalu %>%
  mutate(across(c(inev, imat, ilyl, icn, ies), 
                ~ as.numeric(gsub(",", ".", .))))

# Crear un diccionario para asignar los nombres de las provincias a los números
catalogo_provincias <- data.frame(
  codigo = 1:24,  # Los códigos de las provincias
  provincia = c("AZUAY", "BOLIVAR", "CAÑAR", "CARCHI", "COTOPAXI", 
                "CHIMBORAZO", "EL ORO", "ESMERALDAS", "GUAYAS", 
                "IMBABURA", "LOJA", "LOS RIOS", "MANABI", 
                "MORONA SANTIAGO", "NAPO", "PASTAZA", "PICHINCHA", 
                "TUNGURAHUA", "ZAMORA CHINCHIPE", "GALAPAGOS", 
                "SUCUMBIOS", "ORELLANA", "SANTO DOMINGO DE LOS TSACHILAS", 
                "SANTA ELENA"))

# Transformar la columna de códigos numéricos en nombres de provincias
used_evalu <- used_evalu %>%
  mutate(id_prov = recode(id_prov, 
                          `1` = "AZUAY", `2` = "BOLIVAR", `3` = "CAÑAR", 
                          `4` = "CARCHI", `5` = "COTOPAXI", `6` = "CHIMBORAZO", 
                          `7` = "EL ORO", `8` = "ESMERALDAS", `9` = "GUAYAS", 
                          `10` = "IMBABURA", `11` = "LOJA", `12` = "LOS RIOS", 
                          `13` = "MANABI", `14` = "MORONA SANTIAGO", 
                          `15` = "NAPO", `16` = "PASTAZA", 
                          `17` = "PICHINCHA", `18` = "TUNGURAHUA", 
                          `19` = "ZAMORA CHINCHIPE", `20` = "GALAPAGOS", 
                          `21` = "SUCUMBIOS", `22` = "ORELLANA", 
                          `23` = "SANTO DOMINGO DE LOS TSACHILAS", 
                          `24` = "SANTA ELENA"))

# Verificar los cambios
Mais %>% glimpse()  
used_evalu %>% glimpse() 


# Joins -------------------------------------------------------------------

# 1. Crear un "catálogo" de provincias
catalogo <- Mais %>% 
  distinct(provincia, cod_provi) %>% 
  rename(codigo = cod_provi, provincia = provincia)

# 2. Hacer un inner join
ineval_codigo <- used_evalu %>% 
  inner_join(catalogo, by = c("id_prov" = "provincia")) # Join por provincia



# Descriptivos ----------------------------------------------------------

# Análisis de la base de datos del MAIS (Provincias y Zona de planificacion)

# Frecuencia de las zonas 
Mais %>%
  count(zona) %>%
  ggplot(aes(x = zona, y = n, fill = n)) +  # Asigna el color basado en la frecuencia (n)
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "pink", high = "#CD8162") +  # Degradado de color
  theme_minimal() +
  labs(title = "Distribución de Zonas", x = "Zona", y = "Frecuencia", fill = "Frecuencia")


# Frecuencia de las provincias 
Mais %>%
  count(provincia) %>%
  ggplot(aes(x = fct_reorder(provincia, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#D2B48C", high = "#00E5EE") +  # Degradado de color
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribución de Provincias", x = "Provincia", y = "Frecuencia", fill = "Frecuencia")


# Datos del INEVAL --------------------------------------------------------

# Calcular las calificaciones promedio por provincia
promedio_ineval <- 
  ineval_codigo %>%
  filter(between(inev, 1, 10)) %>%  # Filtrar valores entre 1 y 10
  group_by(id_prov) %>%  # Agrupar por provincia
  summarise(promedio_ineval = mean(inev, na.rm = TRUE))  # Calcular promedio


# Visualizar los resultados del promedio grafucamente
ggplot(promedio_ineval, (aes(x = id_prov, y = promedio_ineval, fill = promedio_ineval))) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "#E056FF") +  # Degradado de color
  labs(title = "Promedio de Calificaciones de INEVAL por Provincia",
       x = "Provincia", 
       y = "Promedio de Calificaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# POR MEJOR MATERIA -------------------------------------------------------
# Filtrar las calificaciones fuera del rango para todas las materias
ineval_codigo_filtrado <- ineval_codigo %>%
  filter(between(imat, 1, 10) &
           between(ilyl, 1, 10) &
           between(icn, 1, 10) &
           between(ies, 1, 10))

# Calcular los promedios por materia después del filtrado
promedios_materias <- ineval_codigo_filtrado %>%
  group_by(id_prov) %>%  # Agrupar por provincia
  summarise(
    promedio_imat = mean(imat, na.rm = TRUE),
    promedio_ilyl = mean(ilyl, na.rm = TRUE),
    promedio_icn = mean(icn, na.rm = TRUE),
    promedio_ies = mean(ies, na.rm = TRUE)
  )

# Reestructurar los datos de "wide" a "long"
promedios_materias_long <- promedios_materias %>%
  pivot_longer(cols = starts_with("promedio_"), 
               names_to = "materia", 
               values_to = "promedio") %>%
  mutate(materia = recode(materia, 
                          "promedio_imat" = "IMAT",
                          "promedio_ilyl" = "ILYL",
                          "promedio_icn" = "ICN",
                          "promedio_ies" = "IES"))

# Visualizar los promedios por materia en cada provincia
ggplot(promedios_materias_long, aes(x = id_prov, y = promedio, fill = materia)) +
  geom_bar(stat = "identity", position = "dodge") +  # Usar 'dodge' para barras separadas
  theme_minimal() +
  labs(title = "Promedio de Calificaciones por Materia y Provincia",
       x = "Provincia", 
       y = "Promedio de Calificación",
       fill = "Materia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas


