################################################################
# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)
library(haven)
library(labelled)

# Datos de las Escuelas ---------------------------------------------------

Mais <- read_excel("Data/MINEDUC/Escuelas.xlsx")%>% 
  clean_names() 

Mais %>% glimpse() # Ver el formato de los datos

library(labelled)

# Definir las etiquetas
prov_labels <- c(
  "01" = "Azuay",
  "02" = "Bolívar",
  "03" = "Cañar",
  "04" = "Carchi",
  "05" = "Cotopaxi",
  "06" = "Chimborazo",
  "07" = "El Oro",
  "08" = "Esmeraldas",
  "09" = "Guayas",
  "10" = "Imbabura",
  "11" = "Loja",
  "12" = "Los Ríos",
  "13" = "Manabí",
  "14" = "Morona Santiago",
  "15" = "Napo",
  "16" = "Pastaza",
  "17" = "Pichincha",
  "18" = "Tungurahua",
  "19" = "Zamora Chinchipe",
  "20" = "Galápagos",
  "21" = "Sucumbíos",
  "22" = "Orellana",
  "23" = "Santo Domingo",
  "24" = "Santa Elena",
  "90" = "Zona No Delimitada",
  "98" = "Exterior"
)

# Asignar las etiquetas
Mais$cod_provi <- set_value_labels(Mais$cod_provi, prov_labels)

# Mostrar las etiquetas asociadas
val_labels(Mais$cod_provi)

# Inspeccionar cómo aparecen los datos
head(Mais$cod_provi)


# Eliminamos los valores NA y los tranfromamos los nuemricos a cero y los categoricos a desconocido

Mais <- Mais %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)),    # Reemplaza NA por 0 en columnas numéricas
         across(where(is.character), ~ replace_na(.x, "Desconocido")))  # Reemplaza NA en columnas de texto por "Desconocido"

# Verifico antes y depsues de la eliminacion de los NA 
unique(Mais$zona)  # ver la categoria de la variable


# Frecuencia de las zonas 
Mais %>%
  count(zona) %>%
  ggplot(aes(x = zona, y = n, fill = n)) +  # Asigna el color basado en la frecuencia (n)
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "red", high = "#CD1162") +  # Degradado de color
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


# Lista base de las escuelas. 

