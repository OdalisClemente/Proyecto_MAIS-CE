###########################################################################
###########################################################################
# --------------------------LIMPIEZA DE DATOS -----------------------------
###########################################################################
###########################################################################

# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)
library(haven)
library(labelled)

# Base de Datos de las Escuelas ------------------------------------------------

Mais <- read_excel("Data/MINEDUC/Escuelas.xlsx")%>% 
  janitor::clean_names()

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
lapply(Mais, unique)   

# Bases de Datos del Ineval --------------------------------------------------------
X2015 <- read_sav("Data/MINEDUC/2015.sav")%>%   
  janitor::clean_names()
X2016 <- read_sav("Data/MINEDUC/2016.sav")%>%   
  janitor::clean_names()
X2017 <- read_sav("Data/MINEDUC/2017.sav")%>% 
  janitor::clean_names()
X2018 <- read_sav("Data/MINEDUC/2018.sav")%>% 
  janitor::clean_names()
X2019 <- read_sav("Data/MINEDUC/2019.sav")%>% 
  janitor::clean_names()
#X2020 <- read_sav("Data/MINEDUC/2020.sav")%>% 
#  janitor::clean_names()

# Añadir una columna de año para identificar cada conjunto de datos
X2015 <- X2015 %>% mutate(anio = 2015)
X2016 <- X2016 %>% mutate(anio = 2016)
X2017 <- X2017 %>% mutate(anio = 2017)
X2018 <- X2018 %>% mutate(anio = 2018)
X2019 <- X2019 %>% mutate(anio = 2019)
#X2020 <- X2019 %>% mutate(anio = 2020)

# Unir todas las bases en una sola
Datos_Unidos <- bind_rows(X2015, X2016, X2017, X2018, X2019)

# Ahora creo base de datos solo con las variables que se consideran usar
Base <- Datos_Unidos %>% 
  select(amie, nm_regi, id_prov, financiamiento, tp_sexo, tp_area, nl_inev, nl_imat, nl_ilyl, nl_icn, nl_ies, anio)

#-------------------------------------------------------------------------------
# Limpieza de Valores (NA) -----------------------------------------------------
#-------------------------------------------------------------------------------

sum(is.na(Base$amie))  #  Libre de valores NA 

Base <- Base %>%
  mutate(amie = as.character(amie)) %>%  # Convertir 'amie' a tipo character
  filter(amie != "999999")               # Eliminar filas con amie igual a "999999"


# Regiones (Costa, Sierra, Oriente, etc.)
sum(is.na(Base$nm_regi))  #  Libre de valores NA 

# Provincias
sum(is.na(Base$id_prov))  # Libre de valores NA

# Financiamiento
sum(is.na(Base$financiamiento)) # Tiene 817097 valores NA

Base <- Base %>%
  mutate(financiamiento = ifelse(is.na(financiamiento), 4, financiamiento))

Base <- Base %>%
  mutate(financiamiento = labelled(financiamiento, 
                                   labels = c("Público (Fiscal y Municipal)" = 1, "Privado (Particular)" = 2, "Mixto (Fiscomisional)" = 3,  "Desconocido" = 4)))
# Sexo
sum(is.na(Base$tp_sexo)) # Esta limpia de valores NA

Base <- Base %>%
  mutate(tp_sexo = ifelse(tp_sexo == 999999, NA, tp_sexo))

# Tipo de Area
sum(is.na(Base$tp_area)) # Esta limpia de valores NA 

# Nivel de logro alcanzado por el sustentante en la Nota de Examen de Grado
sum(is.na(Base$nl_inev)) # Esta variable posee 868889 NA 

# Transforme los NA y los valores erroneos a una sola categoria
Base <- Base %>%
  mutate(
    nl_inev = case_when(
      is.na(nl_inev) ~ 4,                     # Reemplaza los NA por 4
      nl_inev == 999999 ~ 4,                 # Cambia los "No aplica" por 4
      TRUE ~ nl_inev  ))                       # Conserva los valores originales para el resto

# Aqui agregue las categorias de las variaables 
Base <- Base %>%
  mutate(
    nl_inev = labelled(
      nl_inev,
      labels = c(
        "Insuficiente" = 0, "Elemental" = 1, "Satisfactorio" = 2, "Excelente" = 3, "Desconocido" = 4)))

# Matematicas 
sum(is.na(Base$nl_imat)) # Existen 868646 NA, se realiza lo mismo del paso anterior. 

Base <- Base %>%
  mutate(
    nl_imat = case_when(
      is.na(nl_imat) ~ 4,           # Reemplaza los NA por 4
      nl_imat == 999999 ~ 4,        # Cambia los "No aplica" por 4
      TRUE ~ nl_imat ))             # Conserva los valores originales para el resto

Base <- Base %>%
  mutate(
    nl_imat = labelled(
      nl_imat,
      labels = c("Insuficiente" = 0, "Elemental" = 1, "Satisfactorio" = 2, "Excelente" = 3,  "Desconocido" = 4)))

# Lengua y Literatura
sum(is.na(Base$nl_ilyl)) # Existen 868646 NA, se realiza lo mismo del paso anterior. 

Base <- Base %>%
  mutate(
    nl_ilyl = case_when(
      is.na(nl_ilyl) ~ 4,             # Reemplaza los NA por 4
      nl_ilyl == 999999 ~ 4,          # Cambia los "No aplica" por 4
      TRUE ~ nl_ilyl))                # Conserva los valores originales para el resto


Base <- Base %>%
  mutate(
    nl_ilyl = labelled(
      nl_ilyl,
      labels = c("Insuficiente" = 0, "Elemental" = 1, "Satisfactorio" = 2, "Excelente" = 3, "Desconocido" = 4)))


# Ciencias Naturales 
sum(is.na(Base$nl_icn)) # Existen 868324 NA, se realiza lo mismo del paso anterior. 

Base <- Base %>%
  mutate(
    nl_icn = case_when(
      is.na(nl_icn) ~ 4,             # Reemplaza los NA por 4
      nl_icn == 999999 ~ 4,          # Cambia los "No aplica" por 4
      TRUE ~ nl_icn ))                 # Conserva los valores originales para el resto


Base <- Base %>%
  mutate(
    nl_icn = labelled(
      nl_icn,
      labels = c("Insuficiente" = 0, "Elemental" = 1, "Satisfactorio" = 2,  "Excelente" = 3,  "Desconocido" = 4)))


# Estudios Sociales 
sum(is.na(Base$nl_ies)) # Existen 868324 NA, se realiza lo mismo del paso anterior. 

Base <- Base %>%
  mutate(
    nl_ies = case_when(
      is.na(nl_ies) ~ 4,             # Reemplaza los NA por 4
      nl_ies == 999999 ~ 4,          # Cambia los "No aplica" por 4
      TRUE ~ nl_ies  ))                # Conserva los valores originales para el resto

Base <- Base %>%
  mutate(
    nl_ies = labelled(
      nl_ies,
      labels = c("Insuficiente" = 0, "Elemental" = 1, "Satisfactorio" = 2, "Excelente" = 3, "Desconocido" = 4)))

# Verificar valores 
lapply(Base, unique) 

# Verificar si existen valores NA en toda la base de datos
any(is.na(Base))


# JOINS -------------------------------------------------------------------

# Base y Mais 

# Crear la variable 'intervencion' en la base Mais
Mais <- Mais %>%
  mutate(intervencion = 1)  # En Mais, todas las escuelas tendrán valor 1

# Crear la variable 'intervencion' en la base Base con valor 0 por defecto
Base <- Base %>%
  mutate(intervencion = 0)  # En Base, todas las escuelas inicialmente tendrán valor 0

# Unir las dos bases y consolidar la variable 'intervencion' en una sola columna
Base_Final <- Base %>%
  left_join(Mais %>%
              select(amie, intervencion), by = "amie") %>%
  mutate(intervencion = ifelse(!is.na(intervencion.y), 1, intervencion.x)) %>%
  select(-intervencion.x, -intervencion.y)  # Eliminar columnas redundantes


any(is.na(Base_Final)) # Ver si posee la base valores NA



# Descriptivos ------------------------------------------------------------

# Crear un resumen del número de escuelas por año y por intervención
data_resumen <- Base_Final %>%
  group_by(anio, intervencion) %>%
  summarise(amie = n(), .groups = "drop")  # Contar las escuelas

data_resumen

# Generar el gráfico con ggplot2
ggplot(data_resumen, aes(x = as.factor(anio), y = amie, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +  # Barras agrupadas
  labs(
    title = "Número de escuelas con y sin intervención por año",
    x = "Año",
    y = "Número de escuelas",
    fill = "Intervención\n(0 = No, 1 = Sí)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("0" = "purple", "1" = "green"))  # Colores personalizados




# Generar el gráfico de evolución
ggplot(data_resumen, aes(x = anio, y = amie, color = as.factor(intervencion), group = intervencion)) +
  geom_line(size = 1.2) +  # Añadir líneas
  geom_point(size = 3) +   # Añadir puntos para resaltar valores
  labs(
    title = "Evolución del número de escuelas con y sin intervención (2015-2019)",
    x = "Año",
    y = "Número de escuelas",
    color = "Intervención\n(0 = No, 1 = Sí)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("0" = "purple", "1" = "green")) +  # Colores personalizados
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )
















