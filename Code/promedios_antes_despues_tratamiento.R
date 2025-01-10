# Calcular los promedios de antes y despues del tratamiento. 
#Podemos ver los pasos en la Figura 18.4 . Primero, calculamos cuatro promedios: antes del tratamiento
#en el grupo tratado (California), después del tratamiento en el grupo tratado, antes del tratamiento 
#en el grupo no tratado y después del tratamiento en el grupo no tratado.Figura 18.4 (a)



# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)
library(haven)
library(labelled)
library(lfe)  # Para la regresión de efectos fijos
library(modelsummary)
library(fixest)


# Base de Datos de las Escuelas ------------------------------------------------
Mais <- read_excel("Data/MINEDUC/Escuelas.xlsx") %>% 
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

# Eliminamos los valores NA y los transformamos: numéricos a cero y texto a "Desconocido"
Mais <- Mais %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)),    # Reemplaza NA por 0 en columnas numéricas
         across(where(is.character), ~ replace_na(.x, "Desconocido")))  # Reemplaza NA en columnas de texto por "Desconocido"
lapply(Mais, unique)   

# Bases de Datos del Ineval --------------------------------------------------------
X2015 <- read_sav("Data/MINEDUC/2015.sav") %>%   
  janitor::clean_names()
X2016 <- read_sav("Data/MINEDUC/2016.sav") %>%   
  janitor::clean_names()
X2017 <- read_sav("Data/MINEDUC/2017.sav") %>% 
  janitor::clean_names()
X2018 <- read_sav("Data/MINEDUC/2018.sav") %>% 
  janitor::clean_names()
X2019 <- read_sav("Data/MINEDUC/2019.sav") %>% 
  janitor::clean_names()
X2020 <- read_sav("Data/MINEDUC/2020.sav") %>% 
  janitor::clean_names()


# Añadir una columna de año para identificar cada conjunto de datos
X2015 <- X2015 %>% mutate(anio = 2015)
X2016 <- X2016 %>% mutate(anio = 2016)
X2017 <- X2017 %>% mutate(anio = 2017)
X2018 <- X2018 %>% mutate(anio = 2018)
X2019 <- X2019 %>% mutate(anio = 2019)
X2020 <- X2020 %>% mutate(anio = 2020)






# Unir todas las bases en una sola
Datos_Unidos <- bind_rows(X2015, X2016, X2017, X2018, X2019, X2020)

# Ahora creo base de datos solo con las variables que se consideran usar
Base <- Datos_Unidos %>% 
  select(amie, nm_regi, id_prov, financiamiento, quintil, tp_sexo, tp_area, inev, imat, ilyl, icn, ies, anio)

#-------------------------------------------------------------------------------
# Limpieza de Valores (NA) -----------------------------------------------------
#-------------------------------------------------------------------------------

sum(is.na(Base$amie))  # Libre de valores NA 

Base <- Base %>%
  mutate(amie = as.character(amie)) %>%  # Convertir 'amie' a tipo character
  filter(amie != "999999")               # Eliminar filas con amie igual a "999999"

# Regiones (Costa, Sierra, Oriente, etc.)
sum(is.na(Base$nm_regi))  # Libre de valores NA 

# Provincias
sum(is.na(Base$id_prov))  # Libre de valores NA

# Financiamiento
sum(is.na(Base$financiamiento)) 
Base <- Base %>%
  mutate(financiamiento = ifelse(is.na(financiamiento), 4, financiamiento))
Base <- Base %>%
  mutate(financiamiento = labelled(financiamiento, 
                                   labels = c("Público (Fiscal y Municipal)" = 1, "Privado (Particular)" = 2, "Mixto (Fiscomisional)" = 3,  "Desconocido" = 4)))

# Sexo
sum(is.na(Base$tp_sexo)) 

Base <- Base %>%
  mutate(tp_sexo = ifelse(tp_sexo == 999999, NA, tp_sexo))

# Tipo de Área
sum(is.na(Base$tp_area)) 


# Quintil 
sum(is.na(Base$quintil)) 
Base <- Base %>%
  mutate(quintil = ifelse(quintil == 999999, NA, quintil)) %>%
  filter(!is.na(quintil))

# Tipo de Área
sum(is.na(Base$tp_area)) 


# Eliminación de NA y 999999 en las variables clave

Base <- Base %>%
  mutate(
    across(c(inev, imat, ilyl, icn, ies), ~ifelse(is.na(.) | . == 999999, NA, .))
  ) %>%
  filter(if_all(c(inev, imat, ilyl, icn, ies), ~!is.na(.)))

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


####################################################################################
####################################################################################
####################################################################################

# Analisis antes del Modelo con las tendencias

# Calcular promedio por grupo y año
calificaciones_promedio <- Base_Final %>%
  group_by(intervencion, anio) %>%
  summarise(promedio_calificaciones = mean(inev, na.rm = TRUE)) %>%
  ungroup()

# Gráfica de líneas
ggplot(calificaciones_promedio, aes(x = anio, y = promedio_calificaciones, color = factor(intervencion))) +
  geom_line(linewidth = 1) +  # Cambiar size por linewidth
  geom_point(size = 2) +
  labs(title = "Tendencias de calificaciones promedio",
       x = "Año",
       y = "Promedio de calificaciones",
       color = "Intervención") +
  theme_minimal()


#######################################################################################3
#######################################################################################3
#######################################################################################3

# Filtrar datos relevantes para el modelo de diferencias en diferencias ----------------

Base_Modelo <- Base_Final %>%
  filter(anio >= 2015 & anio <= 2019) %>%  # Filtrar datos del rango de años de interés
  mutate(grupo_tratado = ifelse(intervencion == 1, 1, 0),  # Definir grupo tratado independientemente del año
         post_tratamiento = ifelse(anio >= 2018, 1, 0))  # Identificar periodo post-tratamiento

###################################################################

# Modelo de diferencias en diferencias con efectos fijos -------------------------

modelo <- feols( inev ~ grupo_tratado + financiamiento   +quintil|  anio + id_prov,  data = Base_Modelo, cluster = ~ id_prov + anio)
msummary(modelo, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),gof_omit = 'DF|R2|Log.Lik') # omite metricas innecesarias 






#########################################
# Filtrar datos antes del tratamiento
Base_PreTratamiento <- Base_Modelo %>% filter(anio < 2018)
# Ajustar el modelo de regresión
modelo_tendencias <- lm(inev ~ grupo_tratado * anio, data = Base_PreTratamiento)
# Resumen del modelo
summary(modelo_tendencias)
# Interpretación gráfica
library(ggplot2)
ggplot(Base_PreTratamiento, aes(x = anio, y = inev, color = factor(grupo_tratado))) +
  geom_line(stat = "summary", fun = mean) +
  labs(color = "Grupo", x = "Año", y = "INEV promedio",
       title = "Tendencias antes del tratamiento") +
  theme_minimal()

#########################################
#########################################


# 1. Filtrar los datos previos al tratamiento real
Base_Placebo <- Base_Modelo %>%
  filter(anio < 2018)
# 2. Crear la variable de tratamiento falso
Base_Placebo <- Base_Placebo %>%
  mutate(
    FakeTreat = ifelse(anio >= 2016 & grupo_tratado == 1, 1, 0)) # Tratamiento falso desde 2016
# Verificar la distribución de FakeTreat
table(Base_Placebo$anio, Base_Placebo$FakeTreat)
# 3. Ajustar el modelo DID con efectos fijos
placebo_model <- feols( inev ~ FakeTreat + quintil + financiamiento| id_prov + anio, data = Base_Placebo, cluster = ~ anio + id_prov)

# 4. Resumen del modelo
msummary(placebo_model, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01))
















