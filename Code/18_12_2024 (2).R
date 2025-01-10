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

#X2020 <- read_sav("Data/MINEDUC/2020.sav")



# Añadir una columna de año para identificar cada conjunto de datos
X2015 <- X2015 %>% mutate(anio = 2015)
X2016 <- X2016 %>% mutate(anio = 2016)
X2017 <- X2017 %>% mutate(anio = 2017)
X2018 <- X2018 %>% mutate(anio = 2018)
X2019 <- X2019 %>% mutate(anio = 2019)

# Unir todas las bases en una sola
Datos_Unidos <- bind_rows(X2015, X2016, X2017, X2018, X2019)

# Ahora creo base de datos solo con las variables que se consideran usar
Base <- Datos_Unidos %>% 
  select(amie, nm_regi, id_prov, financiamiento, tp_sexo, tp_area, inev, imat, ilyl, icn, ies, anio)

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

any(is.na(Base_Final)) # Ver si posee la base valores NA

######################3 Hasta aqui todo perfecto. 


# Analisis de los Datos.....


# Seccion pre modelo ------------------------------------------------------

# Calcular el número de estudiantes por escuela
Nu_estudiantes_por_amie <- Base_Final %>%
  group_by(amie) %>%  # Agrupar por escuela identificada por 'amie'
  summarise(num_estudiantes = n()) %>%  # Contar número de estudiantes en cada escuela
  arrange(desc(num_estudiantes))  # Ordenar en orden descendente por número de estudiantes

# Número de estudiantes por año
estudiantes_por_anio <- Base_Final %>%
  group_by(anio) %>%  # Agrupar por columna 'anio'
  summarise(num_estudiantes = n()) %>%  # Contar estudiantes por año
  arrange(anio)  # Ordenar por año

# Número de estudiantes por región
estudiantes_region <- Base_Final %>%
  group_by(nm_regi) %>%  # Agrupar por columna 'nm_regi' (nombre de región)
  summarise(num_estudiantes = n()) %>%  # Contar estudiantes por región
  arrange(desc(num_estudiantes))  # Ordenar por número de estudiantes
estudiantes_region

# Resultados similares pero observando sin son tratados y no trata --------

# Número de estudiantes en escuelas tratadas y no tratadas por año atraves de sus calificaciones
estudiantes_por_anio_intervencion <- Base_Final %>%
  group_by(anio, intervencion) %>%  # Agrupar por año y tratamiento
  summarise(num_estudiantes = n()) %>%  # Contar estudiantes
  arrange(anio, intervencion)

# Gráfica del número de estudiantes en tratadas vs no tratadas por año
ggplot(estudiantes_por_anio_intervencion, aes(x = as.factor(anio), y = num_estudiantes, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("purple", "darkorange"), labels = c("No Tratadas", "Tratadas")) +
  labs( title = "Número de Estudiantes en Escuelas Tratadas vs No Tratadas por Año",
    x = "Año",
    y = "Número de Estudiantes",
    fill = "Escuelas" ) + theme_minimal()


################################################

# Número de estudiantes en escuelas tratadas y no tratadas por región
estudiantes_por_region_intervencion <- Base_Final %>%
  group_by(nm_regi, intervencion) %>%  # Agrupar por región y tratamiento
  summarise(num_estudiantes = n()) %>%  # Contar estudiantes
  arrange(desc(num_estudiantes))

# Gráfica del número de estudiantes en tratadas vs no tratadas por región
ggplot(estudiantes_por_region_intervencion, aes(x = reorder(nm_regi, -num_estudiantes), y = num_estudiantes, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("No Tratadas", "Tratadas")) +
  labs( title = "Número de Estudiantes en Escuelas Tratadas vs No Tratadas por Región",
    x = "Región",
    y = "Número de Estudiantes",
    fill = "Escuelas" ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# SECCION II


# Número de estudiantes por año, región y tratamiento
estudiantes_por_anio_region_intervencion <- Base_Final %>%
  group_by(anio, nm_regi, intervencion) %>%  # Agrupar por año, región y tratamiento
  summarise(num_estudiantes = n()) %>%
  arrange(anio, nm_regi, desc(num_estudiantes))

# Ver los resultados
print(estudiantes_por_anio_region_intervencion)

# Gráfica del número de estudiantes por año, región y tratamiento
ggplot(estudiantes_por_anio_region_intervencion, aes(x = as.factor(anio), y = num_estudiantes, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ nm_regi, scales = "free") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("No Tratadas", "Tratadas")) +
  labs(
    title = "Número de Estudiantes en Escuelas Tratadas vs No Tratadas por Año y Región",
    x = "Año",
    y = "Número de Estudiantes",
    fill = "Escuelas"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))









######################## MODELO########################
# SECCION III


# Tendencias previas ------------------------------------------------------------

# Calcular promedios de 'inev' por grupo y año
promedios <- Base_Final %>%
  group_by(anio, intervencion) %>%
  summarise(inev_promedio = mean(inev, na.rm = TRUE)) %>%
  mutate(grupo = ifelse(intervencion == 1, "Tratamiento", "Control"))

# Crear el gráfico de tendencias
ggplot(promedios, aes(x = anio, y = inev_promedio, color = grupo, group = grupo)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Tendencias Previas al Tratamiento",
    x = "Año",
    y = "Promedio de inev",
    color = "Grupo" ) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black") + # Cambiar por el año de la intervención
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom")


# Placebo -----------------------------------------------------------------
# Crear una variable de intervención de placebo (supongamos que la intervención ocurrió en 2017)
Base_Final <- Base_Final %>%
  mutate(intervencion_placebo = ifelse(anio >= 2016, 1, 0))  # Intervención "placebo" a partir de 2017

# Modelo de diferencia en diferencias con la intervención placebo
modelo_placebo <- felm(
  inev ~ intervencion_placebo |
    amie + anio,  # Efectos fijos por escuela y año
  data = Base_Final)

# Resumen del modelo
summary(modelo_placebo)

# Si el coeficiente de la variable 'intervencion_placebo' es no significativo, 
# entonces la prueba de placebo confirma la validez del diseño DID.


################ MODELO 

modelo_did <- felm(inev ~ intervencion * as.factor(anio) | amie + anio, data = Base_Final)
summary(modelo_did)
modelo_did




###################

# Modelo DID -------------------------------------------------------------
# Asegurarse de que la base tenga las variables necesarias
# Variable dependiente (ejemplo: "inev", que puede ser una calificación promedio)
# Variable de tratamiento: 'intervencion' (1: Tratadas, 0: No Tratadas)
# Variable de tiempo: 'anio' (antes y después de la intervención)

# Crear una variable de tiempo ficticia (pre y post intervención)
Base_Final <- Base_Final %>%
  mutate(post_intervencion = ifelse(anio >= 2018, 1, 0))  # 1 si es 2018 o después

# Interacción entre tratamiento e intervención (DID)
Base_Final <- Base_Final %>%
  mutate(did = intervencion * post_intervencion)

# Modelo DID: regresión lineal
modelo_did <- lm(inev ~ intervencion + post_intervencion + did, data = Base_Final)

# Resumen del modelo DID
summary(modelo_did)

# Interpretación básica de los coeficientes:
# - Coeficiente de `did`: mide el efecto causal de la intervención.
# - Significativo y positivo: la intervención tuvo un impacto positivo en la variable dependiente.
# - Significativo y negativo: la intervención tuvo un impacto negativo en la variable dependiente.

# Resultados -------------------------------------------------------------
# Visualización del impacto del tratamiento en las escuelas tratadas y no tratadas
impacto_por_grupo <- Base_Final %>%
  group_by(intervencion, post_intervencion) %>%
  summarise(promedio_calificaciones = mean(inev, na.rm = TRUE)) %>%
  arrange(intervencion, post_intervencion)

# Gráfico del impacto promedio
ggplot(impacto_por_grupo, aes(x = as.factor(post_intervencion), y = promedio_calificaciones, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("steelblue", "darkorange"), labels = c("No Tratadas", "Tratadas")) +
  labs(title = "Impacto de la Intervención en las Calificaciones Promedio",
       x = "Post Intervención (0: Antes, 1: Después)",
       y = "Calificación Promedio",
       fill = "Grupo") +
  theme_minimal()

# Guardar resultados del modelo
stargazer::stargazer(modelo_did, type = "text", title = "Resultados del Modelo DID")





###################################################################################

# Preparación de la base de datos ---------------------------------------
# Crear una variable de tiempo ficticia (pre y post intervención)
Base_Final <- Base_Final %>%
  mutate(post_intervencion = ifelse(anio >= 2018, 1, 0))  # 1 si es 2018 o después

# Variable DID: Interacción entre tratamiento e intervención
Base_Final <- Base_Final %>%
  mutate(did = intervencion * post_intervencion)

# Modelo DID con efectos fijos bidireccionales --------------------------
modelo_did_fx <- feols(
  inev ~ did | amie + anio,  # Efectos fijos por escuela (amie) y año
  cluster = ~ amie,          # Clúster a nivel de escuela
  data = Base_Final
)

# Resumen del modelo
summary(modelo_did_fx, cluster = ~ amie)

# Interpretación ---------------------------------------------------------
# El coeficiente de `did` mide el impacto causal promedio de la intervención.

# Visualización del impacto ----------------------------------------------
# Calcular el promedio de la variable dependiente por grupo
impacto_por_grupo <- Base_Final %>%
  group_by(intervencion, post_intervencion) %>%
  summarise(promedio_inev = mean(inev, na.rm = TRUE)) %>%
  arrange(intervencion, post_intervencion)

# Gráfico del impacto promedio
ggplot(impacto_por_grupo, aes(x = as.factor(post_intervencion), y = promedio_inev, fill = as.factor(intervencion))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), name = "Intervención") +
  labs(
    x = "Periodo (0 = Antes, 1 = Después)",
    y = "Promedio de Calificaciones (inev)",
    title = "Impacto del Tratamiento por Grupo"
  ) +
  theme_minimal()





#######################################3



# Tendencias con la variabel intervencion :

#######################################
# Filtrar datos previos al tratamiento
Base_Tendencias <- Base_Final %>% 
  filter(anio < 2018) # Cambia 2018 al año de inicio del tratamiento

# Calcular promedio por grupo y año
Promedios <- Base_Tendencias %>%
  group_by(anio, intervencion) %>% 
  summarise(promedio_inev = mean(inev, na.rm = TRUE))

# Graficar tendencias previas

ggplot(Promedios, aes(x = anio, y = promedio_inev, color = as.factor(intervencion))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Tendencias Previas por Grupo",
    x = "Año",
    y = "Promedio del Resultado (inev)",
    color = "Grupo"
  ) +
  scale_color_manual(values = c("red", "blue"), labels = c("Control", "Tratado")) +
  theme_minimal()



















