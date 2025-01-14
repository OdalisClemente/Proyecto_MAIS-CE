################################################################################ 
#---- Modelo de Diferencias- Diferencias con efectos fijos bidireccion ---------
################################################################################ 

# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)
library(haven)
library(labelled)
library(lfe)  
library(modelsummary)
library(fixest)

#-------------------------------------------------------------------------------
# Base de Datos de las Escuelas que formaron parte del proyecto ----------------
#-------------------------------------------------------------------------------

Mais <- read_excel("Data/MINEDUC/IE_Adscritas_Nacional.xlsx") %>% 
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
  "98" = "Exterior")

# Asignar las etiquetas
Mais$cod_provi <- set_value_labels(Mais$cod_provi, prov_labels)

# Eliminamos los valores NA y los transformamos: numéricos a cero y texto a "Desconocido"
Mais <- Mais %>% 
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)),    # Reemplaza NA por 0 en columnas numéricas
         across(where(is.character), ~ replace_na(.x, "Desconocido")))  # Reemplaza NA en columnas de texto por "Desconocido"
lapply(Mais, unique)   

#-------------------------------------------------------------------------------
# Bases de Datos del Ineval ----------------------------------------------------
#-------------------------------------------------------------------------------

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
  labels = c("Público (Fiscal y Municipal)" = 1, 
     "Privado (Particular)" = 2, "Mixto (Fiscomisional)" = 3, 
                                        "Desconocido" = 4)))
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
  mutate(across(c(inev, imat, ilyl, icn, ies), ~ifelse(is.na(.) | . == 999999, NA, .)) ) %>%
  filter(if_all(c(inev, imat, ilyl, icn, ies), ~!is.na(.)))
  
# Verificación General de las variables 
lapply(Base, unique) 
any(is.na(Base))

#-------------------------------------------------------------------------------
# JOINS ------------------------------------------------------------------------
#-------------------------------------------------------------------------------

####### Union de la Base y Mais 

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

#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis
Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, financiamiento, nm_regi, tp_area ) |> 
  summarise(num_estud = n(),               # Número total de estudiantes por escuela
            promedio_calif = mean(imat + ilyl + icn + ies, na.rm = T), # Promedio de calificaciones por escuela
            quintil = mean(quintil, na.rm = T))  # Promedio del nivel socioeconómico (quintiles)

# Creación de la variable dependiente en log
Base_Escuelas <- Base_Escuelas %>%
  mutate(log_promedio_calif = log(promedio_calif))

# Filtrar datos ----------------
Base_Modelo <- Base_Escuelas %>%
  mutate(tratada = ifelse(intervencion == 1 & anio >= 2018, 1, 0))


#-------------------------------------------------------------------------------
# Modelo de Diferencias en Diferencias con Efectos Fijos  ----------------------
#-------------------------------------------------------------------------------

# Modelo 1: Efectos fijos por 'amie' y 'anio'
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie, data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_1)

# Modelo 2: Agregando variables 'num_estud' y 'quintil'
modelo_2 <- feols(log_promedio_calif ~ tratada + num_estud + quintil | amie + anio, cluster = ~ amie, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)

#-------------------------------------------------------------------------------
# Análisis Exploratorio: Tendencias Paralelas y Pruebas  ------------------------
#-------------------------------------------------------------------------------

# Calcular promedio por grupo (intervención) y año en las escuelas
calificaciones_promedio <- Base_Escuelas %>%
  group_by(intervencion, anio) %>%  # Agrupar por intervención y año
  summarise(promedio_calificaciones = mean(log_promedio_calif, na.rm = TRUE),  # Promedio de calificaciones
            .groups = "drop")  # Liberar agrupamiento

# Gráfica de tendencias paralelas
ggplot(calificaciones_promedio, aes(x = anio, y = promedio_calificaciones, color = factor(intervencion))) +
  geom_line(linewidth = 1) +  # Línea más gruesa
  geom_point(size = 2) +  # Puntos visibles en cada año
  labs(title = "Tendencias de calificaciones promedio por grupo en las escuelas intervenidas y no intervenidas",
       x = "Año",
       y = "Promedio de calificaciones",
       color = "Intervención (0 = No, 1 = Sí)") +  # Explicación de los grupos
  scale_color_manual(values = c("0" = "red", "1" = "green")) +  # Colores específicos
  theme_minimal()  # Estilo de gráfica

#-------------------------------------------------------------------------------
# Pruebas Estadísticas: Tendencias Paralelas y Placebo -------------------------
#-------------------------------------------------------------------------------

######## Tendencias 

# Filtrar solo los datos de los años previos al tratamiento (2015-2017)
Base_Tendencias <- Base_Modelo %>%
  filter(anio < 2018)

# Crear una nueva variable que identifique si la escuela será tratada después de 2018
Base_Tendencias <- Base_Tendencias %>%
  mutate(eventualmente_tratada = ifelse(intervencion == 1, 1, 0))

# Ajustar el modelo con la nueva variable para prueba de tendencias paralelas
modelo_tendencias <- lm(log_promedio_calif ~ anio + (anio * eventualmente_tratada) + num_estud + quintil, data = Base_Tendencias)
summary(modelo_tendencias)

######## Placebo

# Filtrar datos previos al tratamiento real (2015-2017)
Base_Placebo <- Base_Modelo %>%
  filter(anio < 2018)

# Crear la variable de tratamiento falso (ejemplo: falso tratamiento en 2016)
Base_Placebo <- Base_Placebo %>%
  mutate(tratamiento_falso = ifelse(intervencion == 1 & anio >= 2016, 1, 0))

# Ajustar el modelo DID con el tratamiento falso
modelo_placebo <- feols(log_promedio_calif ~ tratamiento_falso | amie + anio, cluster = ~ amie, data = Base_Placebo)
msummary(modelo_placebo, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_placebo)

modelo_placebo <- feols(log_promedio_calif ~ tratamiento_falso + num_estud + quintil | amie + anio, cluster = ~ amie, data = Base_Placebo)
msummary(modelo_placebo, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_placebo)

#-------------------------------------------------------------------------------
# Efectos a Largo Plazo: Tratamiento Dinámico ---------------------------------
#-------------------------------------------------------------------------------

# Ajustar la variable de tiempo relativa al tratamiento
Base_Largo_Plazo <- Base_Modelo %>%
  mutate(tiempo_relativo = anio - 2018) # Cambia 2018 por el año del tratamiento

# Crear el modelo con interacción dinámica
modelo_dinamico <- feols(log_promedio_calif ~ i(tiempo_relativo, tratada, ref = -1) + num_estud + quintil 
                         | amie + anio, cluster = ~ amie, data = Base_Largo_Plazo)
msummary(modelo_dinamico, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_dinamico)
coefplot(modelo_dinamico)

# Resumen del modelo: Extraer coeficientes para el gráfico
coeficientes <- broom::tidy(modelo_dinamico) %>%
  filter(str_detect(term, "tiempo_relativo")) %>%
  mutate(periodo = as.numeric(str_extract(term, "-?\\d+"))) # Extrae los periodos

# Gráfico de coeficientes
ggplot(coeficientes, aes(x = periodo, y = estimate)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                    ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Efectos dinámicos del tratamiento", 
       x = "Período relativo al tratamiento", 
       y = "Estimación de diferencias en diferencias") +
  theme_minimal()



