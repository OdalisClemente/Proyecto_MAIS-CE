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
  janitor::clean_names()

Mais %>% glimpse()  # Al reavisar este codigo me sale que tengo todos los valores de las variables en caracteres, pero no es asi, hay 3 variables que son totalmente numeros. 

# Para cambiatr el error en donde los numeros no son nuemros si no caracteres primero se les acomoda la coma, luego se los tranfroma a numerico y eso se los hace con el codigo de "stringr" /convertir antes de hacer cadena en numeros
#library(stringr)
#  revenue_trimmed = str_remove(Mais$cod_provi, ",")  # pero en nuestro caso nuestros valores no poseen comas, asi que se los transforma directamente 

Mais <- Mais %>%
  mutate(
    cod_provi = as.numeric(cod_provi),
    cod_cant = as.numeric(cod_cant),
    cod_parro   = as.numeric(cod_parro)
  )  # Aqui estan  en nuemro, pero aun poseen un error en lso codigos, sin embaego como son las variables que no se usaran se las deja para segunda orden. 


# El tratar de limpiar los datos o sanear el formato de los mismos, salió un error. Pues los códigos se distorsionan y no salen como deberían, lo mismo sucede con 
# una de las variables importantes, pero como esta al transformarse se con vientre en un número del 1-24 se pueden identificar a simple vista al momento de realizar el análisis, 
# pero a las demás variables como no serán tomadas en consideración por el momento se dejarán así. 


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

unique(Mais$zona)  # vero la categoria de la variable

unique(Mais$cod_provi)  # vero la categoria de la variable

###############################################################################
###############################################################################
###############################################################################

# Datos del Ineval --------------------------------------------------------

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
#janitor::clean_names()


# Añadir una columna de año para identificar cada conjunto de datos
X2015 <- X2015 %>% mutate(anio = 2015)
X2016 <- X2016 %>% mutate(anio = 2016)
X2017 <- X2017 %>% mutate(anio = 2017)
X2018 <- X2018 %>% mutate(anio = 2018)
X2019 <- X2019 %>% mutate(anio = 2019)
#X2020 <- X2020 %>% mutate(anio = 2020)


# Unir todas las bases en una sola
Datos_Unidos <- bind_rows(X2015, X2016, X2017, X2018, X2019)


# Ahora creo base de datos solo con las variables que usare para no tener problemas y que esten bien 
Base <- Datos_Unidos %>% 
  select(amie, nm_regi, id_prov, financiamiento, tp_sexo, tp_area, nl_inev, nl_imat, nl_ilyl, nl_icn, nl_ies, anio)


# Verificar valores únicos en todas las columnas
lapply(Base, unique)   # Identifdicamos como se encunetran las variables categoricas en su totalidad, 


# Regiones 
sum(is.na(Base$nm_regi))  #  esta limpia de valores NA, pero se eliminana los 999999 y se los pone en NA 

# Provincias
sum(is.na(Base$id_prov))  # Esta limpia de valores NA


# Financiamiento
sum(is.na(Base$financiamiento)) # Tiene valores NA

Base <- Base %>%
  mutate(financiamiento = ifelse(is.na(financiamiento), 4, financiamiento))

# Asignar etiquetas a las categorías de `financiamiento`
Base <- Base %>%
  mutate(financiamiento = labelled(financiamiento, 
                                   labels = c("Público (Fiscal y Municipal)" = 1, "Privado (Particular)" = 2, "Mixto (Fiscomisional)" = 3,  "Desconocido" = 4)))

Base %>% 
  count(financiamiento) # Con este codigo revisamos que estan etiquedaos y sin valores NA 


# Sexo
sum(is.na(Base$tp_sexo)) # Esta limpia de valores NA

Base <- Base %>%
  mutate(tp_sexo = ifelse(tp_sexo == 999999, NA, tp_sexo))

# Tipo de Area
sum(is.na(Base$tp_area)) # Esta limpia de valores NA 


# Nivel de logro alcanzado por el sustentante en la Nota de Examen de Grado
sum(is.na(Base$nl_inev)) # Esta variable posee 868889 NA por lo tanto, como tambien hay valores 999999 que se llaman No aplica seran realizao en una sola variable que digan NA 

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

# Transforme los NA y los valores erroneos a una sola categoria
Base <- Base %>%
  mutate(
    nl_imat = case_when(
      is.na(nl_imat) ~ 4,           # Reemplaza los NA por 4
      nl_imat == 999999 ~ 4,        # Cambia los "No aplica" por 4
      TRUE ~ nl_imat ))               # Conserva los valores originales para el resto
    
# Aqui agregue las categorias de las variaables 
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
      TRUE ~ nl_ilyl))                  # Conserva los valores originales para el resto
    
  
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


# Verificar valores con las tranfromaciones y eliminacionde los valores NA y los demas erroneos   # FALSE TRUE
lapply(Base, unique)   

# Verificar si existen valores NA en toda la base de datos
any(is.na(Base))

###############################################################################
###############################################################################
###############################################################################

# Joins -------------------------------------------------------------------

# Realizamos el inner join
Mais_Base <- inner_join(Mais, Base, by = "amie")

head(Mais_Base)

###############################################################################
###############################################################################
###############################################################################

# Frecuencia de las provincias 
Mais_Base %>% 
  count(provincia) %>%
  ggplot(aes(x = fct_reorder(provincia, n), y = n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low ="#9632B8", high = "#00E5EE") +  # Degradado de color
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribución de Provincias", x = "Provincia", y = "Frecuencia", fill = "Frecuencia")



# Crear un gráfico de barras que cuente escuelas por provincia
ggplot(Mais_Base, aes(x = provincia)) +
  geom_bar(fill = "#FF00CC", color = "black") +
  labs(title = "Número de escuelas por provincia que participaron el el proyecto",
       x = "Provincia",
       y = "Cantidad de Escuelas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas para mayor claridad


