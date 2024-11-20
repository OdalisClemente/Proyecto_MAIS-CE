# Cargar las librerías necesarias
library(readxl) 
library(dplyr)
library(ggplot2)
library(ggthemes) 
library(tidyverse)
library(janitor)
library(haven)


# Datos de las Escuelas ---------------------------------------------------

Mais <- read_excel("Data/MINEDUC/IE_Adscritas_Nacional.xlsx")

# Datos del ineval --------------------------------------------------------

X2015 <- read_sav("Data/MINEDUC/2015.sav")
X2016 <- read_sav("Data/MINEDUC/2016.sav")
X2017 <- read_sav("Data/MINEDUC/2017.sav")
X2018 <- read_sav("Data/MINEDUC/2018.sav")
X2019 <- read_sav("Data/MINEDUC/2019.sav")
X2020 <- read_sav("Data/MINEDUC/2020.sav")

