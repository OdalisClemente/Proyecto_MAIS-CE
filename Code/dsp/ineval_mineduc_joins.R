# ---------------------------------------------
# Joins con la base del MAIS-CE y datos INEVAL de Ser Bachiller
# Diciembre 2024
# ----------------------------------------------

# Preliminares -------------------------------------------------------------

# Cargar paquetes

library(dplyr)
library(haven)
library(readxl)

# Cargar datos

mais_raw <- 
    read_xlsx("Data/MINEDUC/IE_Adscritas_Nacional.xlsx") |> 
    janitor::clean_names()

ineval_15_raw <- read_sav("Data/INEVAL/SBAC15_micro_246169_20200130_SAV.sav")

# Exploratorio ------------------------------------------------------------

mais_raw |> 
    group_by(periodo) |> 
    summarise(n = n())