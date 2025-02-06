
# En este apartado, se analizarán los efectos del programa de intervención sobre
# el rendimiento académico de los estudiantes, desglosado por materias específicas. 
# Este análisis buscará determinar cómo la intervención ha influido en las calificaciones 
# en distintas asignaturas, ajustando por las características invariables de cada unidad educativa.


#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis

# Materia Matematica

Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, tp_area, financiamiento, id_prov ) |> 
  summarise(
    num_estud = n(),                                           # Número total de estudiantes por escuela
    promedio_calif = mean(imat, na.rm = T), # Promedio de calificaciones por escuela
    prop_quintil_bajo = median(quintil %in% c(1, 2), na.rm = T), # Bajo: Quintil 1 y 2
    prop_quintil_medio = median(quintil == 3, na.rm = T),        # Medio: Quintil 3
    prop_quintil_alto = median(quintil %in% c(4, 5), na.rm = T), # Alto: Quintil 4 y 5
    prop_mujeres = mean(tp_sexo == 1, na.rm = T),
    prop_deshonestidad = mean(deshonestidad == 1, na.rm = T),
    prop_discapacidad = mean(discapacidad == 1, na.rm = T),
    promedio_na_eano = mean(na_eano, na.rm = T))

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
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie + anio , data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_1)


# Modelo 2: Agregando Todas las variables 
modelo_2 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo 
                  + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)


# Modelo 3: Agregando todas dejando solo un quintil 
modelo_3 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_3, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_3)



#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis

# Materia Lengua y Literatura

Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, tp_area, financiamiento, id_prov ) |> 
  summarise(
    num_estud = n(),                                           # Número total de estudiantes por escuela
    promedio_calif = mean(ilyl, na.rm = T), # Promedio de calificaciones por escuela
    prop_quintil_bajo = median(quintil %in% c(1, 2), na.rm = T), # Bajo: Quintil 1 y 2
    prop_quintil_medio = median(quintil == 3, na.rm = T),        # Medio: Quintil 3
    prop_quintil_alto = median(quintil %in% c(4, 5), na.rm = T), # Alto: Quintil 4 y 5
    prop_mujeres = mean(tp_sexo == 1, na.rm = T),
    prop_deshonestidad = mean(deshonestidad == 1, na.rm = T),
    prop_discapacidad = mean(discapacidad == 1, na.rm = T),
    promedio_na_eano = mean(na_eano, na.rm = T))

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
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie + anio , data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_1)


# Modelo 2: Agregando Todas las variables 
modelo_2 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo 
                  + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)

# Modelo 3: Agregando todas dejando solo un quintil 
modelo_3 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_3, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_3)




#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis

# Materia Ciencias Naturales

Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, tp_area, financiamiento, id_prov ) |> 
  summarise(
    num_estud = n(),                                           # Número total de estudiantes por escuela
    promedio_calif = mean(icn, na.rm = T), # Promedio de calificaciones por escuela
    prop_quintil_bajo = median(quintil %in% c(1, 2), na.rm = T), # Bajo: Quintil 1 y 2
    prop_quintil_medio = median(quintil == 3, na.rm = T),        # Medio: Quintil 3
    prop_quintil_alto = median(quintil %in% c(4, 5), na.rm = T), # Alto: Quintil 4 y 5
    prop_mujeres = mean(tp_sexo == 1, na.rm = T),
    prop_deshonestidad = mean(deshonestidad == 1, na.rm = T),
    prop_discapacidad = mean(discapacidad == 1, na.rm = T),
    promedio_na_eano = mean(na_eano, na.rm = T))

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
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie + anio , data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_1)


# Modelo 2: Agregando Todas las variables 
modelo_2 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo 
                  + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)


# Modelo 3: Agregando todas dejando solo un quintil 
modelo_3 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_3, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_3)




#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis

# Materia Estudios Sociales 

Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, tp_area, financiamiento, id_prov ) |> 
  summarise(
    num_estud = n(),                                           # Número total de estudiantes por escuela
    promedio_calif = mean(ies, na.rm = T), # Promedio de calificaciones por escuela
    prop_quintil_bajo = median(quintil %in% c(1, 2), na.rm = T), # Bajo: Quintil 1 y 2
    prop_quintil_medio = median(quintil == 3, na.rm = T),        # Medio: Quintil 3
    prop_quintil_alto = median(quintil %in% c(4, 5), na.rm = T), # Alto: Quintil 4 y 5
    prop_mujeres = mean(tp_sexo == 1, na.rm = T),
    prop_deshonestidad = mean(deshonestidad == 1, na.rm = T),
    prop_discapacidad = mean(discapacidad == 1, na.rm = T),
    promedio_na_eano = mean(na_eano, na.rm = T))

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
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie + anio , data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_1)


# Modelo 2: Agregando Todas las variables 
modelo_2 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo 
                  + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)


# Modelo 3: Agregando todas dejando solo un quintil  
modelo_3 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_3, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_3)






# Las variables que salieron significativas de manera individualestan en conjunto para ver como trabajan 

#-------------------------------------------------------------------------------
# Preparación de datos por Escuelas  -------------------------------------------
#-------------------------------------------------------------------------------

# Crear una nueva base de datos con escuelas como unidad de análisis

# Materia Matematica y Estudios Sociales

Base_Escuelas <- Base_Final |> 
  group_by(amie, anio, intervencion, tp_area, financiamiento, id_prov ) |> 
  summarise(
    num_estud = n(),                                           # Número total de estudiantes por escuela
    promedio_calif = mean(imat + ies, na.rm = T), # Promedio de calificaciones por escuela
    prop_quintil_bajo = median(quintil %in% c(1, 2), na.rm = T), # Bajo: Quintil 1 y 2
    prop_quintil_medio = median(quintil == 3, na.rm = T),        # Medio: Quintil 3
    prop_quintil_alto = median(quintil %in% c(4, 5), na.rm = T), # Alto: Quintil 4 y 5
    prop_mujeres = mean(tp_sexo == 1, na.rm = T),
    prop_deshonestidad = mean(deshonestidad == 1, na.rm = T),
    prop_discapacidad = mean(discapacidad == 1, na.rm = T),
    promedio_na_eano = mean(na_eano, na.rm = T))

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
modelo_1 <- feols(log_promedio_calif ~ tratada | amie + anio, cluster = ~ amie + anio , data = Base_Modelo)
msummary(modelo_1, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_1)


# Modelo 2: Agregando Todas las variables 
modelo_2 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo 
                  + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_2, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_2)


# Modelo 3: Agregando todas dejando solo un quintil  
modelo_3 <- feols(log_promedio_calif ~ tratada  + num_estud  + prop_quintil_bajo + prop_mujeres 
                  + prop_deshonestidad + promedio_na_eano + prop_discapacidad
                  |amie + anio, cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_3, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') # omite métricas innecesarias
summary(modelo_3)




