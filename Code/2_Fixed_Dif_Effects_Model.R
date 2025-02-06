

#-------------------------------------------------------------------------------
# Modelo de Diferencias en Diferencias con Efectos Fijos  ----------------------
#-------------------------------------------------------------------------------

# En este apartado se presentan tres especificaciones del modelo de Diferencias en Diferencias (DID) con efectos fijos:  

# Modelo 1. Se incluye únicamente la variable dependiente (promedio de calificaciones) y la variable de tratamiento.  
# Modelo 2. Se incluye la variable dependiente e incorporan todas las variables disponibles en la base de datos.  
# Modelo 3. Se considera la variable dependiente junto con las demás variables, pero restringiendo los quintiles y solo dejando el quintil más bajo.  



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

