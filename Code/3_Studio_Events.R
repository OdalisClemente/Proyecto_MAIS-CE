

###  Análisis de Impacto y Validación de Modelos -----------------------------


# En este apartado se explorarán las tendencias paralelas, los efectos placebo y 
# los efectos a largo plazo, con el fin de validar y analizar el impacto de las intervenciones.


#-------------------------------------------------------------------------------
# Análisis Exploratorio: Tendencias Paralelas y Placebo   ------------------------
#-------------------------------------------------------------------------------

######## Tendencias Paralelas

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


######## Placebo

# Filtrar datos previos al tratamiento real (2015-2017)
Base_Placebo <- Base_Modelo %>%
  filter(anio < 2018)

# Crear la variable de tratamiento falso (ejemplo: falso tratamiento en 2016)
Base_Placebo <- Base_Placebo %>%
  mutate(tratamiento_falso = ifelse(intervencion == 1 & anio >= 2016, 1, 0))

# Ajustar el modelo DID con el tratamiento falso
modelo_placebo <- feols(log_promedio_calif ~ tratamiento_falso | amie + anio, cluster = ~ amie, data = Base_Placebo)
msummary(modelo_placebo, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') 
summary(modelo_placebo)


# Ajustar el modelo DID con el tratamiento falso con todas las variables 
modelo_placebo <- feols(log_promedio_calif ~ tratamiento_falso + num_estud  + prop_quintil_bajo 
                        + prop_quintil_medio + prop_quintil_alto + prop_mujeres 
                        + prop_deshonestidad + promedio_na_eano + prop_discapacidad | amie + anio, cluster = ~ amie, data = Base_Placebo)
msummary(modelo_placebo, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik') 
summary(modelo_placebo)


#-------------------------------------------------------------------------------
# Efectos a Largo Plazo: Tratamiento Dinámico ---------------------------------
#-------------------------------------------------------------------------------

# Crear variable de tiempo centrada
Base_Modelo <- Base_Escuelas %>%
  mutate(tratada = ifelse(intervencion == 1 & anio >= 2018, 1, 0),
         tiempo_centrado = anio - 2018)  # Ajusta 2018 según sea necesario

# Variables indicadoras para cada período de tiempo relativo
Base_Modelo <- Base_Modelo %>%
  mutate(across(tiempo_centrado, ~ factor(.)))

# Modelo de efectos de tratamiento dinámico con efectos fijos
modelo_dinamico <- feols(log_promedio_calif ~ i(tiempo_centrado, tratada, ref = -1) | amie + anio, 
                         cluster = ~ amie + anio, data = Base_Modelo)
msummary(modelo_dinamico, stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01), gof_omit = 'DF|R2|Log.Lik')
summary(modelo_dinamico)
coefplot(modelo_dinamico, main = "Efectos Dinámicos")
