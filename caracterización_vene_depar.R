
# Definir las variables de interés para el análisis de migrantes
migrant_variables <- c(
  # Variables identificadoras
  "DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR", "FEX_C18", "DPTO", "MES", "PERIODO", "PER", "AREA", "REGIS",
  # Variables de Migración
  "P3373S3", "P3374S1", "P3374S2", "P3374S3", "P3373S3A1",
  # Variables Demográficas
  "P6040", "P3271", "P6070",
  # Variables Educativas
  "P3042", "P6170",
  # Variables Laborales
  "OCI", "DSI", "INGLABO", "P6430", "P6800", "PT", "FT", "PET",
  # Variables de Vivienda y Hogar
  "P4000", "P4030S1", "P4030S2", "P4030S3", "P4030S5", "P6008", "P70", "P5090",
  # Variables de Salud
  "P6090", "P6100",
  # Variables de Motivos de Migración
  "P3386"
)

###################### DATA.TABLE, FILTRADA PARA LOS VENEZOLANOS #############


# Filtrar los datos de migrantes que cumplan ciertas condiciones
Venezuelan_Migrants <- data[P3373S3 == 862 & P3374S1 == 862, migrant_variables, with = FALSE]



########################################################################

#CARACTERIZACIÓN A NIVEL NACIONAL 

########################################################################

# 1. ############################ DEMOGRAFIA ######################

### 1.1 Composición por edad y sexo de la población venezolana

# 1.1.1 Asignación de grupos de edad para la pirámide poblacional (Piramide_poblacional)

Venezuelan_Migrants[, age_group := cut(
  P6040,
  breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
  labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
  right = FALSE
)]

# Suponiendo que 'Venezuelan_Migrants' ya es un data.table
Pyramid_Venezuelan <- Venezuelan_Migrants[,  .(personas = sum(FEX_C18)/12), by = .(age_group, P3271)]

# Convertir la tabla a formato ancho
Pyramid_wide_Venezuelan <- dcast(Pyramid_Venezuelan, age_group ~ P3271, value.var = "personas")

# Calcular el total de personas y porcentajes
total_poblacion_venezolana <- sum(abs(Pyramid_wide_Venezuelan$Hombres)) + sum(Pyramid_wide_Venezuelan$Mujeres)
Pyramid_wide_Venezuelan$Hombres_pct <- (abs(Pyramid_wide_Venezuelan$Hombres) / total_poblacion_venezolana) * 100
Pyramid_wide_Venezuelan$Mujeres_pct <- (Pyramid_wide_Venezuelan$Mujeres / total_poblacion_venezolana) * 100
Pyramid_wide_Venezuelan$Mujeres_pct <- -Pyramid_wide_Venezuelan$Mujeres_pct

# 1.1.2 Población por sexo (Gráfico de torta)

sex_Venezuelan <- Venezuelan_Migrants[, .(personas = sum(FEX_C18)/12), by = P3271]
sex_Venezuelan[, sum(personas)]

# Calcular el porcentaje
sex_Venezuelan[, Percentage := (personas / sum(personas)) * 100]

### 1.2 Estado civil

marital_status_Venezuelan <- Venezuelan_Migrants[!is.na(P6070), .(personas = sum(FEX_C18, na.rm = TRUE) / 12), by = P6070]


# 2. ############################ EDUACIÓN #######################

### 2.1. Nivel de educación alcanzado

education_achieved_Venezuelan <- Venezuelan_Migrants[!is.na(P3042), .(personas = sum(FEX_C18, na.rm = TRUE)/12), by = P3042]

### 2.2 Ingreso por nivel de escolaridad

inglabo_Venezuelan <- Venezuelan_Migrants[!is.na(P3042), .(personas = mean(INGLABO, na.rm =TRUE)), by = P3042]


# 3. ############################ MERCADO LABORAL #######################  

### 3.1 Función para agrupar variables laborales y calcular estadísticas relacionadas

group_variables_venezuelan <- function() {
  factor_expansion_venezuelan <- Venezuelan_Migrants[, .(factor_expansion = sum(FEX_C18) / 12), by = P3271]
  ocupados_venezuelan <- Venezuelan_Migrants[, .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 12), by = P3271]
  desocupados_venezuelan <- Venezuelan_Migrants[, .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 12), by = P3271]
  poblacion_edad_trabajar_venezuelan <- Venezuelan_Migrants[P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 12), by = P3271]
  
  combined_venezuelan <- data.table(
    ocupados = ocupados_venezuelan$ocupados,
    desocupados = desocupados_venezuelan$desocupados,
    poblacion_edad_trabajar = poblacion_edad_trabajar_venezuelan$poblacion_edad_trabajar,
    factor_expansion = factor_expansion_venezuelan$factor_expansion
  )
  
  combined_venezuelan[, fuerza_trabajo := (ocupados + desocupados)]
  combined_venezuelan[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100]
  combined_venezuelan[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100]
  combined_venezuelan[, sexo := c("Hombres", "Mujeres")]
  
  return(combined_venezuelan)
}

# Llamada a la función y almacenamiento de los resultados
table_variables_to_graph_venezuelan <- group_variables_venezuelan()


### 3.2 Tipo de trabajo

t_job_venezuelan <- Venezuelan_Migrants[!is.na(P6430), .(personas = sum(FEX_C18, na.rm = TRUE )/12), by = P6430]


# 4. ############################ VIVIENDA ####################### 

### 4.1 Tipo de vivienda

t_house_venezuelan <- Venezuelan_Migrants[!is.na(P5090), .(personas = sum(FEX_C18, na.rm = TRUE )/12), by =  P5090]

### 4.2 Condiciones del hogar

home_conditions_venezuelan <- function() {
  electrical_energy_venezuelan <- Venezuelan_Migrants[, .(energia_electrica = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S1]
  natural_gas_venezuelan <- Venezuelan_Migrants[, .(gas_natural = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S2]
  sewer_venezuelan <- Venezuelan_Migrants[, .(alcantarillado = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S3]
  aqueduct_venezuelan <- Venezuelan_Migrants[, .(acueducto = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S5]
  
  combined_venezuelan <- data.table(
    energia_electrica = electrical_energy_venezuelan$energia_electrica,
    gas_natural = natural_gas_venezuelan$gas_natural,
    alcantarillado = sewer_venezuelan$alcantarillado,
    acueducto = aqueduct_venezuelan$acueducto
  )
  
  total_population_venezuelan <- 2229007
  combined_venezuelan[, porcentaje_energia := (energia_electrica / total_population_venezuelan) * 100]
  combined_venezuelan[, porcentaje_gas := (gas_natural / total_population_venezuelan) * 100]
  combined_venezuelan[, porcentaje_alcantarillado := (alcantarillado / total_population_venezuelan) * 100]
  combined_venezuelan[, porcentaje_acuaducto := (acueducto / total_population_venezuelan) * 100]
  combined_venezuelan[, acceso := c("Si", "No")]
  
  return(combined_venezuelan)
}

# Llamada a las funciones y almacenamiento de los resultados
table_home_conditions_venezuelan <- home_conditions_venezuelan()


# 5. ############################ SALUD #######################

### 5.1 Acceso a salud 

health_coverage_venezuelan <- Venezuelan_Migrants[, .(personas = sum(FEX_C18)/12), by = P6090]

### 5.2 Tipo de Afiliación al sistema de salud

t_health_venezuelan <- Venezuelan_Migrants[!is.na(P6100), .(personas = sum(FEX_C18, na.rm = TRUE)/12), by = P6100]



# 6. ############################ MOTIVO DE LA MIGRACIÓN #######################   

### 6.1 Variables de Motivos de Migración

reasons_migration <- Venezuelan_Migrants[!is.na(P3386), .(personas = sum(FEX_C18, na.rm = TRUE)/12), by = P3386]
