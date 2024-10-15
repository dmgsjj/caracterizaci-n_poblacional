
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

#CARACTERIZACIÓN DE LA POBLACION VENEZOLANA NIVEL NACIONAL 

########################################################################








# Función para filtrar migrantes venezolanos


# 1. DEMOGRAFÍA

# Función para obtener pirámide poblacional por departamento
piramide_poblacional <- function(data) {
  data[, age_group := cut(
    P6040,
    breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
    labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
               "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
    right = FALSE
  )]
  
  # Calcular población por grupo de edad y sexo
  pyramid_dept_m <- data[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, age_group, P3271)]
  pyramid_dept_wide_m <- dcast(pyramid_dept_m, DPTO + age_group ~ P3271, value.var = "personas", fill = 0)
  pyramid_dept_wide_m[, total_poblacion := sum(abs(Hombres)) + sum(Mujeres), by = DPTO]
  pyramid_dept_wide_m[, Hombres_pct := (abs(Hombres) / total_poblacion) * 100]
  pyramid_dept_wide_m[, Mujeres_pct := (Mujeres / total_poblacion) * 100]
  pyramid_dept_wide_m[, Mujeres_pct := -Mujeres_pct]
  
  return(pyramid_dept_wide_m)
}

# Estado civil por departamento
estado_civil <- function(data) {
  marital_status_dept_m <- data[!is.na(P6070), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6070)]
  return(marital_status_dept_m)
}

# Sexo por departamento
sexo_departamento <- function(data) {
  sex_dept_m <- data[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  return(sex_dept_m)
}

# 2. EDUCACIÓN

# Nivel de educación alcanzado por departamento
nivel_educacion <- function(data) {
  education_achieved_dept_m <- data[!is.na(P3042), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3042)]
  return(education_achieved_dept_m)
}

# Nivel de ingreso por nivel educativo
ingreso_por_educacion <- function(data) {
  income_by_education_dept_m <- data[!is.na(P3042) & !is.na(INGLABO), .(ingreso = mean(INGLABO, na.rm = TRUE)), by = .(DPTO, P3042)]
  return(income_by_education_dept_m)
}

# 3. MERCADO LABORAL

# Función para calcular estadísticas laborales
estadisticas_laborales <- function(data) {
  factor_expansion <- data[!is.na(P3271), .(factor_expansion = sum(FEX_C18) / 7), by = .(DPTO, P3271)]
  ocupados <- data[!is.na(P3271), .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 7), by = .( DPTO, P3271)]
  desocupados <- data[!is.na(P3271), .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  poblacion_edad_trabajar <- data[P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
  
  combined <- factor_expansion[ocupados, on = c("DPTO", "P3271")][desocupados, on = c("DPTO", "P3271")][poblacion_edad_trabajar, on = c("DPTO", "P3271")]
  combined[, fuerza_trabajo := (ocupados + desocupados)]
  combined[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100]
  combined[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100]
  
  return(combined)
}

# Tipo de trabajo por departamento
tipo_trabajo <- function(data) {
  t_job_dep_m <- data[!is.na(P6430), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6430)]
  return(t_job_dep_m)
}

# 4. VIVIENDA

# Tipo de vivienda por departamento
tipo_vivienda <- function(data) {
  housing_type_dept_m <- data[!is.na(P5090), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P5090)]
  return(housing_type_dept_m)
}

# Servicios básicos: energía, gas, alcantarillado, acueducto
servicios_basicos <- function(data) {
  energia <- data[, .(energia = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S1)]
  gas <- data[, .(gas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S2)]
  alcantarillado <- data[, .(alcantarillado = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S3)]
  acueducto <- data[, .(acueducto = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S5)]
  
  return(list(energia = energia, gas = gas, alcantarillado = alcantarillado, acueducto = acueducto))
}

# 5. SALUD

# Cobertura de salud por departamento
cobertura_salud <- function(data) {
  health_coverage_dep_m <- data[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6090)]
  return(health_coverage_dep_m)
}

# Afiliación al sistema de salud
afiliacion_salud <- function(data) {
  health_affiliation_dept_m <- data[!is.na(P6100), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6100)]
  return(health_affiliation_dept_m)
}



getwd()
 colnames(a)
