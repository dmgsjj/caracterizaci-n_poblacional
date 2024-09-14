########### PREPRACIÓN DEL CÓDIGO PARA DEPARTAMENTOS #############

# Cargar paquetes necesarios
library(data.table)
library(scales)

caract_nacional <- data

# Asumimos que `data` es tu data.table con los departamentos ya reemplazados por su nombre
# Aquí un ejemplo de cómo calcular las estadísticas a nivel departamental.

# 1. ############################ DEMOGRAFIA ######################

    # 1. Grupos de edad para pirámide poblacional por departamento
caract_nacional[, age_group := cut(
      P6040,
      breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
      right = FALSE
    )]
    
    pyramid_dept <- caract_nacional[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, age_group, P3271)]
    
    # 2. Estado civil por departamento
    marital_status_dept <- caract_nacional[!is.na(P6070), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6070)]
    
    
    # 3. Sexo de la población por departamento
    sex_dept <- caract_nacional[, .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
    

# 2. ############################ EDUACIÓN #######################
    

    # 1. Nivel de educación alcanzado por departamento
    education_achieved_dept <- caract_nacional[!is.na(P3042), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3042)]
    
    # 2. Nivel de ingreso alcanzado por nivel educativo por departamento
    income_by_education_dept <- caract_nacional[!is.na(P3042) & !is.na(INGLABO), .(ingreso = mean(INGLABO, na.rm = TRUE)), by = .(DPTO, P3042)]

    
# 3. ############################ MERCADO LABORAL #######################  
    
    # 1. Función para agrupar variables laborales y calcular estadísticas relacionadas
    
    group_variables <- function() {
      # Filtrar por grupo
      
      # Calcular los datos requeridos
      factor_expansion <- caract_nacional[, .(factor_expansion = sum(FEX_C18) / 7), by = .(DPTO, P3271)]
      ocupados <- caract_nacional[, .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 7), by = .( DPTO, P3271)]
      desocupados <- caract_nacional[, .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
      desocupados
      
      
      poblacion_edad_trabajar <- caract_nacional[P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3271)]
      
      # Combinar en un data.table
      combined <- data.table(
        ocupados = ocupados$ocupados,
        desocupados = desocupados$desocupados,
        poblacion_edad_trabajar = poblacion_edad_trabajar$poblacion_edad_trabajar,
        factor_expansion = factor_expansion$factor_expansion
      )
      
      # Calcular tasas
      combined[, fuerza_trabajo := (ocupados + desocupados)] # POBLACIÓN ACTIVA
      combined[, tasa_desempleo := (desocupados / fuerza_trabajo) * 100] # TASA DE DESEMPLEO
      combined[, tasa_ocupacion := (ocupados / poblacion_edad_trabajar) * 100] # TASA DE OCUPACIÓN
      combined[, sexo:= c("Hombres", "Mujeres")]
      
      return(combined)
    }
    
    # Llamada a la función para ambos grupos
    table_variables_dep <- group_variables
    
    
# 2. Tipo de trabajo
    
    t_job <- caract_nacional[!is.na(P6430), .(personas = sum(FEX_C18, na.rm =TRUE )/7), by = .(DPTO, P6430) ]  
    
    
# 4. ############################ VIVIENDA ####################### 
    
# 6. Tipo de vivienda por departamento
housing_type_dept <- caract_nacional[!is.na(P5090), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P5090)]
    
# Función para calcular condiciones del hogar
home_conditions <- function() {
  # Calcular cada condición del hogar y almacenarlas en un data.table
  electrical_energy <- caract_nacional[, .(energia_electrica = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S1)]
  natural_gas <- caract_nacional[, .(gas_natural = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S2)]
  sewer <- caract_nacional[, .(alcantarillado = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S3)]
  aqueduct <- caract_nacional[, .(acueducto = sum(PT * FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P4030S5)]
  
  # Crear un data.table combinado con todas las condiciones
  combined <- data.table(
    energia_electrica = electrical_energy$energia_electrica,
    gas_natural = natural_gas$gas_natural,
    alcantarillado = sewer$alcantarillado,
    acueducto = aqueduct$acueducto
  )
  
  # Calcular porcentajes
  total_population <- 2229007
  combined[, porcentaje_energia := (energia_electrica / total_population) * 100]
  combined[, porcentaje_gas := (gas_natural / total_population) * 100]
  combined[, porcentaje_alcantarillado := (alcantarillado / total_population) * 100]
  combined[, porcentaje_acuaducto := (acueducto / total_population) * 100]
  combined[, acceso := c("Si", "No")]
  
  return(combined)
}

# Llamada a las funciones y almacenamiento de los resultados
table_home_conditions_dep <- home_conditions()
    
    
    
# 3. Afiliación al sistema de salud por departamento
health_affiliation_dept <- data[!is.na(P6100), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6100)]



# 5. Ocupación por departamento
occupation_dept <- data[!is.na(P6430), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P6430)]





# 8. Motivos de migración por departamento
migration_reasons_dept <- data[!is.na(P3386), .(personas = sum(FEX_C18, na.rm = TRUE) / 7), by = .(DPTO, P3386)]




