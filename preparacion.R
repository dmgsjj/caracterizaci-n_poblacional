
########### PREPRACION DEL CODIGO  #############

# Cargar paquetes necesarios
install.packages("scales")
library(data.table)
library(scales)

#REEMPLAZAR CODIGO DE LOS DEPARTAMENTOS POR SU NOMBRE RESPECTIVO
data[, DPTO := fcase(
  DPTO == "5", "Antioquia",
  DPTO == "8", "Atlántico",
  DPTO == "11", "Bogotá",
  DPTO == "13", "Bolívar",
  DPTO == "15", "Boyacá",
  DPTO == "17", "Caldas",
  DPTO == "18", "Caquetá",
  DPTO == "19", "Cauca",
  DPTO == "20", "Cesar",
  DPTO == "23", "Córdoba",
  DPTO == "25", "Cundinamarca",
  DPTO == "27", "Chocó",
  DPTO == "41", "Huila",
  DPTO == "44", "La Guajira",
  DPTO == "47", "Magdalena",
  DPTO == "50", "Meta",
  DPTO == "52", "Nariño",
  DPTO == "54", "Norte de Santander",
  DPTO == "63", "Quindío",
  DPTO == "66", "Risaralda",
  DPTO == "68", "Santander",
  DPTO == "70", "Sucre",
  DPTO == "73", "Tolima",
  DPTO == "76", "Valle del Cauca",
  DPTO == "81", "Arauca",
  DPTO == "85", "Casanare",
  DPTO == "86", "Putumayo",
  DPTO == "88", "San Andrés y Providencia",
  DPTO == "91", "Amazonas",
  DPTO == "94", "Guainía",
  DPTO == "95", "Guaviare",
  DPTO == "97", "Vaupés",
  DPTO == "99", "Vichada",
  default = NA_character_
)]



# 1. Estado civil

# Mapear valores de variables con códigos a descripciones legibles
# Ejemplo: Asignación de etiquetas legibles para "estado civil"
replacement_map_estado_civil <- c(
  "1" = "Pareja < 2 años", "2" = "Pareja >= 2 años", "3" = "Casado(a)",
  "4" = "Separado/Divorciado", "5" = "Viudo(a)", "6" = "Soltero(a)"
)
data[, P6070 := replacement_map_estado_civil[as.character(P6070)]]


# 2. Nivel de educación alcanzado

# Mapear los niveles educativos
education_map_short <- c(
  "1" = "Ninguno", "2" = "Preescolar", "3" = "Primaria",
  "4" = "Secundaria", "5" = "Media Académica", "6" = "Media Técnica",
  "7" = "Normalista", "8" = "Técnica Prof.", "9" = "Tecnológica",
  "10" = "Universitaria", "11" = "Especialización", "12" = "Maestría",
  "13" = "Doctorado", "99" = "No sabe"
)
data[, P3042 := education_map_short[as.character(P3042)]]


# 3.Acceso a salud

# Mapear acceso a salud
acceso_health <- c(
  "9" = "no_informa", "2" = "No", "1" = "Sí"
)
data[, P6090 := acceso_health[as.character(P6090)]]

# 4. 
# Afiliación al sistema de salud
replacement_map_afiliacion <- c(
  "1" = "Contributivo", "2" = "Especial", "3" = "Subsidiado", "9" = "No informa"
)

data[, P6100 := replacement_map_afiliacion[as.character(P6100)]]

# 5. Tipo de trabajo

# Mapear tipos de trabajo
ocupacion_map_short <- c(
  "1" = "Empleado privado", "2" = "Empleado gobierno", "3" = "Empleado doméstico",
  "4" = "Cuenta propia", "5" = "Empleador", "6" = "Familiar sin pago",
  "7" = "Trabajador sin pago", "8" = "Jornalero", "9" = "Otro"
)
data[, P6430 := ocupacion_map_short[as.character(P6430)]]

# 6. La vivienda ocupada por este hogar es:

# Mapear tipos de vivienda
propiedad_map_short <- c(
  "1" = "Propia, totalmente pagada", "2" = "Propia, la están pagando",
  "3" = "En arriendo/subarriendo", "4" = "En usufructo", "5" = "Posesión sin título",
  "6" = "Propiedad colectiva", "7" = "Otra"
)
data[, P5090 := propiedad_map_short[as.character(P5090)]]

# 7. Variables de Motivos de Migración

# Motivos de migración
replacement_map_p3386 <- c(
  "1" = "Trabajo", "2" = "Estudio", "3" = "Salud", "4" = "Conflicto armado",
  "5" = "Violencia", "6" = "Desastres", "7" = "Nuevo hogar",
  "8" = "Acompañar hogar", "9" = "Motivos culturales", "10" = "Vivienda propia",
  "12" = "Otro"
)
data[, P3386 := replacement_map_p3386[as.character(P3386)]]

# 8. Sexo de la poblacion
replacement_sexo <- c(
  "1"= "Hombres", "2" = "Mujeres"
)
data[, P3271 := replacement_sexo[as.character(P3271)]]

caract_nacional <- data



# 1. ############################ DEMOGRAFIA ######################

### 1.1 Composición por edad y sexo de la población en venezolana

# 1.1.1 Asignación de grupos de edad para la pirámide poblacional (Piramide_poblacional)

caract_nacional[, age_group := cut(
  P6040,
  breaks = c(0, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
  labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
  right = FALSE
)]

# Suponiendo que 'Migrants' ya es un data.table
Pyramid<- caract_nacional[,  .(personas=sum(FEX_C18)/12), by = .(age_group, P3271)]

# Convertir la tabla a formato ancho
Pyramid_wide <- dcast(Pyramid, age_group ~ P3271, value.var = "personas")

# Calcular el total de personas y porcentajes
total_poblacion <- sum(abs(Pyramid_wide$Hombres)) + sum(Pyramid_wide$Mujeres)
Pyramid_wide$Hombres_pct <- (abs(Pyramid_wide$Hombres) / total_poblacion) * 100
Pyramid_wide$Mujeres_pct <- (Pyramid_wide$Mujeres / total_poblacion) * 100
Pyramid_wide$Mujeres_pct <- -Pyramid_wide$Mujeres_pct

# 1.1.2 POblacion por sexo (Grafico de torta)

sex <- caract_nacional[, .(personas = sum(FEX_C18)/12), by = P3271]
sex[, sum(personas)]

# Calcular el porcentaje
sex[, Percentage := (personas / sum(personas)) * 100]


### 1.2 Estado civil

marital_status <- caract_nacional[!is.na(P6070), .(personas = sum(FEX_C18, na.rm = TRUE) / 12), by = P6070]


# 2. ############################ EDUACIÓN #######################

### 2.1. Nivel de educación alcanzado

education_achieved <- caract_nacional[!is.na(P3042), .(personas = sum(FEX_C18, na.rm = TRUE)/12), by = P3042]

### 2.2 Ingreso por nivel de escolaridad

inglabo <- caract_nacional[!is.na(P3042), .(personas = mean(INGLABO, na.rm =TRUE)), by = P3042]


# 3. ############################ MERCADO LABORAL #######################  

### 3.1 Función para agrupar variables laborales y calcular estadísticas relacionadas

group_variables <- function() {
  # Calcular los datos requeridos
  factor_expansion <- caract_nacional[, .(factor_expansion = sum(FEX_C18) / 12), by = P3271]
  ocupados <- caract_nacional[, .(ocupados = sum(OCI * FEX_C18, na.rm = TRUE) / 12), by = P3271]
  desocupados <- caract_nacional[, .(desocupados = sum(DSI * FEX_C18, na.rm = TRUE) / 12), by = P3271]
  poblacion_edad_trabajar <- caract_nacional[P6040 >= 15, .(poblacion_edad_trabajar = sum(FEX_C18, na.rm = TRUE) / 12), by = P3271]
  
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

# Llamada a las funciones y almacenamiento de los resultados
table_variables_to_graph <- group_variables()




### 3.2 Tipo de trabajo

t_job <- caract_nacional[!is.na(P6430), .(personas = sum(FEX_C18, na.rm =TRUE )/12), by = P6430]


# 4. ############################ VIVIENDA ####################### 

### 4.1 Tipo de vivienda

t_house <- caract_nacional[!is.na(P5090), .(personas = sum(FEX_C18, na.rm =TRUE )/12), by =  P5090]

### 4.2 Condiciones del hogar

# Función para calcular condiciones del hogar
home_conditions <- function() {
  # Calcular cada condición del hogar y almacenarlas en un data.table
  electrical_energy <- caract_nacional[, .(energia_electrica = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S1]
  natural_gas <- caract_nacional[, .(gas_natural = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S2]
  sewer <- caract_nacional[, .(alcantarillado = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S3]
  aqueduct <- caract_nacional[, .(acueducto = sum(PT * FEX_C18, na.rm = TRUE) / 12), by = P4030S5]
  
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
table_home_conditions <- home_conditions()




# 5. ############################ SALUD #######################

### 5.1 Acceso a salud 

health_coverage <- caract_nacional[, .(personas=sum(FEX_C18)/12), by = P6090]

### 5.2 Tipo de  Afiliación al sistema de salud

t_health <- caract_nacional[!is.na(P6100), .(personas = sum(FEX_C18, na.rm = TRUE)/12), by = P6100]

