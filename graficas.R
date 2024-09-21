pyramid_population_result = pyramid_population_nacional(data)
marital_status_result = marital_status_nacional(data)
sex_result = sex_nacional(data)
education_achieved_result = education_achieved_nacional(data)
income_by_education_result = income_by_education_nacional(data)
group_variables_result = group_variables_nacional(data)
job_type_result = job_type_nacional(data)
calcular_tipo_vivienda_result = calcular_tipo_vivienda_nacional(data)
home_conditions_result = calcular_condiciones_hogar_nacional(data)
calcular_acceso_salud_result = calcular_acceso_salud_nacional(data)
calcular_afiliacion_salud_result = calcular_afiliacion_salud_nacional(data)


######PRIRAMIDE 


# Asegurar que el rango de edad esté ordenado de menor a mayor
pyramid_population_result[, age_group := factor(age_group, 
                                                levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                           "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                                           "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))]

# Asignar colores de la paleta viridis
colors <- viridis(2, option = "viridis")
library(paletteer)
colors <- paletteer_c("ggthemes::Classic Blue", 30)

# Crear gráfico de pirámide poblacional con plotly
pyramid_plotly <- plot_ly(pyramid_population_result, 
                          x = ~Mujeres_pct, 
                          y = ~age_group, 
                          type = 'bar', 
                          name = 'Mujeres', 
                          orientation = 'h', 
                          marker = list(color = colors[10]),  # Color para Mujeres
                          hoverinfo = 'text',
                          text = ~paste0("<b>", round(abs(Mujeres_pct), 1), "%</b>"),  # Valores en blanco y negrita dentro del gráfico
                          textfont = list(color = 'white')) %>%  # Texto en blanco
  
  add_trace(x = ~Hombres_pct, 
            y = ~age_group, 
            type = 'bar', 
            name = 'Hombres', 
            orientation = 'h', 
            marker = list(color = colors[20]),  # Color para Hombres
            hoverinfo = 'text',
            text = ~paste0("<b>", round(abs(Hombres_pct), 1), "%</b>"),  # Valores en blanco y negrita dentro del gráfico
            textfont = list(color = 'white')) %>%  # Texto en blanco
  
  layout(
    barmode = 'overlay',
    xaxis = list(title = 'Porcentaje de Población',
                 tickformat = '.0%',  # Sin decimales
                 range = c(-5, 5),  # Ajustar rango de los ejes para mostrar de 0 a 5 en ambos lados
                 tickvals = seq(-5, 5, by = 1),  # Marcas en el eje x
                 ticktext = paste0(abs(seq(-5, 5, by = 1)), "%"),  # Etiquetas en porcentaje desde 0 a 5
                 showgrid = TRUE, 
                 zeroline = FALSE,
                 titlefont = list(family = "bold", color = 'white'),  # Fuente bold para el título del eje X
                 tickfont = list(family = "bold", color = 'white')),  # Color blanco para el eje X
    yaxis = list(title = 'Grupos de Edad', 
                 categoryorder = "array", 
                 categoryarray = levels(pyramid_population_result$age_group), 
                 titlefont = list(family = "bold", color = 'white'),  # Fuente bold para el título del eje Y
                 tickfont = list(family = "bold", color = 'white')),  # Color blanco para las etiquetas del eje Y
    title = list(text = "Pirámide Poblacional", font = list(family = "bold", size = 22, color = 'white')),  # Título en negrita
    legend = list(title = list(text = 'Sexo', font = list(family = "bold", color = 'white')), 
                  font = list(family = "bold", color = 'white')),  # Color blanco y negrita para la leyenda
    hovermode = "compare",
    
    # Ajustar el tamaño del gráfico, color de fondo y márgenes
    plot_bgcolor = '#013B63',  # Color de fondo
    paper_bgcolor = '#013B63',  # Color del área total del gráfico
    margin = list(l = 100, r = 20, t = 50, b = 50)  # Reducir márgenes para aprovechar el espacio
  )

# Mostrar el gráfico interactivo con los valores en blanco y negrita
pyramid_plotly



# Cargar la paleta de colores
library(paletteer)
colors_pie <- paletteer_c("ggthemes::Classic Blue", 30)

# Usar los mismos colores que el gráfico de pirámide poblacional
colors_gender <- c(colors[10], colors[20])  # Asignar los mismos colores para Hombres y Mujeres

# Crear el gráfico circular con plotly
Sexo_r <- plot_ly(sex_result, 
                  labels = ~P3271, 
                  values = ~Percentage, 
                  type = 'pie',
                  text = ~paste0(format(personas, big.mark = ",", scientific = FALSE), 
                                 "\n", round(Percentage, 1), "%"),
                  textposition = 'inside',  # Colocar las etiquetas dentro de las porciones
                  textinfo = 'text',  # Mostrar solo el texto personalizado
                  insidetextfont = list(color = 'white',  # Texto blanco dentro de las porciones
                                        size = 14, 
                                        family = 'Arial'),  # Estilo del texto
                  hoverinfo = 'label+value+percent',  # Información al pasar el cursor
                  marker = list(colors = colors_gender))  # Usar los mismos colores para hombres y mujeres

# Layout del gráfico
Sexo_r <- Sexo_r %>%
  layout(title = list(text = "Distribución de Género",
                      x = 0.5,  # Centrar el título
                      font = list(size = 16, color = 'white', face = "bold")),  # Título en blanco y negrita
         legend = list(title = list(text = "Género", font = list(color = 'white', face = "bold")),  # Título de la leyenda en blanco
                       font = list(color = 'white', face = "bold"),  # Texto de la leyenda en blanco
                       x = 0.9,  # Posición de la leyenda
                       y = 0.5), 
         plot_bgcolor = '#013B63',  # Color de fondo
         paper_bgcolor = '#013B63',  # Color del área total del gráfico
         margin = list(l = 100, r = 30, t = 10, b = 10))

# Mostrar el gráfico interactivo
Sexo_r




# Cargar las bibliotecas necesarias
library(plotly)
library(paletteer)

# Definir la paleta de colores manualmente
colors <- c("#B4D4DAFF", "#A9D2DCFF", "#9ECFDDFF", "#93CDDFFF", "#86CAE1FF",
            "#7AC7E2FF", "#76C1DFFF", "#72BCDCFF", "#6EB6D9FF", "#6AB1D6FF",
            "#64AAD2FF", "#5BA2CCFF", "#529AC6FF", "#4993C0FF", "#3F8BBAFF",
            "#3885B6FF", "#3281B5FF", "#2D7DB4FF", "#2678B3FF", "#1F74B1FF",
            "#1C6FAEFF", "#1C6AA8FF", "#1C65A3FF", "#1C5F9EFF", "#1C5A99FF",
            "#1F5591FF", "#225188FF", "#244D7FFF", "#254976FF", "#26456EFF")

# Crear el gráfico con plotly
grafico3 <- plot_ly(
  marital_status_result, 
  x = ~personas,  # El número de personas va en el eje X
  y = ~reorder(as.factor(P6070), personas),  # El estado civil va en el eje Y, reordenado
  type = 'bar', 
  orientation = 'h',  # Barras horizontales
  color = ~as.factor(P6070), 
  colors = colors,  # Usar la paleta de colores definida manualmente
  text = ~paste0(format(personas, big.mark = ",", scientific = FALSE)),  # Etiquetas de texto
  textposition = 'auto',  # Posicionar automáticamente las etiquetas dentro o fuera de la barra
  textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
  hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
  marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
)

# Configurar el diseño del gráfico
grafico3 <- grafico3 %>%
  layout(
  # Título en blanco y negrita
    xaxis = list(title = 'Número de Personas',
                 tickformat = ',',  # Formato de los números
                 tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje X en blanco y negrita
                 titlefont = list(size = 14, color = 'white', family = "bold")),  # Título del eje X en blanco y negrita
    yaxis = list(title = '',
                 tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en blanco y negrita
                 titlefont = list(size = 14),
                 automargin = TRUE,
                 ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
                 ticklen = 8,  # Ajustar longitud de las etiquetas
                 tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
                 tickcolor = '#013B63'),  # Aumentar el margen automáticamente
    legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                  font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
    plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
    paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
    margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
  )

# Mostrar el gráfico interactivo
grafico3

#############
grafico4 <- plot_ly(
  education_achieved_result,
  x = ~personas,  # El número de personas en el eje X
  y = ~reorder(as.factor(P3042), personas),  # El nivel educativo en el eje Y, reordenado
  type = 'bar', 
  orientation = 'h',  # Barras horizontales
  color = ~as.factor(P3042), 
  colors = colors,  # Usar paleta de colores viridis
  text = ~format(round(personas, 0), big.mark = ","),  # Etiquetas con separadores de miles con coma
  textposition = 'auto',  # Posicionar automáticamente las etiquetas
  textfont = list(color = 'white', size = 14, family = "bold"),  # Texto en blanco y negrita
  hoverinfo = 'text',  # Mostrar solo las etiquetas al pasar el cursor
  marker = list(line = list(color = 'black', width = 1))  # Borde de las barras
)

# Configurar el diseño del gráfico
grafico4 <- grafico4 %>%
  layout(
    # Título en blanco y negrita
    xaxis = list(title = 'Número de Personas',
                 tickformat = ',',  # Formato de los números
                 tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje X en blanco y negrita
                 titlefont = list(size = 14, color = 'white', family = "bold")),  # Título del eje X en blanco y negrita
    yaxis = list(title = '',
                 tickfont = list(size = 12, color = 'white', family = "bold"),  # Etiquetas del eje Y en blanco y negrita
                 titlefont = list(size = 14),
                 automargin = TRUE,
                 ticklabelposition = "outside",  # Posicionar las etiquetas fuera del eje
                 ticklen = 8,  # Ajustar longitud de las etiquetas
                 tickwidth = 1,  # Ajustar grosor de las líneas de las etiquetas
                 tickcolor = '#013B63'),  # Aumentar el margen automáticamente
    legend = list(title = list(text = '', font = list(color = 'white', face = "bold")),  # Eliminar el título de la leyenda
                  font = list(color = 'white', face = "bold")),  # Texto de la leyenda en blanco y negrita
    plot_bgcolor = '#013B63',  # Fondo del gráfico en azul oscuro
    paper_bgcolor = '#013B63',  # Fondo del área total del gráfico en azul oscuro
    margin = list(l = 150, r = 50, t = 50, b = 50)  # Ajustar márgenes
  )

grafico4


