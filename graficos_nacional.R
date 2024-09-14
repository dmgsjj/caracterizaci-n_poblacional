# Pirámide Poblacional
output$pyramidPlot <- renderPlot({
  #### PIRAMIDE POBLACIONAL#####
  # Convertir age_group en un factor con niveles ordenados
  Pyramid_wide[, age_group := factor(age_group, 
                                     levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                                "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))]
  
  # Ordenar el data.table por `age_group`
  setorder(Pyramid_wide, age_group)
  
  
  ggplot(Pyramid_wide, aes(x = age_group)) +
    geom_bar(aes(y = Mujeres_pct, fill = "Mujeres"), stat = "identity") +
    geom_bar(aes(y = Hombres_pct, fill = "Hombres"), stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    coord_flip() +  # Gira la gráfica para que se vea como una pirámide
    scale_y_continuous(labels = function(x) paste0(abs(x), "%"), 
                       breaks = seq(-10, 10, by = 1)) +  # Ajusta el intervalo de las etiquetas
    labs(x = "Grupos de Edad",
         y = "Porcentaje de Población",
         fill = "Sexo") +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold"),  # Ejes en negrita
          axis.text = element_text(face = "bold"),  # Texto de los ejes en negrita
          panel.grid.major = element_blank(),  # Quitar las líneas de fondo
          panel.grid.minor = element_blank())+
    # Agregar etiquetas de porcentaje en cada barra
    geom_text(aes(y = Mujeres_pct, label = paste0(round(abs(Mujeres_pct), 1), "%")), 
              vjust = 0.5, hjust = ifelse(abs(Pyramid_wide$Mujeres_pct) < 5, 1.2, -0.2), 
              color = "black", size = 3.5) +
    geom_text(aes(y = Hombres_pct, label = paste0(round(Hombres_pct, 1), "%")), 
              vjust = 0.5, hjust = ifelse(abs(Pyramid_wide$Hombres_pct) < 5, -0.2, 1.2), 
              color = "black", size = 3.5)
  
})


# Calcular las estadísticas de género
output$genderPiePlot <- renderPlot({
  
  # Crear el gráfico circular
  ggplot(sex, aes(x = "", y = Percentage, fill = P3271)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    coord_polar(theta = "y") +  # Convertir la gráfica en un gráfico circular
    labs(title = "Distribución de Género",
         fill = "Género") +
    theme_void() +  # Quitar el fondo y los ejes
    geom_text(aes(label = paste0(format(personas, big.mark = ",", scientific = FALSE), "\n",
                                 round(Percentage, 1), "%")),
              position = position_stack(vjust = 0.5), size = 5, fontface = "bold", , color = "white") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar el título
      legend.position = "left",  # Colocar la leyenda en la parte inferior
      legend.title = element_text(face = "bold"),  # Negrita para el título de la leyenda
      legend.text = element_text(face = "bold")  # Negrita para el texto de la leyenda
    )
  
})


# Estado Civil

output$maritalStatusBarPlot <- renderPlot({
  marital_status <- marital_status[order(-personas)]
  
  ggplot(marital_status, aes(x = reorder(as.factor(P6070), personas), y = personas, fill = as.factor(P6070))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") +  # Usar una paleta de colores intensos
    labs(x = "Estado Civil", y = "Número de Personas", title = "Estado Civil") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Ajustar tamaño de fuente y aplicar negrita para eje Y
      axis.text.x = element_text(size = 12, face = "bold"),  # Ajustar tamaño de fuente y aplicar negrita para eje X (si es necesario)
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14, face = "bold"),  # Ajustar tamaño de los títulos de los ejes y aplicar negrita
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal
})


# Nivel Educativo Alcanzado

output$educationBarPlot <- renderPlot({
  
  ggplot(education_achieved, aes(x = reorder(as.factor(P3042), personas), y = personas, fill = as.factor(P3042))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para mejor contraste
    labs(x = "Nivel Educativo", y = "Número de Personas") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),  # Ajustar tamaño de fuente para eje Y
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14, , face = "bold"),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  color = ifelse(education_achieved$personas >= 400000, "white", "black")),  # Condicional para el color del texto
              hjust = ifelse(education_achieved$personas >= 400000, 1.2, -0.2),  # Ajustar posición según el color
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal
})



# Ingreso por Nivel de Escolaridad
output$incomeBarPlot <- renderPlot({
  
  # Ordenar los datos de mayor a menor según el número de personas
  inglabo <- inglabo[order(-personas)]
  
  ggplot(inglabo, aes(x = reorder(as.factor(P3042), personas), y = personas, fill = as.factor(P3042))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Nivel Educativo", y = "Ingreso Promedio") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 12),  # Ajustar tamaño de fuente para eje Y
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  color = ifelse(inglabo$personas >= 4000000, "white", "black")),  # Condicional para el color del texto
              hjust = ifelse(inglabo$personas >= 4000000, 1.2, -0.2),  # Ajustar posición según el color
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal
})



# Mercado Laboral
output$laborMarketBarPlot <- renderPlot({
  labor_data <- table_variables_to_graph %>%
    group_by(sexo) 
  
  labor_data_long <- pivot_longer(labor_data, cols = c("ocupados", "desocupados", "fuerza_trabajo"),
                                  names_to = "variable",
                                  values_to = "valor") %>%
    # Ordenar los niveles del factor `variable` en función del valor
    mutate(variable = factor(variable, levels = unique(variable[order(valor)])))
  
  ggplot(labor_data_long, aes(x = variable, y = valor, fill = variable)) +
    geom_bar(stat = "identity")  +
    scale_fill_viridis_d(option = "viridis") +  # Usar una paleta de colores intensos
    facet_wrap(~ sexo) +
    labs(x = "Variable", y = "Numero de personas") +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Ajustar tamaño de fuente y aplicar negrita para eje Y
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(valor, 0), 
                  hjust = ifelse(valor < max(valor) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(valor < max(valor) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal
})
#tasa de desempleo

output$laborMarketBarPlot2 <- renderPlot({
  
  labor_data <- table_variables_to_graph %>%
    group_by(sexo) 
  
  labor_data_long <- pivot_longer(labor_data, cols = c("tasa_desempleo", "tasa_ocupacion"),
                                  names_to = "variable",
                                  values_to = "valor")
  
  ggplot(labor_data_long, aes(x = variable, y = valor, fill = variable)) +
    geom_bar(stat = "identity")  +
    scale_fill_viridis_d(option = "viridis") +  # Usar una paleta de colores intensos
    facet_wrap(~ sexo) +
    labs(x = "Variable", y = "Porcentaje") +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Ajustar tamaño de fuente y aplicar negrita para eje Y
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(valor, 1), 
                  hjust = ifelse(valor < max(valor) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(valor < max(valor) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal
})

# Tipo de Trabajo
output$jobTypeBarPlot <- renderPlot({
  job_type <- t_job %>%
    group_by(P6430) 
  
  # Ordenar los niveles de P6430 según el valor de personas en orden descendente
  job_type <- job_type %>%
    arrange(desc(personas))
  
  ggplot(job_type, aes(x = reorder(as.factor(P6430), personas), y = personas, fill = as.factor(P6430))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Tipo de Trabajo", y = "Número de Personas") +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),   # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal  tal      
  
})

# Tipo de Vivienda
output$housingTypeBarPlot <- renderPlot({
  
  # Ordenar los datos por el número de personas en orden descendente
  t_house <- t_house %>%
    arrange(desc(personas))
  
  ggplot(t_house, aes(x = reorder(as.factor(P5090), personas), y = personas, fill = as.factor(P5090))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Tipo de Vivienda", y = "Número de Personas") +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),   # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal  
})


# Condiciones del Hogar
output$homeConditionsBarPlot <- renderPlot({
  
  
  # Transformar los datos en formato largo
  home_conditions_long <- pivot_longer(
    table_home_conditions, 
    cols = c("porcentaje_energia", "porcentaje_gas", 
             "porcentaje_alcantarillado", "porcentaje_acuaducto"),
    names_to = "variable",
    values_to = "valor"
  )
  
  # Ordenar las barras de mayor a menor dentro de cada grupo
  home_conditions_long <- home_conditions_long %>%
    group_by(variable) %>%
    arrange(desc(valor), .by_group = TRUE)
  
  # Crear el gráfico
  ggplot(home_conditions_long, aes(x = reorder(variable, -valor), y = valor, fill = acceso)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  # Separar las barras y que la más grande esté primero
    scale_fill_viridis_d(option = "viridis") +  # Usar una paleta de viridis para colores contrastantes
    labs(x = "Condiciones del Hogar", y = "Porcentaje") +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(valor, 0)),
              position = position_dodge(width = 0.9),  # Posicionar las etiquetas en el centro de las barras
              color = "black",  # Cambiar el color del texto a blanco
              size = 4, fontface = "bold",
              vjust = ifelse(home_conditions_long$valor / max(home_conditions_long$valor) < 2, 1, 1),  # Ajustar la posición vertical del texto
              hjust = ifelse(home_conditions_long$valor / max(home_conditions_long$valor) < 0.5, 1, 1.8)) +  # Ajustar la posición horizontal del texto
    coord_flip()  # Cambiar la orientación a horizontal
  
  
})

# Acceso a Salud
output$healthCoverageBarPlot <- renderPlot({
  
  ggplot(health_coverage, aes(x = reorder(as.factor(P6090),personas), y = personas, fill = as.factor(P6090))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Acceso a Salud", y = "Número de Personas")+
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),   # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal  
})

# Tipo de Afiliación al Sistema de Salud
output$healthAffiliationBarPlot <- renderPlot({
  
  
  ggplot(t_health, aes(x = reorder(as.factor(P6100), personas), y = personas, fill = as.factor(P6100))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Tipo de Afiliación al Sistema de Salud", y = "Número de Personas")+
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal  tal  
})

# Motivo de Migración
output$migrationReasonsBarPlot <- renderPlot({
  
  
  ggplot(reasons_migration, aes(x = reorder(as.factor(P3386), personas), y = personas, fill = as.factor(P3386))) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d(option = "viridis") + # Usar una paleta de viridis para colores contrastantes
    labs(x = "Motivo de Migración", y = "Número de Personas")+
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.y = element_text(size = 12, face = "bold"),  # Rotar y ajustar tamaño de fuente
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centrar y ajustar el título
      axis.title = element_text(size = 14),  # Ajustar tamaño de los títulos de los ejes
      legend.title = element_blank()  # Eliminar título de la leyenda
    ) +
    geom_text(aes(label = round(personas, 0), 
                  hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1),  # Ajustar posición de la etiqueta
                  color = ifelse(personas < max(personas) * 0.2, "black", "white")),  # Color según posición
              size = 4, 
              fontface = "bold") +  # Añadir etiquetas en las barras
    scale_color_identity() +  # Usar los colores especificados sin escala adicional
    coord_flip()  # Cambiar la orientación a horizontal  tal  
})