library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Lista de departamentos de Colombia
departamentos_colombia <- c(
  "Amazonas", "Antioquia", "Arauca", "Atlántico", "Bolívar", 
  "Boyacá", "Caldas", "Caquetá", "Casanare", "Cauca", 
  "Cesar", "Chocó", "Córdoba", "Cundinamarca", "Guainía", 
  "Guaviare", "Huila", "La Guajira", "Magdalena", "Meta", 
  "Nariño", "Norte de Santander", "Putumayo", "Quindío", 
  "Risaralda", "San Andrés y Providencia", "Santander", 
  "Sucre", "Tolima", "Valle del Cauca", "Vaupés", "Vichada", "Bogotá"
)

# Definir la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Nacional de Migrantes Venezolanos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demografía", tabName = "demographics", icon = icon("users")),
      menuItem("Educación", tabName = "education", icon = icon("graduation-cap")),
      menuItem("Mercado Laboral", tabName = "labor_market", icon = icon("briefcase")),
      menuItem("Vivienda", tabName = "housing", icon = icon("home")),
      menuItem("Salud", tabName = "health", icon = icon("medkit")),
      menuItem("Motivo de Migración", tabName = "migration_reasons", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    fluidRow(
      # Filtros
      box(title = "Seleccionar Nivel", status = "primary", solidHeader = TRUE,
          selectInput("level_selection", "Nivel:", choices = c("Nacional", "Departamental")),
          conditionalPanel(
            condition = "input.level_selection == 'Departamental'",
            selectInput("department_selection", "Seleccionar Departamento:", choices = departamentos_colombia)
          )
      ),
      box(title = "Migración Venezolana", status = "primary", solidHeader = TRUE,
          selectInput("migration_filter", "Migración Venezolana:", choices = c("Todos", "Solo Migración Venezolana"))
      )
    ),
    
    # Agregar los gráficos por pestaña
    tabItems(
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Pirámide Poblacional", status = "primary", solidHeader = TRUE,
                    plotOutput("pyramidPlot")),
                box(title = "Distribución de Género", status = "primary", solidHeader = TRUE,
                    plotOutput("genderPiePlot")),
                box(title = "Estado Civil", status = "primary", solidHeader = TRUE,
                    plotOutput("maritalStatusBarPlot"))
              )
      ),
      tabItem(tabName = "education",
              fluidRow(
                box(title = "Nivel Educativo Alcanzado", status = "primary", solidHeader = TRUE,
                    plotOutput("educationBarPlot")),
                box(title = "Ingreso por Nivel de Escolaridad", status = "primary", solidHeader = TRUE,
                    plotOutput("incomeBarPlot"))
              )
      ),
      tabItem(tabName = "labor_market",
              fluidRow(
                box(title = "Tasa de Desempleo y Ocupación por Género", status = "primary", solidHeader = TRUE,
                    plotOutput("laborMarketBarPlot")),
                box(title = "Tipo de Trabajo", status = "primary", solidHeader = TRUE,
                    plotOutput("jobTypeBarPlot"))
              )
      ),
      tabItem(tabName = "housing",
              fluidRow(
                box(title = "Tipo de Vivienda", status = "primary", solidHeader = TRUE,
                    plotOutput("housingTypeBarPlot")),
                box(title = "Condiciones del Hogar", status = "primary", solidHeader = TRUE,
                    plotOutput("homeConditionsBarPlot"))
              )
      ),
      tabItem(tabName = "health",
              fluidRow(
                box(title = "Acceso a Salud", status = "primary", solidHeader = TRUE,
                    plotOutput("healthCoverageBarPlot")),
                box(title = "Tipo de Afiliación al Sistema de Salud", status = "primary", solidHeader = TRUE,
                    plotOutput("healthAffiliationBarPlot"))
              )
      ),
      tabItem(tabName = "migration_reasons",
              fluidRow(
                box(title = "Motivo de Migración", status = "primary", solidHeader = TRUE,
                    plotOutput("migrationReasonsBarPlot"))
              )
      )
    )
  ),
  
  skin = "blue"
)

server <- function(input, output) {
  
  # Pirámide Poblacional
  output$pyramidPlot <- renderPlot({
    # Asegurarte de que Pyramid_wide contenga solo la población nacional
    Pyramid_wide[, age_group := factor(age_group, 
                                       levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                                                  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85+"))]
    
    setorder(Pyramid_wide, age_group)
    
    ggplot(Pyramid_wide, aes(x = age_group)) +
      geom_bar(aes(y = Mujeres_pct, fill = "Mujeres"), stat = "identity") +
      geom_bar(aes(y = Hombres_pct, fill = "Hombres"), stat = "identity") +
      scale_fill_viridis_d(option = "viridis") + 
      coord_flip() +  
      scale_y_continuous(labels = function(x) paste0(abs(x), "%"), 
                         breaks = seq(-10, 10, by = 1)) +
      labs(x = "Grupos de Edad",
           y = "Porcentaje de Población",
           fill = "Sexo") +
      theme_minimal() +
      theme(axis.title = element_text(face = "bold"),  
            axis.text = element_text(face = "bold"),  
            panel.grid.major = element_blank(),  
            panel.grid.minor = element_blank()) +
      geom_text(aes(y = Mujeres_pct, label = paste0(round(abs(Mujeres_pct), 1), "%")), 
                vjust = 0.5, hjust = ifelse(abs(Pyramid_wide$Mujeres_pct) < 5, 1.2, -0.2), 
                color = "black", size = 3.5) +
      geom_text(aes(y = Hombres_pct, label = paste0(round(Hombres_pct, 1), "%")), 
                vjust = 0.5, hjust = ifelse(abs(Pyramid_wide$Hombres_pct) < 5, -0.2, 1.2), 
                color = "black", size = 3.5)
  })
  
  # Gráfico Circular de Distribución de Género
  output$genderPiePlot <- renderPlot({
    ggplot(sex, aes(x = "", y = Percentage, fill = P3271)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "viridis") +
      coord_polar(theta = "y") +
      labs(title = "Distribución de Género",
           fill = "Género") +
      theme_void() +
      geom_text(aes(label = paste0(format(personas, big.mark = ",", scientific = FALSE), "\n",
                                   round(Percentage, 1), "%")),
                position = position_stack(vjust = 0.5), size = 5, fontface = "bold", color = "white") +
      theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            legend.position = "left",
            legend.title = element_text(face = "bold"),
            legend.text = element_text(face = "bold"))
  })
  
  # Estado Civil
  output$maritalStatusBarPlot <- renderPlot({
    marital_status <- marital_status[order(-personas)]
    ggplot(marital_status, aes(x = reorder(as.factor(P6070), personas), y = personas, fill = as.factor(P6070))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(option = "viridis") +
      labs(x = "Estado Civil", y = "Número de Personas", title = "Estado Civil") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 12, face = "bold"),
            axis.text.x = element_text(size = 12, face = "bold"),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            legend.title = element_blank()) +
      geom_text(aes(label = round(personas, 0), 
                    hjust = ifelse(personas < max(personas) * 0.2, -0.2, 1.1), 
                    color = ifelse(personas < max(personas) * 0.2, "black", "white")), 
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_flip()
  })
  
  # Nivel Educativo Alcanzado
  output$educationBarPlot <- renderPlot({
    ggplot(education_achieved, aes(x = reorder(as.factor(P3042), personas), y = personas, fill = as.factor(P3042))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(option = "viridis") +
      labs(x = "Nivel Educativo", y = "Número de Personas") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 14) +
      theme(axis.text.y = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            legend.title = element_blank()) +
      geom_text(aes(label = round(personas, 0), 
                    color = ifelse(education_achieved$personas >= 400000, "white", "black")),
                hjust = ifelse(education_achieved$personas >= 400000, 1.2, -0.2),
                size = 4, fontface = "bold") +
      scale_color_identity() +
      coord_flip()
  })
  
}



# Iniciar la aplicación de Shiny
shinyApp(ui, server)
