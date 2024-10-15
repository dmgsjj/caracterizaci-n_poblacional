library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(RColorBrewer)
library(viridis)
library(data.table)
# Definir la interfaz de usuario

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Nacional de Migrantes Venezolanos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Nacional", icon = icon("globe"), startExpanded = TRUE,
               menuSubItem("Demografía", tabName = "demographics", icon = icon("users")),
               menuSubItem("Educación", tabName = "education", icon = icon("graduation-cap")),
               menuSubItem("Mercado Laboral", tabName = "labor_market", icon = icon("briefcase")),
               menuSubItem("Vivienda", tabName = "housing", icon = icon("home")),
               menuSubItem("Salud", tabName = "health", icon = icon("medkit")),
               menuSubItem("Motivo de Migración", tabName = "migration_reasons", icon = icon("globe"))
      ),
      menuItem("Grupos", icon = icon("users"), startExpanded = FALSE,
               menuItem("Grupo de Tratamiento", icon = icon("user-check"),
                        menuSubItem("Demografía", tabName = "treatment_demographics", icon = icon("users")),
                        menuSubItem("Educación", tabName = "treatment_education", icon = icon("graduation-cap")),
                        menuSubItem("Mercado Laboral", tabName = "treatment_labor_market", icon = icon("briefcase")),
                        menuSubItem("Vivienda", tabName = "treatment_housing", icon = icon("home")),
                        menuSubItem("Salud", tabName = "treatment_health", icon = icon("medkit")),
                        menuSubItem("Motivo de Migración", tabName = "treatment_migration_reasons", icon = icon("globe"))
               ),
               menuItem("Grupo de Control", icon = icon("user-times"),
                        menuSubItem("Demografía", tabName = "control_demographics", icon = icon("users")),
                        menuSubItem("Educación", tabName = "control_education", icon = icon("graduation-cap")),
                        menuSubItem("Mercado Laboral", tabName = "control_labor_market", icon = icon("briefcase")),
                        menuSubItem("Vivienda", tabName = "control_housing", icon = icon("home")),
                        menuSubItem("Salud", tabName = "control_health", icon = icon("medkit")),
                        menuSubItem("Motivo de Migración", tabName = "control_migration_reasons", icon = icon("globe"))
               )
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body.light-mode {
          background-color: #ffffff;
          color: #000000;
        }
        body.dark-mode {
          background-color: #2c3e50;
          color: #ffffff;
        }
        .toggle-btn {
          position: fixed;
          top: 10px;
          right: 10px;
          padding: 10px;
          cursor: pointer;
          background-color: #007bff;
          color: #ffffff;
          border: none;
          border-radius: 5px;
          z-index: 9999;
        }
      "))
    ),
    tags$script(HTML("
      function toggleMode() {
        var body = document.body;
        if (body.classList.contains('light-mode')) {
          body.classList.remove('light-mode');
          body.classList.add('dark-mode');
        } else {
          body.classList.remove('dark-mode');
          body.classList.add('light-mode');
        }
      }
    ")),
    
    # Botón para alternar entre modos
    tags$button("Alternar Modo Oscuro/Claro", class = "toggle-btn", onclick = "toggleMode()"),
    
    # Establecer modo inicial
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        document.body.classList.add('light-mode');
      });
    ")),
    
    # Aquí van tus elementos de UI (tabItems)
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
      )
    )
  ),
  
  skin = "blue" # Especificamos un tema de color predeterminado
)

server <- function(input, output, session) {
  # Lógica del servidor si es necesario
}

shinyApp(ui, server)
