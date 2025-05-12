library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(leaflet)
library(DT)
library(plotly)
library(scales)
library(shinyjs)
library(htmltools)
library(shinycssloaders)
library(stats)
library(waiter)
library(RColorBrewer)
library(forecast)

# Define NISR color palette
nisr_colors <- c("#046A38", "#FFCE00", "#8A1538", "#4C4C4C", "#0033A0")

# Load data
load_data <- function() {
  # Path to Excel file
  path <- "data/EICV7_Tables_MainIndicatorReport.xlsx"
  
  # Get sheet names
  sheets <- excel_sheets(path)
  
  # Create a list to store datasets
  data_list <- list()
  
  # Read each sheet
  for (sheet in sheets) {
    tryCatch({
      data_list[[sheet]] <- read_excel(path, sheet = sheet)
    }, error = function(e) {
      message(paste("Error reading sheet:", sheet, "-", e$message))
    })
  }
  
  return(data_list)
}

# Create a reactive function that loads data once
get_data <- function() {
  data <- reactiveVal(NULL)
  
  # Initialize the data when the app starts
  observe({
    if (is.null(data())) {
      withProgress(message = 'Loading data...', value = 0.5, {
        data(load_data())
      })
    }
  })
  
  return(data)
}

# Define UI
ui <- fluidPage(
  useShinyjs(),
  waiter::use_waiter(),
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js")
  ),
  
  # Header
  div(class = "header",
      div(class = "logo-container",
          img(src = "nisr_logo.png", class = "logo"),
          img(src = "gov_rw_logo.png", class = "logo")
      ),
      h1("Rwanda Household Living Conditions: EICV 7 Insights"),
      div(class = "theme-toggle",
          actionButton("toggleTheme", "", icon = icon("moon"))
      )
  ),
  
  # Navigation
  navbarPage(
    id = "navBar",
    title = NULL,
    windowTitle = "EICV7 Dashboard",
    
    # Home Tab
    tabPanel(
      "Home",
      div(class = "home-content",
          h2("Welcome to EICV7 Data Explorer"),
          p("This dashboard presents insights from the Integrated Household Living Conditions Survey (EICV7) conducted by the National Institute of Statistics of Rwanda (NISR)."),
          p("The EICV survey provides comprehensive data on socioeconomic conditions across Rwanda, serving as a vital tool for policy planning and development tracking."),
          
          h3("Dashboard Features:"),
          div(class = "feature-cards",
              div(class = "feature-card",
                  icon("chart-bar"),
                  h4("Data Visualization"),
                  p("Interactive charts and maps")
              ),
              div(class = "feature-card",
                  icon("filter"),
                  h4("Dynamic Filtering"),
                  p("Customize by region, gender, and more")
              ),
              div(class = "feature-card",
                  icon("table"),
                  h4("Data Tables"),
                  p("Access detailed statistics")
              ),
              div(class = "feature-card",
                  icon("download"),
                  h4("Export Options"),
                  p("Download charts and filtered data")
              )
          ),
          
          h3("Select a category from the navigation bar to explore the data."),
          
          div(class = "footer",
              p("© 2025 National Institute of Statistics of Rwanda")
          )
      )
    ),
    
    # Poverty & Inequality Tab
    tabPanel(
      "Poverty & Inequality",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("pov_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("pov_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   hr(),
                   downloadButton("download_pov_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Explore poverty indicators across different regions and demographic groups in Rwanda.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Poverty Rate"), 
                               h3(textOutput("poverty_rate")),
                               p("National Average"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Extreme Poverty"), 
                               h3(textOutput("extreme_poverty_rate")),
                               p("National Average"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Poverty Gap"), 
                               h3(textOutput("poverty_gap")),
                               p("Average")))
               ),
               fluidRow(
                 column(12, 
                        h3("Poverty Rates by Province"),
                        withSpinner(plotlyOutput("poverty_by_province")))
               ),
               fluidRow(
                 column(6, 
                        h3("Rural vs Urban Poverty"),
                        withSpinner(plotlyOutput("poverty_rural_urban"))),
                 column(6, 
                        h3("Poverty by Consumption Quintile"),
                        withSpinner(plotlyOutput("poverty_by_quintile")))
               ),
               fluidRow(
                 column(12, 
                        h3("Multidimensional Poverty Indicators"),
                        withSpinner(plotlyOutput("mpi_indicators")))
               )
        )
      )
    ),
    
    # Employment & Labour Tab
    tabPanel(
      "Employment & Labour",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("emp_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("emp_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   selectInput("emp_gender", "Gender", choices = c("All", "Male", "Female")),
                   selectInput("emp_age", "Age Group", choices = c("All", "16-24", "25-34", "35-54", "55+")),
                   hr(),
                   downloadButton("download_emp_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Analyze employment statistics and labor force patterns across Rwanda.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Workforce to Population"), 
                               h3(textOutput("workforce_ratio")),
                               p("Ratio"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Female Employment"), 
                               h3(textOutput("female_employment")),
                               p("Rate"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Youth Employment"), 
                               h3(textOutput("youth_employment")),
                               p("Rate (16-24)")))
               ),
               fluidRow(
                 column(12, 
                        h3("Workforce to Population Ratio by Gender"),
                        withSpinner(plotlyOutput("workforce_by_gender")))
               ),
               fluidRow(
                 column(6, 
                        h3("Distribution by Economic Activity"),
                        withSpinner(plotlyOutput("employment_by_sector"))),
                 column(6, 
                        h3("Workforce Ratio by Province"),
                        withSpinner(plotlyOutput("workforce_by_province")))
               ),
               fluidRow(
                 column(12, 
                        h3("Employment Trends"),
                        withSpinner(plotlyOutput("employment_trends")))
               )
        )
      )
    ),
    
    # Education & Literacy Tab
    tabPanel(
      "Education & Literacy",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("edu_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("edu_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   selectInput("edu_gender", "Gender", choices = c("All", "Male", "Female")),
                   selectInput("edu_quintile", "Consumption Quintile", choices = c("All", "Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)")),
                   hr(),
                   downloadButton("download_edu_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Explore education indicators and literacy rates across different demographic groups.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Literacy Rate (15+)"), 
                               h3(textOutput("literacy_rate")),
                               p("National Average"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Primary School NAR"), 
                               h3(textOutput("primary_nar")),
                               p("Net Attendance Rate"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Secondary School NAR"), 
                               h3(textOutput("secondary_nar")),
                               p("Net Attendance Rate")))
               ),
               fluidRow(
                 column(12, 
                        h3("Literacy Rates by Province and Gender"),
                        withSpinner(plotlyOutput("literacy_by_province_gender")))
               ),
               fluidRow(
                 column(6, 
                        h3("Computer Literacy by Age Group"),
                        withSpinner(plotlyOutput("computer_literacy_by_age"))),
                 column(6, 
                        h3("School Attendance by Age Group"),
                        withSpinner(plotlyOutput("school_attendance_by_age")))
               ),
               fluidRow(
                 column(12, 
                        h3("Education Attainment by Quintile"),
                        withSpinner(plotlyOutput("education_by_quintile")))
               )
        )
      )
    ),
    
    # Health Tab
    tabPanel(
      "Health",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("health_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("health_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   selectInput("health_gender", "Gender", choices = c("All", "Male", "Female")),
                   selectInput("health_quintile", "Consumption Quintile", choices = c("All", "Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)")),
                   hr(),
                   downloadButton("download_health_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Analyze health indicators and access to healthcare services across Rwanda.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Health Insurance Coverage"), 
                               h3(textOutput("health_insurance_rate")),
                               p("National Average"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Distance to Health Facility"), 
                               h3(textOutput("health_facility_distance")),
                               p("Avg. Minutes (Walking)"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Disability Rate"), 
                               h3(textOutput("disability_rate")),
                               p("Population 5+ years")))
               ),
               fluidRow(
                 column(12, 
                        h3("Health Insurance Coverage by Province"),
                        withSpinner(plotlyOutput("insurance_by_province")))
               ),
               fluidRow(
                 column(6, 
                        h3("Health Insurance Type Distribution"),
                        withSpinner(plotlyOutput("insurance_types"))),
                 column(6, 
                        h3("Access to Health Facilities"),
                        withSpinner(plotlyOutput("health_facility_access")))
               ),
               fluidRow(
                 column(12, 
                        h3("Medical Consultation Rates"),
                        withSpinner(plotlyOutput("medical_consultation_rates")))
               )
        )
      )
    ),
    
    # Housing & Utilities Tab
    tabPanel(
      "Housing & Utilities",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("housing_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("housing_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   selectInput("housing_head", "Head of Household", choices = c("All", "Male", "Female")),
                   selectInput("housing_quintile", "Consumption Quintile", choices = c("All", "Q1 (Poorest)", "Q2", "Q3", "Q4", "Q5 (Richest)")),
                   hr(),
                   downloadButton("download_housing_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Explore housing conditions and access to basic utilities across Rwanda.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Electricity Access"), 
                               h3(textOutput("electricity_access")),
                               p("As Main Lighting Source"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Improved Water Source"), 
                               h3(textOutput("improved_water")),
                               p("Access Rate"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Improved Sanitation"), 
                               h3(textOutput("improved_sanitation")),
                               p("Access Rate")))
               ),
               fluidRow(
                 column(12, 
                        h3("Housing Materials by Province"),
                        withSpinner(plotlyOutput("housing_materials")))
               ),
               fluidRow(
                 column(6, 
                        h3("Main Cooking Fuel Types"),
                        withSpinner(plotlyOutput("cooking_fuel"))),
                 column(6, 
                        h3("Internet Access by Province"),
                        withSpinner(plotlyOutput("internet_access")))
               ),
               fluidRow(
                 column(12, 
                        h3("Household Asset Ownership"),
                        withSpinner(plotlyOutput("asset_ownership")))
               )
        )
      )
    ),
    
    # Demographics Tab
    tabPanel(
      "Demographics",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Filters"),
                   selectInput("demo_area", "Area", choices = c("All", "Rural", "Urban")),
                   selectInput("demo_province", "Province", choices = c("All", "Kigali", "Eastern", "Northern", "Southern", "Western")),
                   hr(),
                   downloadButton("download_demo_data", "Download Data"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Analyze demographic patterns and household characteristics across Rwanda.")
                   )
               )
        ),
        column(9,
               fluidRow(
                 column(4, div(class = "value-box value-box-1", 
                               h4("Average Household Size"), 
                               h3(textOutput("avg_household_size")),
                               p("Members per Household"))),
                 column(4, div(class = "value-box value-box-2", 
                               h4("Dependency Ratio"), 
                               h3(textOutput("dependency_ratio")),
                               p("National Average"))),
                 column(4, div(class = "value-box value-box-3", 
                               h4("Sex Ratio"), 
                               h3(textOutput("sex_ratio")),
                               p("Males per 100 Females")))
               ),
               fluidRow(
                 column(12, 
                        h3("Population Distribution by Province"),
                        withSpinner(plotlyOutput("population_by_province")))
               ),
               fluidRow(
                 column(6, 
                        h3("Household Size Distribution"),
                        withSpinner(plotlyOutput("household_size_dist"))),
                 column(6, 
                        h3("Rural vs Urban Population"),
                        withSpinner(plotlyOutput("rural_urban_pop")))
               ),
               fluidRow(
                 column(12, 
                        h3("Migration Patterns"),
                        withSpinner(plotlyOutput("migration_patterns")))
               )
        )
      )
    ),
    
    # Geospatial Tab
    tabPanel(
      "Geospatial Map",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Map Controls"),
                   selectInput("map_indicator", "Select Indicator", 
                               choices = c("Poverty Rate", "Health Insurance Coverage", 
                                           "Literacy Rate", "Electricity Access", 
                                           "Improved Water Access")),
                   checkboxInput("map_labels", "Show District Labels", TRUE),
                   sliderInput("map_opacity", "Layer Opacity", min = 0.1, max = 1, value = 0.7, step = 0.1),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Map"),
                       p("Visualize key indicators across Rwanda's provinces and districts.")
                   )
               )
        ),
        column(9,
               div(class = "map-container",
                   withSpinner(leafletOutput("rwanda_map", height = "700px")),
                   div(id = "map-legend", class = "map-legend")
               )
        )
      )
    ),
    
    # SDG Tracker Tab
    tabPanel(
      "SDG Tracker",
      fluidRow(
        column(12,
               div(class = "sdg-header",
                   h2("Rwanda's Progress Towards Sustainable Development Goals"),
                   p("Track Rwanda's journey towards achieving the 2030 Agenda for Sustainable Development based on EICV7 data.")
               )
        )
      ),
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("SDG Filter"),
                   selectInput("sdg_select", "Select SDG", 
                               choices = c("All SDGs", 
                                           "SDG 1: No Poverty", 
                                           "SDG 3: Good Health", 
                                           "SDG 4: Quality Education",
                                           "SDG 5: Gender Equality",
                                           "SDG 6: Clean Water & Sanitation",
                                           "SDG 7: Affordable & Clean Energy",
                                           "SDG 8: Decent Work & Economic Growth")),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About SDG Tracking"),
                       p("This section relates EICV7 indicators to relevant Sustainable Development Goals and Rwanda's National Strategy for Transformation (NST1).")
                   )
               )
        ),
        column(9,
               uiOutput("sdg_indicators")
        )
      )
    ),
    
    # Data Tables Tab
    tabPanel(
      "Data Tables",
      fluidRow(
        column(3,
               div(class = "sidebar",
                   h3("Table Selection"),
                   selectInput("table_select", "Select Table Category", 
                               choices = c("Demographics", "Poverty", "Education", 
                                           "Health", "Housing", "Employment")),
                   uiOutput("table_selection"),
                   hr(),
                   downloadButton("download_table_data", "Download Selected Table"),
                   hr(),
                   div(class = "sidebar-info",
                       h4("About this Section"),
                       p("Access the complete tables from the EICV7 dataset.")
                   )
               )
        ),
        column(9,
               h3(textOutput("selected_table_title")),
               withSpinner(DTOutput("data_table"))
        )
      )
    ),
    
    # About Tab
    tabPanel(
      "About",
      div(class = "about-container",
          h2("About This Dashboard"),
          p("This interactive dashboard presents data from Rwanda's Integrated Household Living Conditions Survey (EICV7), conducted by the National Institute of Statistics of Rwanda (NISR)."),
          
          h3("About EICV7"),
          p("The Integrated Household Living Conditions Survey (Enquête Intégrale sur les Conditions de Vie des ménages - EICV) is a nationwide survey that collects information on demographic characteristics, housing conditions, health, education, economic activities, and other socioeconomic indicators of the Rwandan population."),
          p("EICV7 was conducted in 2023-2024 and provides up-to-date information to monitor Rwanda's progress in poverty reduction and development goals."),
          
          h3("Data Sources"),
          p("All data presented in this dashboard comes from the EICV7 survey results published by the National Institute of Statistics of Rwanda (NISR)."),
          p("For the complete datasets and reports, please visit the ", 
            tags$a(href = "https://www.statistics.gov.rw", "NISR website", target = "_blank"), "."),
          
          h3("Development"),
          p("This dashboard was developed using R Shiny, with various R packages for data visualization and interactivity."),
          p("For technical questions or feedback about this dashboard, please contact [contact email]."),
          
          div(class = "footer",
              p("© 2025 National Institute of Statistics of Rwanda")
          )
      )
    )
  )
)