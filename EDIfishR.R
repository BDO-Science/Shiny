library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

# Corrected Function to Load EDI Data
load_edi_data <- function(package_id, entity_id, delim = ",", na_values = c(" ", ".", "NA", "")) {
  url <- paste0("https://pasta.lternet.edu/package/data/eml/", 
                gsub("\\.", "/", package_id), "/", entity_id)
  
  tryCatch({
    data <- read_delim(
      url,
      delim = delim,
      na = na_values,
      col_types = cols(.default = "c") # Default all columns as character
    )
    return(data)
  }, error = function(e) {
    stop("Failed to load data. Check the package ID or entity ID.")
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "EDI Data Viewer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDI Data", tabName = "edi_data", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "edi_data",
              fluidRow(
                box(
                  title = "Select EDI Dataset",
                  width = 12, status = "primary", solidHeader = TRUE,
                  textInput("package_id", "Package ID:", value = "edi.1662.1"),
                  textInput("entity_id", "Entity ID:", value = "99a9fd89a42afe10e13bb8c5dcff8990"),
                  actionButton("load_data", "Load Data")
                )
              ),
              fluidRow(
                box(
                  title = "Dataset Preview",
                  width = 12, status = "primary", solidHeader = TRUE,
                  DTOutput("data_table")
                )
              ),
              fluidRow(
                box(
                  title = "Data Visualization",
                  width = 12, status = "primary", solidHeader = TRUE,
                  uiOutput("column_selector"),
                  selectInput("plot_type", "Plot Type:", 
                              choices = c("Scatterplot" = "scatter", "Line Plot" = "line", "Bar Chart" = "bar")),
                  actionButton("generate_plot", "Generate Plot")
                )
              ),
              fluidRow(
                box(
                  title = "Plot Output",
                  width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("data_plot")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  # Load EDI Data
  observeEvent(input$load_data, {
    tryCatch({
      data <- load_edi_data(input$package_id, input$entity_id)
      dataset(data)
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error loading data. Check the package ID and entity ID.", type = "error")
    })
  })
  
  # Display Data Table
  output$data_table <- renderDT({
    req(dataset())
    datatable(dataset(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Create Dynamic Column Selectors
  output$column_selector <- renderUI({
    req(dataset())
    col_names <- names(dataset())
    fluidRow(
      column(6, selectInput("x_column", "X-Axis:", choices = col_names)),
      column(6, selectInput("y_column", "Y-Axis:", choices = col_names))
    )
  })
  
  # Generate Plot
  output$data_plot <- renderPlot({
    req(dataset(), input$x_column, input$y_column, input$plot_type)
    data <- dataset()
    
    gg <- ggplot(data, aes_string(x = input$x_column, y = input$y_column))
    
    if (input$plot_type == "scatter") {
      gg <- gg + geom_point()
    } else if (input$plot_type == "line") {
      gg <- gg + geom_line()
    } else if (input$plot_type == "bar") {
      gg <- gg + geom_bar(stat = "identity")
    }
    
    gg + theme_minimal() +
      labs(title = "EDI Data Visualization", x = input$x_column, y = input$y_column)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

