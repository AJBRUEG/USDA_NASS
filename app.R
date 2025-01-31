# Load required libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnassqs)
library(shinyjs)
library(purrr)
library(shinyBS)
library(shinycssloaders)

# Set your NASS API key
nassqs_auth(key = "A5F4A4DE-59B3-301C-A163-AD7901EF6E55")

# List of U.S. states
us_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", 
  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
  "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", 
  "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", 
  "Wisconsin", "Wyoming"
)

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Enable shinyjs for JavaScript integration
  titlePanel("NASS Data Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("crop", "Select Crop:", choices = c("Corn", "Soybeans", "Wheat"), selected = "Corn"),
      selectInput("state", "Select State:", choices = us_states, selected = "Illinois"),
      numericInput("start_year", "Start Year:", value = 2020, min = 1950, max = as.integer(format(Sys.Date(), "%Y"))),
      numericInput("end_year", "End Year:", value = 2021, min = 1950, max = as.integer(format(Sys.Date(), "%Y"))),
      actionButton("fetch_data", "Fetch Data")
    ),
    
    mainPanel(
      bsCollapse(
        id = "collapse_panels",
        open = NULL,  # Initially collapsed
        bsCollapsePanel(
          title = "Full Table", style = "primary",
          div(style = "overflow-x: auto;", withSpinner(DT::DTOutput("data_table"),type=4))
        ),
        bsCollapsePanel(
          title = "Summary Table", style = "info",
          div(style = "overflow-x: auto;", withSpinner(DT::DTOutput("summary_table"),type=4))
        ),
        bsCollapsePanel(
          title = "Graph", style = "success",
          withSpinner(plotOutput("data_plot"),type=4)
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # JavaScript: Pressing Enter triggers Fetch Data button
  observe({
    runjs('
      $(document).on("keypress", function(e) {
        if(e.which == 13) {  // 13 is the Enter key
          $("#fetch_data").click();  // Simulate button click
        }
      });
    ')
  })
  
  # Function to fetch data
  fetchData <- eventReactive(input$fetch_data, {
    req(input$crop, input$state, input$start_year, input$end_year)
    
    # Translate crop to NASS commodity code
    crop_codes <- list(Corn = "CORN", Soybeans = "SOYBEANS", Wheat = "WHEAT")
    crop_code <- crop_codes[[input$crop]]
    
    if (is.null(crop_code)) {
      showNotification("Invalid crop selected. Please choose a valid crop.", type = "error")
      return(NULL)
    }
    
    all_data <- data.frame()
    
    for (year in seq(input$start_year, input$end_year)) {
      query <- list(
        source_desc = "SURVEY",
        commodity_desc = crop_code,
        year = year,
        state_name = toupper(input$state),
        agg_level_desc = "STATE",
        statisticcat_desc = "YIELD",
        reference_period_desc = "YEAR",
        util_practice_desc = ifelse(crop_code == "CORN", "GRAIN", ""),
        class_desc = ifelse(crop_code=="WHEAT","ALL CLASSES","")
      )
      query <- keep(query, function(x) { x != "" })
      # print(query) #troubleshoot to make sure search is working
      
      tryCatch({
        data <- nassqs(query)
        if (nrow(data) > 0) {
          all_data <- bind_rows(all_data, data)
        }
      }, error = function(e) {
        showNotification(paste("Error fetching data for", year, ":", conditionMessage(e)), type = "error")
      })
    }
    
    if (nrow(all_data) == 0) {
      showNotification("No data found for the selected parameters.", type = "warning")
    }
    
    return(all_data)
  })
  
  output$data_table <- DT::renderDT({ req(fetchData()); fetchData() })
  
  output$summary_table <- DT::renderDT({
    req(fetchData())
    data <- fetchData()
    summary_data <- data %>%
      group_by(commodity_desc, state_name, year) %>%
      summarize(Ave_Yield=mean(Value))
    summary_data
  })
  
  output$data_plot <- renderPlot({
    req(fetchData())
    data <- fetchData()
    ggplot(data, aes(x = year, y = Value,)) +
      stat_summary(group=1,fun.y=mean,geom="line",color="green4",size=2) +
      labs(title = paste(input$crop, "Yield in", input$state, "for", input$start_year, "to", input$end_year),
           x = "Year", y = "Yield") +
      #cleanup
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
