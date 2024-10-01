library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyverse)
library(janitor)
library(sf)
library(stringr)
library(rmarkdown)
library(rsconnect)
library(here)

# User-Interface
ui <- fluidPage(
  titlePanel("Data Analysis"),
  
  navbarPage("Navigation",
             
             # Home tab
             tabPanel("Home",
                      sidebarLayout(
                        sidebarPanel(
                          # File upload input
                          fileInput("file", "Browse file", accept = c(".csv")),
                          # Download sample dataset
                          downloadButton("downloadSampleData", "Download the sample dataset"),
                          # Display summary of uploaded file
                          verbatimTextOutput("fileSummary")
                        ),
                        mainPanel(
                          h4("File Summary"),
                          tableOutput("summaryTable")
                        )
                      )
             ),
             
             # Statewise tab
             tabPanel("Statewise",
                      sidebarLayout(
                        sidebarPanel(
                          # Dropdown to select state
                          selectInput("state", "Select a state:", choices = c(NULL)) # dynamically populated
                        ),
                        mainPanel(
                          plotOutput("statePlot"),
                          tableOutput("stateTable")
                        )
                      )
             ),
             
             # Map tab
             tabPanel("Map",
                      leafletOutput("indiaMap", height = 600),  
                      p("Hover over a state to see the summary of rape victims.")
             ),
             
             # Report tab
             tabPanel("Report",
                      h4("Generate Report"),
                      plotOutput("bar_plot"),              
                      downloadButton("downloadBarPlot", "Download Bar Plot"),
                      downloadButton("report", "Generate report"),
                      sliderInput("slider", "Select Year Range", min = 2000, max = 2020, value = c(2010, 2015))
             )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive data upload
  uploaded_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Summary of the uploaded file
  output$fileSummary <- renderPrint({
    req(uploaded_data())
    summary(uploaded_data())
  })
  
  # Table showing the first 10 rows of the uploaded file
  output$summaryTable <- renderTable({
    req(uploaded_data())
    head(uploaded_data(), 10)
  })
  
  # Dynamically populate state dropdown
  observe({
    req(uploaded_data())
    states <- unique(uploaded_data()$Area_Name)
    updateSelectInput(session, "state", choices = states)
  })
  
  # Plot based on selected state
  output$statePlot <- renderPlot({
    req(uploaded_data())
    req(input$state)
    
    # Filter data for the selected state and plot
    state_data <- uploaded_data() %>%
      filter(Area_Name == input$state, Subgroup == "Total Rape Victims")
    
    ggplot(state_data, aes(x = Year, y = Victims_of_Rape_Total)) +
      geom_line(color = "blue") + 
      geom_point(color = "red") +
      labs(title = paste("Total Rape Victims in", input$state),
           x = "Year",
           y = "Total Rape Victims") +
      theme_minimal()
  })
  
  # Table for the selected state
  output$stateTable <- renderTable({
    req(uploaded_data())
    req(input$state)
    
    state_data <- uploaded_data() %>%
      filter(Area_Name == input$state, Subgroup == "Total Rape Victims")
    
    state_data
  })
  
  # Bar plot for all states
  dataf <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read_csv(here("20_Victims_of_rape.csv")) %>%
      clean_names()
  })
  
  output$bar_plot <- renderPlot({
    req(dataf())
    dataf() %>%
      mutate(area_name = fct_reorder(area_name, rape_cases_reported, .desc = TRUE)) %>%
      ggplot(aes(x = area_name, y = rape_cases_reported)) + 
      geom_bar(stat = "identity", fill = "steelblue") + 
      labs(x = "State", y = "Number of Cases", title = "Number of Cases per State (Descending Order)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
  })
  
  # Download handler for the bar plot
  output$downloadBarPlot <- downloadHandler(
    filename = function() {
      paste("state-vs-cases-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
  
  # Map rendering logic
  shpdf <- read_sf("India Shape/india_st.shp") %>%
    mutate(STATE = str_to_title(STATE))
  
  # Process map data reactively
  processed_map_data <- reactive({
    req(dataf())  # Ensure the data is ready
    
    lp1 <- shpdf %>%
      group_by(STATE) %>%
      summarise(geometry = st_union(geometry)) %>%
      distinct(STATE, .keep_all = TRUE)
    
    dataf_selected <- dataf() %>%
      select(area_name, rape_cases_reported) %>%
      group_by(area_name) %>%
      summarise(rape_cases_reported = sum(rape_cases_reported, na.rm = TRUE)) %>%
      distinct(area_name, .keep_all = TRUE)
    
    lp1 <- lp1 %>%
      left_join(dataf_selected, by = c("STATE" = "area_name"))
    
    lp1
  })
  
  output$indiaMap <- renderLeaflet({
    req(processed_map_data())  # Ensure processed data is ready
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g rape cases reported",
      processed_map_data()$STATE, processed_map_data()$rape_cases_reported
    ) %>% lapply(htmltools::HTML)
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = processed_map_data()$rape_cases_reported
    )
    
    leaflet(processed_map_data()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(rape_cases_reported),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
  
  # Report generation logic
  filtered_data <- reactive({
    req(uploaded_data())
    uploaded_data() %>%
      filter(Year >= input$slider[1] & Year <= input$slider[2])
  })
  
  state_plot <- reactive({
    ggplot(filtered_data(), aes(x = Year, y = Victims_of_Rape_Total)) +
      geom_bar(stat = "identity", fill = "purple") +
      #geom_point(color = "red") +
      labs(title = "Rape Victims over Time", x = "Year", y = "Total Rape Victims") +
      theme_minimal()
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste("statewise-report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        slider_value = input$slider,
        plot = state_plot()
      )
      
      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  # Download handler for sample dataset
  output$downloadSampleData <- downloadHandler(
    filename = function() {
      "20_Victims_of_rape.csv"
    },
    content = function(file) {
      file.copy("20_Victims_of_rape.csv", file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
