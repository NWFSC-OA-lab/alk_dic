# shiny app for alkalinity plots ----

## libraries ----
library(tidyverse)
library(shiny)
library(readxl)

## source files ----
source("alk_plot_app_fun.R", local = TRUE)

# shiny options ----
options(shiny.maxRequestSize=50*1024^2) 

# Define UI ----
ui <- fluidPage(
  titlePanel("Alkalinity Analysis"),
  sidebarLayout(
    sidebarPanel( width = 2,
                  fileInput("files", h4("Alkalinity Files"), multiple = TRUE, accept = c(".xls", ".xlsx")),
                  fileInput("treatment_file", h4("Treatment File"), multiple = FALSE, accept = c(".csv")),
                  
                   uiOutput("filter_alk_lab"),
                  uiOutput("filter_run_by"),
                  uiOutput("filter_experiment"),
                  uiOutput("filter_unit"),
                  uiOutput("filter_unit_id"),
                  uiOutput("filter_water_source"),
                  uiOutput("filter_sample_set"),
                  uiOutput("filter_date"),
                  uiOutput("filter_treatment_name"),
                  
                  
                  checkboxGroupInput("x_axis_vars", "X-axis variable", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source","sample_set",
                                                 "date", "treatment_name", "salinity"),
                                     selected = "experiment", inline = TRUE),
                  checkboxGroupInput("colour_by", "Colour by", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source","sample_set",
                                                 "date", "treatment_name", "salinity"),
                                     selected = NULL, inline = TRUE),
                  checkboxGroupInput("facet_by", "Facet_by", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source","sample_set",
                                                 "date", "treatment_name", "salinity"),
                                     selected = NULL, inline = TRUE),
                  #dateRangeInput("dates", h4("Date range"), start = "2018-03-13"),
                  downloadButton("downloadData", "Download")
    ),
    mainPanel(
      width = 10,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotOutput("plot", width = "100%", height = "740px"),
      radioButtons("plot_type", h4("Plot Type"), 
                   choices = c("box", "scatter")),
      sliderInput("point_size", "Plot point size",
                  min = 0.5, max = 8, value = 1, step = 0.5),
      sliderInput("ySlider", "Y-axis Range)", 
                  min = 6, max = 9, value = c(7, 8.4), step = 0.1),
      checkboxInput("yRangeCheckbox", "Limit Graph Y-axixs Range", value = FALSE),
    )
  )
)


# Define server logic ----
server <- function(input, output) {
  values <- reactiveValues(d_alk = NULL, d_treat = NULL)
  
  ## read alk data ----
 observeEvent(input$files, {
    values$d_alk <- input$files$datapath %>%
      map_dfr(read_excel) %>% 
      unite("unit_id", c(unit, unit_number), remove = FALSE) 
  })
  
  ## read treatment data ---
  #TODO make treatment join to d_alk conditional on dates
  #TODO make the alk/treatment join a button to control flow
  ## read treatment file ----
  observeEvent(input$treatment_file, {
    values$d_treat <- read_csv(input$treatment_file$datapath) %>%
      filter(!is.na(Unit)) %>%
      mutate(StartDate = as.Date(StartDate, "%m/%d/%y"),
             EndDate = as.Date(EndDate, "%m/%d/%y"),
             UnitNumber = as.numeric(UnitNumber)) %>%
      rename(unit = Unit,
             unit_number = UnitNumber,
             treatment_name = TreatmentName,
             treat_start_date = StartDate,
             treat_end_date = EndDate,
             treat_temperature = Tempereture,
             treat_ph = pH,
             treat_additional = AdditionalTreatmentA)
    #Add treatments to alk file
    values$d_alk <- values$d_alk %>%
      left_join(values$d_treat, by = c("unit", "unit_number"))
  })
  
  ## dynamic filter check boxes ----
  #alk_lab include dynamic checkboxes
  output$filter_alk_lab <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$alk_lab))
    checkboxGroupInput("alk_lab_filter", "Include lab", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #run_by include dynamic checkboxes
  output$filter_run_by <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$run_by))
    checkboxGroupInput("run_by_filter", "Include run by", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #experiment include dynamic checkboxes
  output$filter_experiment <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$experiment))
    checkboxGroupInput("experiment_filter", "Include experiment", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit include dynamic checkboxes
  output$filter_unit <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$unit))
    checkboxGroupInput("unit_filter", "Include unit", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit_id include dynamic checkboxes
  output$filter_unit_id <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$unit_id))
    checkboxGroupInput("unit_id_filter", "Include unit id", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #water_source include dynamic checkboxes
  output$filter_water_source <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$water_source))
    checkboxGroupInput("water_source_filter", "Include water source", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #sample_source include dynamic checkboxes
  output$filter_sample_set <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$sample_set))
    checkboxGroupInput("sample_set_filter", "Include sample set", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #TODO set data as range or check box
  #date include dynamic checkboxes
  output$filter_date <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$date))
    checkboxGroupInput("date_filter", "Include date", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #treatment_name include dynamic checkboxes
  output$filter_treatment_name <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$treatment_name))
    checkboxGroupInput("treatment_name_filter", "Include treatment name", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  
  ## render plot ----
  output$plot <- renderPlot({

    d_filtered <- filter_alk(values$d_alk, input$alk_lab_filter, input$run_by_filter,
                             input$experiment_filter, input$unit_filter, input$unit_id_filter,
                             input$water_source_filter, input$sample_set_filter, input$date_filter,
                             input$treatment_name_filter)
    
    alk_plot(d_filtered, input$plot_type, input$x_axis_vars, input$colour_by, input$facet_by) 
  })
  
  ## download csv ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("alk_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$d_alk, file, row.names = FALSE)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)



















