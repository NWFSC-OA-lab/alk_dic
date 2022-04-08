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
                  
                  textInput("alk_sal_slope", h4("Alk vs Salinity Slope"), value = 110.925),
                  textInput("alk_sal_intercept", h4("Alk vs Salinity Intercept"), value = -183.270),
                  
                  uiOutput("filter_alk_lab"),
                  uiOutput("filter_run_by"),
                  uiOutput("filter_experiment"),
                  uiOutput("filter_unit"),
                  uiOutput("filter_unit_id"),
                  uiOutput("filter_water_source"),
                  uiOutput("filter_water_type"),
                  uiOutput("filter_sample_set"),
                  uiOutput("filter_date_collected"),
                  uiOutput("filter_treatment_name"),
                  uiOutput("filter_quality_flag"),
                  
                  checkboxGroupInput("x_axis_vars", "X-axis variable", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source", "water_type","sample_set",
                                                 "date_collected", "treatment_name", "quality_flag", "salinity"),
                                     selected = "experiment", inline = TRUE),
                  checkboxGroupInput("colour_by", "Colour by", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source", "water_type", "sample_set",
                                                 "date_collected", "treatment_name", "quality_flag", "salinity"),
                                     selected = NULL, inline = TRUE),
                  checkboxGroupInput("facet_by", "Facet_by", 
                                     choices = c("alk_lab", "run_by", "experiment", "unit",
                                                 "unit_id", "water_source", "water_type", "sample_set",
                                                 "date_collected", "treatment_name", "quality_flag", "salinity"),
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
                   choices = c("box", "scatter"), inline = TRUE),
      checkboxInput("show_alk_sal_est", "Show alk from sal estimate mean", value = FALSE),
      checkboxInput("show_ref_value", "Show standard reference value", value = FALSE),
      sliderInput("point_size", "Plot point size",
                  min = 0.5, max = 8, value = 1, step = 0.5),
      sliderInput("font_size", "Font size",
                  min = 12, max = 32, value = 16, step = 1),
      checkboxInput("rotate_x_axis", "Rotate x-axis", value = FALSE),
      sliderInput("ySlider", "Y-axis Range)", 
                  min = 2000, max = 6000, value = c(2500, 3500), step = 100),
      checkboxInput("yRangeCheckbox", "Limit Graph Y-axixs Range", value = FALSE),
      checkboxInput("free_y_facet", "Facet free y-axis", value = FALSE)
    )
  )
)

read_fun <- function(file){
  return(read_excel(file) %>% 
           mutate(unit_number = as.character(unit_number)) %>%
           mutate(alkalinity = as.numeric(alkalinity)))
}


# Define server logic ----
server <- function(input, output) {
  values <- reactiveValues(d_alk = NULL, d_treat = NULL)

      
  ## read alk data ----
  # also adds alk estimated from salinity
 observeEvent(input$files, {
    values$d_alk <- input$files$datapath %>%
      map_dfr(read_fun) %>% 
      unite("unit_id", c(unit, unit_number), remove = FALSE) %>%
      mutate(alk_sal_slope = as.numeric(input$alk_sal_slope),
             alk_sal_intercept = as.numeric(input$alk_sal_intercept)) %>%
      mutate(alk_sal_est = salinity * alk_sal_slope + alk_sal_intercept) %>%
      
      mutate(treatment_name = NA)
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
             UnitNumber = as.character(UnitNumber)) %>%
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
      select(-treatment_name) %>%
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
  #water_source include dynamic checkboxes
  output$filter_water_type <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$water_type))
    checkboxGroupInput("water_type_filter", "Include water type", filter_opts, 
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
  output$filter_date_collected <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$date_collected))
    checkboxGroupInput("date_collected_filter", "Include date collected", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #treatment_name include dynamic checkboxes
  output$filter_treatment_name <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$treatment_name))
    checkboxGroupInput("treatment_name_filter", "Include treatment name", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #quality_flag include dynamic checkboxes
  output$filter_quality_flag <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$quality_flag))
    checkboxGroupInput("quality_flag_filter", "Include quality flag", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  
  ## render plot ----
  output$plot <- renderPlot({

    d_filtered <- filter_alk(values$d_alk, input$alk_lab_filter, input$run_by_filter,
                             input$experiment_filter, input$unit_filter, input$unit_id_filter,
                             input$water_source_filter, input$water_type_filter,input$sample_set_filter, 
                             input$date_collected_filter, input$treatment_name_filter, input$quality_flag_filter)
    
    alk_plot(d_filtered, input$plot_type, input$x_axis_vars, input$colour_by, input$facet_by,
             input$point_size, input$yRangeCheckbox, input$ySlider, input$font_size, input$show_alk_sal_est,
             input$rotate_x_axis, input$show_ref_value, input$free_y_facet) 
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



















