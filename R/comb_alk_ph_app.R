## shiny app to combine alkalinity and pH files

## libraries ----
library(tidyverse)
library(shiny)
library(DT)
library(seacarb)

## source files ----
source("comb_alk_ph_app_fun.R", local = TRUE)

# shiny options ----
options(shiny.maxRequestSize=50*1024^2) 

# Define UI ----
ui <- fluidPage(
  titlePanel("Combine Alk and pH"),
  fluidRow(
    ## alk input ui ----
    column( width = 2,
                  fileInput("alk_file", h4("Alkalinity File"), multiple = FALSE, accept = c(".csv")),
                  
                  uiOutput("alk_filter_alk_lab"),
                  uiOutput("alk_filter_run_by"),
                  uiOutput("alk_filter_experiment"),
                  uiOutput("alk_filter_unit"),
                  uiOutput("alk_filter_unit_id"),
                  uiOutput("alk_filter_water_source"),
                  uiOutput("alk_filter_water_type"),
                  uiOutput("alk_filter_sample_set"),
                  uiOutput("alk_filter_date_collected"),
                  uiOutput("alk_filter_treatment_name"),
                  uiOutput("alk_filter_quality_flag"),
            
                  actionButton("view_match", "View match"), 
                  actionButton("save_match", "Save match"),
                  actionButton("delete_rows", "Delete match table rows"),
                  downloadButton("downloadData", "Download saved match table")
    ),
    ## table ui ----
    column( width = 8,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      h3("Alkalinity table"),
      DT::dataTableOutput("alk_table"),
      h3("pH table"),
      DT::dataTableOutput("ph_table"),
      h3("Current match"),
      DT::dataTableOutput("current_match_view"),
      h3("Saved matches"),
      DT::dataTableOutput("match_table_view")
    ),
    ## ph input ui ----
    column(width = 2,
           fileInput("ph_file", h4("pH File"), multiple = FALSE, accept = c(".csv")),
           uiOutput("ph_filter_run_by"),
           uiOutput("ph_filter_experiment"),
           uiOutput("ph_filter_unit"),
           uiOutput("ph_filter_unit_id"),
           uiOutput("ph_filter_water_source"),
           uiOutput("ph_filter_water_type"),
           uiOutput("ph_filter_sample_set"),
           uiOutput("ph_filter_date_collected"),
           uiOutput("ph_filter_treatment_name")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  values <- reactiveValues(d_alk = NULL, d_ph = NULL, d_match = NULL, match_table = NULL)
  
  ## read alk data ----
  # also adds alk estimated from salinity
  observeEvent(input$alk_file, {
    values$d_alk <- read_csv(input$alk_file$datapath)
      
  })
  
  ## read pH data ----
  # also adds alk estimated from salinity
  observeEvent(input$ph_file, {
    values$d_ph <- read_csv(input$ph_file$datapath) %>%
      unite("unit_id", c(unit, unit_number), remove = FALSE) %>%
      #rename(treatment_name = treatName) %>%
      #mutate(date_collected = as.Date(dateString, "%m/%d/%y")) %>%
      {.}
  })
  
  ## alk dynamic filter check boxes ----
  #alk_lab include dynamic checkboxes
  output$alk_filter_alk_lab <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$alk_lab))
    checkboxGroupInput("alk_lab_filter", "Include lab", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #run_by include dynamic checkboxes
  output$alk_filter_run_by <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$run_by))
    checkboxGroupInput("run_by_filter", "Include run by", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #experiment include dynamic checkboxes
  output$alk_filter_experiment <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$experiment))
    checkboxGroupInput("experiment_filter", "Include experiment", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit include dynamic checkboxes
  output$alk_filter_unit <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$unit))
    checkboxGroupInput("unit_filter", "Include unit", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit_id include dynamic checkboxes
  output$alk_filter_unit_id <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$unit_id))
    checkboxGroupInput("unit_id_filter", "Include unit id", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #water_source include dynamic checkboxes
  output$alk_filter_water_source <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$water_source))
    checkboxGroupInput("water_source_filter", "Include water source", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #water_source include dynamic checkboxes
  output$alk_filter_water_type <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$water_type))
    checkboxGroupInput("water_type_filter", "Include water type", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #sample_source include dynamic checkboxes
  output$alk_filter_sample_set <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$sample_set))
    checkboxGroupInput("sample_set_filter", "Include sample set", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #TODO set data as range or check box
  #date include dynamic checkboxes
  output$alk_filter_date_collected <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$date_collected))
    checkboxGroupInput("date_collected_filter", "Include date collected", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #treatment_name include dynamic checkboxes
  output$alk_filter_treatment_name <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$treatment_name))
    checkboxGroupInput("treatment_name_filter", "Include treatment name", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #quality_flag include dynamic checkboxes
  output$alk_filter_quality_flag <- renderUI({
    filter_opts <- as.character(unique(values$d_alk$quality_flag))
    checkboxGroupInput("quality_flag_filter", "Include quality flag", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  
  ## ph dynamic check boxes ----
  
  ## ph dynamic filter check boxes ----
  #run_by include dynamic checkboxes
  output$ph_filter_run_by <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$run_by))
    checkboxGroupInput("ph_run_by_filter", "Include run by", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #experiment include dynamic checkboxes
  output$ph_filter_experiment <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$experiment))
    checkboxGroupInput("ph_experiment_filter", "Include experiment", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit include dynamic checkboxes
  output$ph_filter_unit <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$unit))
    checkboxGroupInput("ph_unit_filter", "Include unit", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #unit_id include dynamic checkboxes
  output$ph_filter_unit_id <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$unit_id))
    checkboxGroupInput("ph_unit_id_filter", "Include unit id", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #water_source include dynamic checkboxes
  output$ph_filter_water_source <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$water_source))
    checkboxGroupInput("ph_water_source_filter", "Include water source", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #water_source include dynamic checkboxes
  output$ph_filter_water_type <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$water_type))
    checkboxGroupInput("ph_water_type_filter", "Include water type", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #sample_source include dynamic checkboxes
  output$ph_filter_sample_set <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$sample_set))
    checkboxGroupInput("ph_sample_set_filter", "Include sample set", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })

  #date include dynamic checkboxes
  output$ph_filter_date_collected <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$date_collected))
    checkboxGroupInput("ph_date_collected_filter", "Include date collected", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  #treatment_name include dynamic checkboxes
  output$ph_filter_treatment_name <- renderUI({
    filter_opts <- as.character(unique(values$d_ph$treatment_name))
    checkboxGroupInput("ph_treatment_name_filter", "Include treatment name", filter_opts, 
                       selected = filter_opts, inline = TRUE)
  })
  ## render alk table ----
  output$alk_table <- DT::renderDataTable(filter_table(values$d_alk, input$alk_lab_filter, input$run_by_filter,
                             input$experiment_filter, input$unit_filter, input$unit_id_filter,
                             input$water_source_filter, input$water_type_filter,input$sample_set_filter,
                             input$date_collected_filter, input$treatment_name_filter, input$quality_flag_filter) %>%
                               select(alk_lab, experiment, unit_id, water_source, water_type, sample_set,
                                      date_collected, treatment_name, alkalinity, salinity),
                             options = list(paging = TRUE,    ## paginate the output
                                            pageLength = 6,  ## number of rows to output for each page
                                            scrollX = TRUE,   ## enable scrolling on X axis
                                            scrollY = TRUE,   ## enable scrolling on Y axis
                                            autoWidth = TRUE, ## use smart column width handling
                                            server = FALSE),
                             rownames = FALSE
                              )
  
  ## render ph tables ----
  output$ph_table <- DT::renderDataTable(filter_table(values$d_ph, NULL, input$ph_run_by_filter,
                                 input$ph_experiment_filter, input$ph_unit_filter, input$ph_unit_id_filter,
                                 input$ph_water_source_filter, input$ph_water_type_filter,input$ph_sample_set_filter,
                                 input$ph_date_collected_filter, input$ph_treatment_name_filter, NULL) %>%
                                   select(experiment, unit_id, water_source, water_type, sample_set,
                                          date_collected, treatment_name, ph_insitu, salinity, insitu_temperature),
                                 
                                 options = list(paging = TRUE,    ## paginate the output
                                                pageLength = 6,  ## number of rows to output for each page
                                                scrollX = TRUE,   ## enable scrolling on X axis
                                                scrollY = TRUE,   ## enable scrolling on Y axis
                                                autoWidth = TRUE, ## use smart column width handling
                                                server = FALSE),
                                 rownames = FALSE
  )
  ## view current match  ----                                                           
  observeEvent(input$view_match, {
    alk_table  <- filter_table(values$d_alk, input$alk_lab_filter, input$run_by_filter,
                        input$experiment_filter, input$unit_filter, input$unit_id_filter,
                        input$water_source_filter, input$water_type_filter,input$sample_set_filter,
                        input$date_collected_filter, input$treatment_name_filter, input$quality_flag_filter)
    ph_table <- filter_table(values$d_ph, NULL, input$ph_run_by_filter,
                             input$ph_experiment_filter, input$ph_unit_filter, input$ph_unit_id_filter,
                             input$ph_water_source_filter, input$ph_water_type_filter,input$ph_sample_set_filter,
                             input$ph_date_collected_filter, input$ph_treatment_name_filter, NULL)
    values$d_match <- make_match(alk_table, ph_table)
    output$current_match_view <- DT::renderDataTable(values$d_match,
                                              options = list(paging = TRUE,    ## paginate the output
                                                             pageLength = 6,  ## number of rows to output for each page
                                                             scrollX = TRUE,   ## enable scrolling on X axis
                                                             scrollY = TRUE,   ## enable scrolling on Y axis
                                                             autoWidth = TRUE, ## use smart column width handling
                                                             server = FALSE),
                                              rownames = FALSE)
  })
  ## view saved matches
  observeEvent(input$save_match, {
    values$match_table <- rbind(values$match_table, values$d_match)
    output$match_table_view <- DT::renderDataTable(values$match_table,
                                              options = list(paging = TRUE,    ## paginate the output
                                                             pageLength = 6,  ## number of rows to output for each page
                                                             scrollX = TRUE,   ## enable scrolling on X axis
                                                             scrollY = TRUE,   ## enable scrolling on Y axis
                                                             autoWidth = TRUE, ## use smart column width handling
                                                             server = FALSE),
                                              rownames = FALSE)
    
  })
  ## delete match rows ----
  observeEvent(input$delete_rows,{
    if (!is.null(input$match_table_view_rows_selected)) {
      rows_selected <- input$match_table_view_rows_selected
      values$match_table <- values$match_table[-as.numeric(input$match_table_view_rows_selected),]
    }
  })
  
  ## download csv ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("carb_table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$match_table, file, row.names = FALSE)
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)