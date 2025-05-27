
## Author: Junmin Wang
## Date: 05/26/2025
## Note: To run the shiny app, one needs to construct the DOSE-L1000 sqlite database 
## and provide the correct path to the database file (line 65).
## Code to construct the DOSE-L1000 database is available in the "build_db" folder.

## load libraries
library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(mgcv)
library(shinycssloaders)
library(shinyjs)
library(stringr)
library(DT)
library(jsonlite)

## Load helper functions
source(file.path("helper", "fit_GAM.R"))

## user interface
ui <- fluidPage(
  useShinyjs(),
  
  div(
    id = "loading-overlay",
    style = "
      position: fixed;
      top: 0; left: 0; width: 100%; height: 100%;
      background-color: rgba(255, 255, 255, 0.8);
      z-index: 1000; display: none;
    ",
    h3("Processing, please wait...", style = "text-align: center; margin-top: 20%; color: #000;")
  ),
  
  titlePanel("DOSE-L1000 Visualizer"),
  
  tabsetPanel(type = "pills",
              source(file.path("ui", "ui_03_drc.R"), local = TRUE)$value,
              source(file.path("ui", "ui_04_efficacy_vs_potency.R"), local = TRUE)$value,
              source(file.path("ui", "ui_02_gene_view.R"), local = TRUE)$value,
              source(file.path("ui", "ui_01_compound_view.R"), local = TRUE)$value,
              source(file.path("ui", "ui_07_signature_search.R"), local = TRUE)$value,
              source(file.path("ui", "ui_06_readme.R"), local = TRUE)$value
  )
)

## Define server logic for the Shiny app
server <- function(input, output, session) {
  showLoading <- function() {
    shinyjs::runjs("document.getElementById('loading-overlay').style.display = 'block';")
  }
  
  hideLoading <- function() {
    shinyjs::runjs("document.getElementById('loading-overlay').style.display = 'none';")
  }
  
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000_052325.db", flags = SQLITE_RO)
  
  # Load default data
  load("default_data.RData")
  
  source(file.path("server", "server_03_drc.R"), local = TRUE)$value
  source(file.path("server", "server_04_efficacy_vs_potency.R"), local = TRUE)$value
  source(file.path("server", "server_02_gene_view.R"), local = TRUE)$value
  source(file.path("server", "server_01_compound_view.R"), local = TRUE)$value
  source(file.path("server", "server_07_signature_search.R"), local = TRUE)$value
  
  # Close the connection when the server stops
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
