
## Author: Junmin Wang
## Date: 10/27/2024
## Note: To run the shiny app, one needs to construct the DOSE-L1000 sqlite database 
## and provide the correct path to the database file (line 37).
## Code to construct the DOSE-L1000 database is available in the "build_db" folder.

## load libraries
library(shiny)
library(DBI)
library(RSQLite)
library(ggplot2)
library(plotly)
library(dplyr)
library(mgcv)
library(shinycssloaders)

## Load helper functions
source(file.path("helper", "fit_GAM.R"))

## user interface
ui <- fluidPage(
  titlePanel("DOSE-L1000 Visualizer"),
  
  tabsetPanel(
    source(file.path("ui", "ui_01_compound_view.R"), local = TRUE)$value,
    source(file.path("ui", "ui_02_gene_view.R"), local = TRUE)$value,
    source(file.path("ui", "ui_03_drc.R"), local = TRUE)$value,
    source(file.path("ui", "ui_04_efficacy_vs_potency.R"), local = TRUE)$value,
    source(file.path("ui", "ui_05_readme.R"), local = TRUE)$value
  )
)

## Define server logic for the Shiny app
server <- function(input, output, session) {
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "/path/to/DOSE_L1000.db")
  
  source(file.path("server", "server_01_compound_view.R"), local = TRUE)$value
  source(file.path("server", "server_02_gene_view.R"), local = TRUE)$value
  source(file.path("server", "server_03_drc.R"), local = TRUE)$value
  source(file.path("server", "server_04_efficacy_vs_potency.R"), local = TRUE)$value
  
  # Close the connection when the server stops
  onStop(function() {
    dbDisconnect(con)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
