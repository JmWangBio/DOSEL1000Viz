
tabPanel("Dose Response Curves",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate dose response curves for any combination of 
                      compounds, cell lines, times, and genes in the DOSE-L1000 database."),
             helpText(""),
             selectizeInput("compoundDRC", 
                            label = "Select Compound:",
                            choices = NULL),
             selectInput("geneDRC",
                         label = "Select Gene:", 
                         choices = NULL),
             selectInput("cellLineDRC",
                         label = "Select Cell Line:",
                         choices = NULL),
             selectInput("timeDRC",
                         label = "Select Time (Unit: h):",
                         choices = NULL),
             actionButton("addDRCCombo", "Add Combination"),
             actionButton("clearDRCCombos", "Clear All Combinations"),
             tags$hr(),
             h4("Selected Combinations (Max: 6)"),
             uiOutput("DRCComboList"),
             tags$hr(),
             actionButton("plotDRCBtn", "Generate Plot"),
             tags$hr(),
             uiOutput("downloadDRC")
           ),
           mainPanel(
             withSpinner(plotlyOutput("DRCPlot"))
           )
         )
)
