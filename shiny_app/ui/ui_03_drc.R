
tabPanel("Dose Response Curves",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate dose response curves for any condition of 
                      compounds, cell lines, times, and genes in the DOSE-L1000 database.",
                      HTML("<br><br>Data are fitted using <b>generalized additive models</b>."),
                      HTML("<br><br><span style='color:blue;'>Use 'Add Condition', 'Remove', and 'Clear All Conditions' to manage your selections. Click 'Generate Plot' to visualize the selected conditions. After plotting, click the 'Download Data' button below to save the data.</span>")),
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
             selectInput("batchDRC",
                         label = "Select Batch:",
                         choices = NULL),
             actionButton("addDRCCombo", "Add Condition"),
             actionButton("clearDRCCombos", "Clear All Conditions"),
             tags$hr(),
             h4("Selected Conditions (Max: 6)"),
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
