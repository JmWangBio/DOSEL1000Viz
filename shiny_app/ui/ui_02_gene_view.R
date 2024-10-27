
tabPanel("Gene View",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate volcano plots to visualize compound-induced transcriptional 
                      changes for any genes in the DOSE-L1000 database.
                      It is especially useful for discovering compounds capable of regulating a selected gene 
                      across a wide array of cell lines, time points, and doses."),
             helpText(""),
             selectInput("geneGV", 
                         label = "Select Gene:",
                         choices = NULL),
             selectInput("cellLineGV",
                         label = "Select Cell Line:",
                         choices = NULL),
             selectInput("timeGV",
                         label = "Select Time (Unit: h):",
                         choices = NULL),
             sliderInput("doseGV",
                         label = "Select a Range for Dose (Unit: uM; Log10 Scale):", 
                         min = -4, 
                         max = 5,
                         value = c(-1, 1),
                         step = 0.1),
             actionButton("addGVCombo", "Add Combination"),
             actionButton("clearGVCombos", "Clear All Combinations"),
             tags$hr(),
             h4("Selected Combinations (Max: 6)"),
             uiOutput("GVComboList"),
             tags$hr(),
             actionButton("plotGVBtn", "Generate Plot"),
             tags$hr(),
             uiOutput("downloadGV")
           ),
           mainPanel(
             withSpinner(plotlyOutput("GVPlot"))
           )
         )
)
