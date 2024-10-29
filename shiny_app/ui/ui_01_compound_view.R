
tabPanel("Compound View",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate volcano plots to visualize compound-induced transcriptional 
                      changes for any compounds in the DOSE-L1000 database. 
                      It is especially useful for understanding the on-target and off-target effects of 
                      different compounds across a wide array of cell lines, time points, and doses.",
                      HTML("<br><br><span style='color:red;'>red dots - adj. p-value &lt; 0.05</span>; <span style='color:gray;'gray dots - adj. p-value &gt; 0.05</span>")),
             helpText(""),
             selectizeInput("compoundCV", 
                            label = "Select Compound:",
                            choices = NULL),
             selectInput("cellLineCV",
                         label = "Select Cell Line:",
                         choices = NULL),
             selectInput("timeCV",
                         label = "Select Time (Unit: h):",
                         choices = NULL),
             selectInput("doseCV",
                         label = "Select Dose (Unit: uM):", 
                         choices = NULL),
             actionButton("addCVCombo", "Add Combination"),
             actionButton("clearCVCombos", "Clear All Combinations"),
             tags$hr(),
             h4("Selected Combinations (Max: 6)"),
             uiOutput("CVComboList"),
             tags$hr(),
             actionButton("plotCVBtn", "Generate Plot"),
             tags$hr(),
             uiOutput("downloadCV")
           ),
           mainPanel(
             withSpinner(plotlyOutput("CVPlot"))
           )
         )
)
