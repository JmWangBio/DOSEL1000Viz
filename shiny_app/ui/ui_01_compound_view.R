
tabPanel("Compound View",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate volcano plots to visualize transcriptional 
                      changes for any compounds in the DOSE-L1000 database. 
                      It is especially useful for understanding the on-target and off-target effects of 
                      different compounds across a wide array of cell lines, time points, and doses.",
                      HTML("<br><br>Log<sub>2</sub> fold changes and p-values are calculated using <b>generalized additive models</b> or <b> robust linear models</b>."),
                      HTML("<br><br><span style='color:red;'>red dots - adj. p-value &lt; 0.05 and log<sub>2</sub> fold change &gt; 1</span>"),
                      HTML("<br><br><span style='color:blue;'>Use 'Add Condition', 'Remove', and 'Clear All Conditions' to manage your selections. Click 'Generate Plot' to visualize the selected conditions. After plotting, click the 'Download Data' button below to save the data.</span>")),
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
             selectInput("batchCV",
                         label = "Select Batch:",
                         choices = NULL),
             selectInput("doseCV",
                         label = "Select Dose (Unit: uM):", 
                         choices = NULL),
             actionButton("addCVCombo", "Add Condition"),
             actionButton("clearCVCombos", "Clear All Conditions"),
             tags$hr(),
             h4("Selected Conditions (Max: 6)"),
             uiOutput("CVComboList"),
             tags$hr(),
             actionButton("plotCVBtn", "Generate Plot"),
             tags$hr(),
             uiOutput("downloadCV")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Volcano Plots",
                        withSpinner(plotlyOutput("CVPlot"))),
               tabPanel("Gene Expression Data",
                        tags$hr(),
                        dataTableOutput("CVTable"))
             )
           )
         )
)
