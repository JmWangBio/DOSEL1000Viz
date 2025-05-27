
tabPanel("Target View",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate volcano plots to visualize transcriptional 
                      changes for any targets in the DOSE-L1000 database.
                      It is especially useful for ranking compounds regulating a selected target based on efficacy, potency, and log2 fold change 
                      across a wide array of cell lines and time points.",
                      HTML("<br><br>Efficacy, potency, log2 fold changes, and p-values are calculated using <b>generalized additive models</b> or <b>robust linear models</b>."),
                      HTML("<br><br><span style='color:red;'>red dots - 95% confidence interval for efficacy excludes 100%, indicating a significant change</span>"),
                      HTML("<br><br><span style='color:blue;'>Use 'Add Condition', 'Remove', and 'Clear All Conditions' to manage your selections. Click 'Generate Plot' to visualize the selected conditions. After plotting, click the 'Download Data' button below to save the data.</span>")),
             helpText(""),
             selectInput("geneGV", 
                         label = "Select Gene:",
                         choices = NULL),
             selectInput("rankGV",
                         label = "Rank Compounds by:",
                         choices = c("Efficacy", "Potency", "p-value (L2FC)"),
                         selected = "Efficacy"),
             selectizeInput("cellLineGV",
                            label = "Select Cell Line:",
                            choices = NULL,
                            multiple = TRUE),
             selectizeInput("timeGV",
                            label = "Select Time (Unit: h):",
                            choices = NULL,
                            multiple = TRUE),
             radioButtons("effect", "Type of effect:",
                          c("Inhibition" = "inhibition",
                            "Activation" = "activation")),
             uiOutput("gamOnlyGV_ui"),
             actionButton("addGVCombo", "Add Condition"),
             actionButton("clearGVCombos", "Clear All Conditions"),
             tags$hr(),
             h4("Selected Conditions (Max: 6)"),
             uiOutput("GVComboList"),
             tags$hr(),
             actionButton("plotGVBtn", "Generate Plot"),
             tags$hr(),
             uiOutput("downloadGVPE"),
             uiOutput("downloadGVL2FC")
           ),
           mainPanel(
             tabsetPanel(
               tabPanel("Pot. and Eff. Ranking",
                        withSpinner(plotlyOutput("GVPEPlot"))),
               tabPanel("Pot. and Eff. Data",
                        tags$hr(),
                        dataTableOutput("GVPETable")),
               tabPanel("Log2 Fold Change Ranking",
                        withSpinner(plotlyOutput("GVL2FCPlot"))),
               tabPanel("Log2 Fold Change Data",
                        tags$hr(),
                        dataTableOutput("GVL2FCTable"))
             )
           )
         )
)
