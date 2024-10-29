
tabPanel("Efficacy vs. Potency",
         sidebarLayout(
           sidebarPanel(
             helpText("This module can generate efficacy vs. potency plots for any combination of 
                      compounds, cell lines, times, and genes in the DOSE-L1000 database. Potency and 
                      efficacy can be compared within a single compound, gene, or cell line."),
             helpText(),
             selectInput("multipleInputChoice",
                         label = "Choose which to include multiple values:",
                         choices = c("Compound", "Gene", "Cell Line")),
             helpText(HTML("<span style='color:blue;'>If no options appear after selecting a compound or cell line, it indicates no data are available for the selections. Please try selecting a different compound or cell line.</span>")),
             
             # Input UI for multiple selections
             selectizeInput("multipleInput", 
                            label = "", 
                            choices = NULL, 
                            multiple = TRUE,
                            options = list(maxItems = 10)),
             
             # Dynamic input UI for single selection
             selectizeInput("singleInput1",
                            label = "",
                            choices = NULL),
             selectizeInput("singleInput2",
                            label = "",
                            choices = NULL),
             selectizeInput("timeEff",
                            label = "",
                            choices = NULL),
             
             # Add combination button
             actionButton("addEffCombo", "Add Combinations"),
             actionButton("clearEffCombos", "Clear All Combinations"),
             tags$hr(),
             h4("Selected Combinations (Max: 6)"),
             uiOutput("EffComboList"),
             tags$hr(),
             
             # Add plot button
             actionButton("plotEffBtn", "Generate Plot"),
             tags$hr(),
             
             # Add download button
             uiOutput("downloadEff"),
             tags$hr(),
             
             # Add notes
             helpText("Notes: NA values won't be shown.")
           ),
           mainPanel(
             withSpinner(plotlyOutput("EffPlot"))
           )
         )
)
