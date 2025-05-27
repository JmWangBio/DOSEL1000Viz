
tabPanel("Signature Search",
         sidebarLayout(
           sidebarPanel(
             helpText('Provide gene sets in the boxes below (one gene symbol per line). Once ready, click "Submit". ("Validate" is optional.)'),
             textAreaInput("up_gene_symbols", 
                           "Upregulated Genes", 
                           "", 
                           rows = 6),
             textAreaInput("down_gene_symbols",
                           "Downregulated Genes",
                           "",
                           rows = 6),
             actionButton("exampleSig", "Example"),
             actionButton("validateSig", "Validate"),
             actionButton("submitSig", "Submit"),
             tags$hr(),
             textOutput("status_msg"),
             tags$hr(),
             uiOutput("downloadSig"),
             tags$hr(),
             
             # Add instructions
             HTML("<span style='color:blue;'>Click <em>Example</em>, then <em>Validate</em>, and <em>Submit</em> to run a demo of this module. The example loads the gene signature of pravastatin in MCF7 cells, 10uM, 24hrs post-treatment.</span>"),
             tags$hr(),
             helpText("Notes: Only significant reference signatures (adjusted p-value < 0.05) are shown in the table. The complete results can be downloaded.")
           ),
           
           mainPanel(
             withSpinner(dataTableOutput("SigTable"))
           )
         )
)
