
tabPanel("ReadMe",
         h2("DOSE-L1000-Viz"),
         
         p(strong("by Junmin Wang")),
         
         p("The LINCS L1000 project has collected gene expression profiles for thousands of compounds 
         across a wide array of concentrations, cell lines, and time points. However, conventional 
         analysis methods often fall short in capturing the information encapsulated within 
         the L1000 dose-response data."),
         
         p("To faciliate the analysis of L1000 dose-response data, we created the DOSE-L1000 database. 
         DOSE-L1000 was created by fitting generalized additive models (GAMs) and robust linear models 
         (RLMs) to quantile-normalized log2-transformed gene expression data in L1000, followed 
         by differential expression analysis and potency/efficacy calculations. Over ", strong("140 million"), " models 
         were fitted to a vast array of ", strong(33395), " compounds, ", strong(82), " cell lines, and ", strong(978), " genes. Details can be 
         found in the following publication:"),
         
         tags$ul(
           tags$li("Wang J, Novick S. DOSE-L1000: unveiling the intricate landscape of compound-induced 
                   transcriptional changes. Bioinformatics 39 (11): btad683.")
         ),
         
         p("Here we introduce DOSE-L1000-Viz, a shiny app that provides rich visualization capabilities 
           for examining the DOSE-L1000 database."),
         
         withMathJax(),
         h3("How to Use Dose-L1000-Viz:"),
         tags$ul(
           tags$li(strong("Dose Response Curves"), " is a module that plots the dose reponse data and GAM-fitted curves 
                   for selected compound-gene pairs. To begin, please ", strong("select"), " a compound, followed by a gene, 
                   cell line, and time. The text following the colon symbol in the compound name is the BROAD ID. ", strong("Hit"), " the 'Add Condition' button to save it (you may select up to 
                   six conditions). Then, ", strong("click"), " 'Generate Plot' to visualize the data. The black dots are the 
                   experimental data, while the light blue curve is the model fit. Values inside the parentheses 
                   represent the 95% confidence intervals of potency and efficacy.",
                   p("- ", strong("Example"), ": To understand how bazedoxifene affects the dose response of ER targets such as BIRC5, 
                     add and plot the conditions of 'bazedoxifene : BRD-K90195324', 'breast : MCF7', '24h', and 'REP.A026' coupled with 
                     'BIRC5'. This shows how ER targets like BIRC5 respond to bazedoxifene.")),
           tags$li(strong("Efficacy vs Potency"), " is a module that illustrates the estimates (\\(\\mu\\)) and standard errors (\\(SE\\)) of efficacy (i.e., % DMSO) and 
                   potency (i.e., IC50 or EC50 depending on the direction of the change) of selected 
                   compound-gene pairs. First, ", strong("select"), " a field for comparison (either compound, gene, 
                   or cell line) to allow multiple inputs. This will be the variable you want to compare 
                   across. Next, ", strong("choose"), " the remaining fields to hold constant (control variables). 
                   Afterwards, ", strong("click"), " the 'Add Condition' button to save the selected condition (a 
                   limit of six conditions applies). Finally, ", strong("click"), " 'Generate Plot' to visualize 
                   the data. The error bars represent the 95% confidence intervals of potency and efficacy: \\(\\exp(\\log(\\mu) \\pm z_{1 - \\frac{0.05}{2}} \\cdot \\log(SE))\\).
                   A compound is considered to have significant effects on a gene if the 95% confidence 
                   interval of its efficacy does not contain the dashed line, i.e., 100% DMSO or equivalently, no change.",
                   p("- ", strong("Example 1"), ": To understand the potency and efficacy of bazedoxifene acting on CTSD, MYC, CCND1, and BIRC5,
                     choose 'Gene' to include multiple values. Then add and plot the conditions of 
                     'bazedoxifene : BRD-K90195324 (REP.A026)', 'breast : MCF7', and '24h' coupled with 
                     'CTSD', 'MYC', 'CCND1', and 'BIRC5'. This shows the quantitative differences in signal propagation to downstream targets following ER inhibition."),
                   p("- ", strong("Example 2"), ": To understand the potency and efficacy of bazedoxifene acting on CTSD in MCF7, HA1E, HT29, and A375 cell lines,
                     choose 'Cell Line' to include multiple values. Then add and plot the conditions of 
                     'bazedoxifene : BRD-K90195324 (REP.A026)', 'CTSD', and '24h' coupled with 
                     'breast : MCF7', 'kidney : HA1E', 'large intestine : HT29', and 'skin : A375'. This shows the cell line-specificity of bazedoxifene."),
                   p("- ", strong("Example 3"), ": To understand the potency and efficacy of toremifene, clomifene, and bazedoxifene acting on CTSD in MCF7 cells,
                     choose 'Compound' to include multiple values. Then add and plot the conditions of 
                     'CTSD', 'breast : MCF7', and '24h' coupled with
                     'toremifene : BRD-K51350053 (REP.A007)', 'clomifene : BRD-K29950728 (REP.A009)', and 'bazedoxifene : BRD-K90195324 (REP.A026)'. 
                     This highlights the quantitative differences among the ER blockers.")),
           tags$li(strong("Target View"), " is a module that generates volcano plots for any target in DOSE-L1000. 
                   To start, please ", strong("select"), " a gene, followed by a cell line and time. 
                   To continue, ", strong("click"), " the 'Add Condition' button to save the selected condition (up 
                   to six conditions are allowed). Once done, ", strong("click"), " 'Generate Plot' to display the 
                   volcano plots. The dashed line represents a p-value threshold of 0.05.",
                   p("- ", strong("Example"), ": To identify PAX8-inhibiting compounds in HA1E kidney cells at 6h, add and plot the condition of 'PAX8', 
                     'kidney : HA1E', and '6h'. This highlights the top hits capable of modulating PAX8 
                     at an early time point.")),
           tags$li(strong("Compound View"),  " is a module that generates volcano plots for any compound in DOSE-L1000. 
                   To start, please ", strong("select"), " a compound, followed by a cell line, time, and dose. To continue, 
                   ", strong("click"), " the 'Add Condition' button to save the selected condition (a maximum of six 
                   conditions can be selected). Once you have made your selections, ", strong("click"), " 'Generate Plot' 
                   to create the volcano plots. The dashed lines represent an adjusted p-value threshold of 0.05 and log2 fold change thresholds of +/-1.",
                   p("- ", strong("Example"), ": To understand how clomifene affects the transcriptome of MCF7 breast cancer 
                     cells in a dose-dependent manner, add and plot the conditions of 'clomifene : BRD-K29950728', 
                     'breast : MCF7', '24h', and 'REP.A009' at '0.041uM', '0.12uM', '0.37uM', '1.1uM', '3.3uM', and '10uM'.")),
           tags$li(strong("Signature Search"), " is a module that identifies perturbation conditions with similar transcriptional signatures. It compares user-provided input to all 82741 reference signatures in the DOSE-L1000 database using Fisher's exact test.
                   To start, please ", strong("provide"), " upregulated and downregulated gene sets in the input boxes (one gene per line). ", strong("Click"), " 'Validate' to check that your genes are present in the L1000 landmark gene space. Once ready, ", strong("click"), " 'Submit' to run the search.",
                   p("- Click ", strong("Example"), " to load a sample signature of pravastatin in HA1E kidney cells, 10uM, 24 hrs post-treatment.")),
           tags$li("Users can ", strong("download"), " the backend database as individual RDS files through the 'Download Data' tab. GSE92742 and 
                   GSE70138 are two phases of the LINCS L1000 project.")
         ),
         
         h3("Important Notes:"),
         
         tags$ul(
           tags$li("Once the 'Generate Plot' button is clicked, a 'Download Data' button will appear. All data 
                   underlying the plots can then be ", strong("downloaded"), " as tab-delimited text files."),
           tags$li("In Compound View, p-values are adjusted within each compound. However, in Target View, 
                   no adjustment is applied. Therefore, please use unadjusted p-values only for ranking 
                   compounds, not for assessing statistical significance."),
           tags$li("In Target View, only the top 1000 perturbation conditions (compound and dose) are displayed 
                   in the volcano plot. The rest of the conditions can be found in the downloadable data."),
           tags$li("Please contact Junmin at jmwang.bio@gmail.com should you have any questions or comments.")
         )
)
