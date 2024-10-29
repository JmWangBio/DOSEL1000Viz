
# Create a reactive object with no values
rv_CV <- reactiveValues()
selected_CV_combinations <- reactiveVal(data.frame(
  Compound = character(),
  CellLine = character(),
  Time = character(),
  Dose = character(),
  CombID = numeric(),
  stringsAsFactors = FALSE
))

# Update compound selection based on available data
observe({
  compound_choices_df <- dbGetQuery(con, "SELECT pert_name FROM pert")
  compound_choices <- compound_choices_df$pert_name
  updateSelectizeInput(session, inputId = "compoundCV", 
                       choices = compound_choices,
                       server = TRUE, selected = "bazedoxifene : BRD-K90195324 (REP.A026)")
})

# Update cell line selection based on the selected compound
observeEvent(input$compoundCV, {
  req(input$compoundCV)
  selected_pert_id_df <- dbGetQuery(con, sprintf("SELECT pert_id FROM pert 
                                                 WHERE pert_name = '%s'", 
                                                 input$compoundCV))
  rv_CV$selected_pert_id <- selected_pert_id_df$pert_id[1]
  
  req(rv_CV$selected_pert_id)
  cell_line_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT cell_id FROM combination 
                                                  WHERE pert_id = %s", 
                                                  rv_CV$selected_pert_id))
  cell_line_choices <- cell_line_choices_df$cell_id
  updateSelectInput(session, "cellLineCV", choices = cell_line_choices, selected = ifelse("MCF7" %in% cell_line_choices, "MCF7", cell_line_choices[1]))
})

# Update time selection based on the selected compound and cell line
observeEvent(list(input$compoundCV, input$cellLineCV), {
  req(input$compoundCV, input$cellLineCV, rv_CV$selected_pert_id)
  time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM combination
                                             WHERE pert_id = %s AND cell_id = '%s'",
                                             rv_CV$selected_pert_id,
                                             input$cellLineCV))
  time_choices <- time_choices_df$pert_time
  updateSelectInput(session, "timeCV", choices = time_choices)
})

# Determine the combination ID based on the selected compound, cell line, and time
# Update dose selection based on the selected combination ID
observeEvent(list(input$compoundCV, input$cellLineCV, input$timeCV), {
  req(input$compoundCV, input$cellLineCV, input$timeCV, rv_CV$selected_pert_id)
  selected_comb_id_df <- dbGetQuery(con, sprintf("SELECT comb_id FROM combination
                                                 WHERE pert_id = %s AND
                                                 cell_id = '%s' AND
                                                 pert_time = %s",
                                                 rv_CV$selected_pert_id,
                                                 input$cellLineCV,
                                                 input$timeCV))
  rv_CV$selected_comb_id <- selected_comb_id_df$comb_id[1]
  
  req(rv_CV$selected_comb_id)
  dose_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_dose FROM combination_gene cg
                                             JOIN combination c ON cg.comb_id = c.comb_id
                                             JOIN test t ON cg.comb_gene_id = t.comb_gene_id
                                             WHERE c.comb_id = %s",
                                             rv_CV$selected_comb_id))
  dose_choices <- dose_choices_df$pert_dose
  if (is.numeric(dose_choices)) {
    names(dose_choices) <- signif(dose_choices, 2)
  }
  updateSelectInput(session, "doseCV", choices = dose_choices)
})

# Add selected combination to the list
observeEvent(input$addCVCombo, {
  new_combo <- data.frame(
    Compound = input$compoundCV,
    CellLine = input$cellLineCV,
    Time = input$timeCV,
    Dose = input$doseCV,
    CombID = rv_CV$selected_comb_id,
    stringsAsFactors = FALSE
  )
  
  current_combos <- selected_CV_combinations()
  
  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_CV_combinations(rbind(current_combos, new_combo))
  }
})

# Clear selected combinations
observeEvent(input$clearCVCombos, {
  selected_CV_combinations(data.frame(
    Compound = character(),
    CellLine = character(),
    Time = character(),
    Dose = character(),
    CombID = numeric(),
    stringsAsFactors = FALSE
  ))
})

# Display selected combinations
output$CVComboList <- renderUI({
  combo_df <- selected_CV_combinations()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(combo_df$Compound[i],
                          combo_df$CellLine[i],
                          paste0(combo_df$Time[i], "h"),
                          paste0(signif(as.numeric(combo_df$Dose[i]), 2), "uM"),
                          sep = " - ")),
          column(4, actionButton(inputId = paste0("remove_cv_", i),
                                 label = "Remove",
                                 class = "btn-danger"))
        )
      })
    )
  } else {
    "No combinations added yet."
  }
})

# Remove selected combination from the list
observe({
  combo_df <- selected_CV_combinations()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_cv_", i)]], {
      updated_combos <- combo_df[-i, ]
      selected_CV_combinations(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent(input$plotCVBtn, {
  isolate({
    combo_df <- selected_CV_combinations()
    if (nrow(combo_df) > 0) {
      filtered_df_lst <- lapply(seq_len(nrow(combo_df)), function(i) {
        combo <- combo_df[i, ]
        
        selected_comb_gene_id_df <- dbGetQuery(con, sprintf("SELECT comb_gene_id FROM combination_gene
                                                            WHERE comb_id IN (%s)",
                                                            combo$CombID))
        selected_comb_gene_id_lst <- selected_comb_gene_id_df$comb_gene_id
        
        query <- sprintf("SELECT t.*, gi.symbol FROM combination_gene cg
                         JOIN test t ON t.comb_gene_id = cg.comb_gene_id
                         JOIN gene_info gi ON cg.gene = gi.gene
                         WHERE t.comb_gene_id IN (%s) AND t.pert_dose > %s AND t.pert_dose < %s",
                         paste(selected_comb_gene_id_lst, collapse = ","),
                         as.numeric(combo$Dose) * 0.98, as.numeric(combo$Dose) * 1.02)
        filtered_df <- dbGetQuery(con, query)
        filtered_df <- filtered_df %>%
          mutate(adj.pval = p.adjust(pval, method = "fdr"),
                 color = ifelse(adj.pval < 0.05, "red", "grey"),
                 text_label = paste(symbol, "<br>Log2FC:", round(Diff, 2),
                                    "<br>Adj. P-Value:", signif(adj.pval, 2)),
                 Compound = combo$Compound,
                 CellLine = combo$CellLine,
                 Time = combo$Time,
                 Dose = combo$Dose,
                 Plot = i)
      })
    } else {
      filtered_df_lst <- list()
    }
    rv_CV$filtered_df_lst <- filtered_df_lst
  })
})

# Generate and display the interactive Compound View volcano plot
output$CVPlot <- renderPlotly({
  req(input$plotCVBtn, rv_CV$filtered_df_lst)
  isolate({
    combo_df <- selected_CV_combinations()
    if (nrow(combo_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_CV$filtered_df_lst)), function(i) {
        filtered_df <- rv_CV$filtered_df_lst[[i]]
        combo <- combo_df[i, ]
        
        suppressWarnings({
          gg <- ggplot(filtered_df,
                       aes(x = Diff, y = -log10(adj.pval), color = color)) +
            geom_point(aes(text = text_label), size = 2, alpha = 0.5) +
            scale_color_manual(values = c("grey" = "grey", "red" = "red")) +
            geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
            annotate("text", x = 0, y = max(c(-log10(filtered_df$adj.pval), -log10(0.05))) + 1,
                     label = paste0(combo$Compound, " - \n",
                                    paste(combo$CellLine,
                                          paste0(combo$Time, "h"),
                                          paste0(signif(as.numeric(combo$Dose), 2), "uM"),
                                          sep = " - ")),
                     size = 3, hjust = 0.5) +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(limits = c(-1.2, 1.2) * max(abs(filtered_df$Diff)))          
        })
        
        ggplotly(gg, tooltip = "text")
      })
      
      num_rows <- ceiling(length(plot_list) / 2)
      total_height <- num_rows * 400 + 25 * (num_rows - 1)
      total_width <- ifelse(length(plot_list) <= 1, 400, 800)
      
      # Use subplot to arrange multiple plots
      suppressWarnings({
        subplot(plot_list, nrows = num_rows,
                shareX = FALSE, shareY = FALSE) %>%
          layout(height = total_height,
                 width = total_width,
                 annotations = list(list(x = 0.5,
                                         y = -0.1 * 400 / total_height,
                                         text = "Log2 Fold Change",
                                         showarrow = FALSE, xref = 'paper', yref = 'paper',
                                         font = list(size = 12)),
                                    list(x = -0.15 * 400 / total_width,
                                         y = 0.5,
                                         text = "-Log10(Adj. P-Value)",
                                         showarrow = FALSE, xref = 'paper', yref = 'paper',
                                         textangle = -90,
                                         font = list(size = 12))),
                 margin = list(l = 80,
                               r = 40,
                               b = 80,
                               t = 40))
      })
    } else {
      NULL
    }
  })
})

# Activate download button for output
output$downloadCV <- renderUI({
  if (length(rv_CV$filtered_df_lst) > 0) {
    downloadButton('CVOutputFile', 'Download Data')
  }
})

# Export result to output for downloading
output$CVOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_filtered_df <- do.call('rbind', lapply(rv_CV$filtered_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, Compound, CellLine, Time, Dose, symbol,
                      Diff, pval, adj.pval) %>%
        `colnames<-`(c("Plot", "Compound", "Cell Line", "Time (Unit: h)", "Dose (Unit: uM)", "Gene",
                       "Log2FC", "P-Value", "Adj. P-Value"))
    }))
    write.table(all_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)
