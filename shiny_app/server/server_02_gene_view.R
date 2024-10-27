
# Create a reactive object with no values
rv_GV <- reactiveValues()
selected_GV_combinations <- reactiveVal(data.frame(
  GeneSymbol = character(),
  CellLine = character(),
  Time = character(),
  DoseMin = numeric(),
  DoseMax = numeric(),
  GeneID = numeric(),
  CombGeneIDs = character(),
  stringsAsFactors = FALSE
))

# Update gene selection based on available data
observe({
  gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info")
  gene_choices <- gene_choices_df$symbol
  updateSelectInput(session, inputId = "geneGV", choices = gene_choices)
})

# Update cell line selection based on available data
observeEvent(input$geneGV, {
  req(input$geneGV)
  cell_line_choices_df <- dbGetQuery(con, "SELECT DISTINCT cell_id FROM combination")
  cell_line_choices <- cell_line_choices_df$cell_id
  updateSelectInput(session, "cellLineGV", choices = cell_line_choices)
})

# Update time selection based on the selected cell line
observeEvent(list(input$geneGV, input$cellLineGV), {
  req(input$geneGV, input$cellLineGV)
  time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM combination 
                                             WHERE cell_id = '%s'", 
                                             input$cellLineGV))
  time_choices <- time_choices_df$pert_time
  updateSelectInput(session, "timeGV", choices = time_choices)
})

# Determine the gene ID, the list of combination IDs, and the list of combination gene IDs based on the selected gene, cell line, and time
# Update dose selection based on the selected list of combination gene IDs
observeEvent(list(input$geneGV, input$cellLineGV, input$timeGV), {
  req(input$geneGV, input$cellLineGV, input$timeGV)
  selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info
                                              WHERE symbol = '%s'",
                                              input$geneGV))
  rv_GV$selected_gene <- selected_gene_df$gene[1]

  selected_comb_id_df <- dbGetQuery(con, sprintf("SELECT comb_id FROM combination
                                                 WHERE cell_id = '%s' AND pert_time = %s",
                                                 input$cellLineGV,
                                                 input$timeGV))
  selected_comb_id_lst <- selected_comb_id_df$comb_id

  req(rv_GV$selected_gene)
  selected_comb_gene_id_df <- dbGetQuery(con, sprintf("SELECT comb_gene_id FROM combination_gene cg 
                                                      WHERE cg.comb_id IN (%s) AND
                                                      cg.gene = %s",
                                                      paste(selected_comb_id_lst, collapse = ","),
                                                      rv_GV$selected_gene))
  selected_comb_gene_id_lst <- selected_comb_gene_id_df$comb_gene_id
  rv_GV$selected_comb_gene_ids <- paste(selected_comb_gene_id_lst, collapse = ",")

  req(rv_GV$selected_comb_gene_ids)
  dose_min_max_df <- dbGetQuery(con, sprintf("SELECT MIN(pert_dose) AS min_dose, MAX(pert_dose) AS max_dose 
                                             FROM test WHERE comb_gene_id IN (%s)",
                                             rv_GV$selected_comb_gene_ids))
  dose_min <- dose_min_max_df$min_dose
  dose_max <- dose_min_max_df$max_dose
  updateSliderInput(session, "doseGV",
                    min = floor(log10(dose_min)),
                    max = ceiling(log10(dose_max)),
                    value = c(floor(log10(dose_min)), 
                              ceiling(log10(dose_max))))
})

# Add selected combination to the list
observeEvent(input$addGVCombo, {
  new_combo <- data.frame(
    GeneSymbol = input$geneGV,
    CellLine = input$cellLineGV,
    Time = input$timeGV,
    DoseMin = input$doseGV[1],
    DoseMax = input$doseGV[2],
    GeneID = rv_GV$selected_gene,
    CombGeneIDs = rv_GV$selected_comb_gene_ids,
    stringsAsFactors = FALSE
  )

  current_combos <- selected_GV_combinations()

  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_GV_combinations(rbind(current_combos, new_combo))
  }
})

# Clear selected combinations
observeEvent(input$clearGVCombos, {
  selected_GV_combinations(data.frame(
    GeneSymbol = character(),
    CellLine = character(),
    Time = character(),
    DoseMin = numeric(),
    DoseMax = numeric(),
    GeneID = numeric(),
    CombGeneIDs = character(),
    stringsAsFactors = FALSE
  ))
})

# Display selected combinations
output$GVComboList <- renderUI({
  combo_df <- selected_GV_combinations()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(combo_df$GeneSymbol[i],
                          combo_df$CellLine[i],
                          paste0(combo_df$Time[i], "h"),
                          paste0(signif(10^combo_df$DoseMin[i], 2), "uM"),
                          paste0(signif(10^combo_df$DoseMax[i], 2), "uM"),
                          sep = " - ")),
          column(4, actionButton(inputId = paste0("remove_gv_", i),
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
  combo_df <- selected_GV_combinations()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_gv_", i)]], {
      updated_combos <- combo_df[-i, ]
      selected_GV_combinations(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent(input$plotGVBtn, {
  isolate({
    combo_df <- selected_GV_combinations()
    if (nrow(combo_df) > 0) {
      filtered_df_lst <- lapply(seq_len(nrow(combo_df)), function(i) {
        combo <- combo_df[i, ]
        
        query <- sprintf("SELECT t.*, p.pert_name FROM combination_gene cg 
                         JOIN combination c ON cg.comb_id = c.comb_id 
                         JOIN test t ON cg.comb_gene_id = t.comb_gene_id 
                         JOIN pert p ON c.pert_id = p.pert_id 
                         WHERE t.comb_gene_id IN (%s) AND 
                         t.pert_dose >= %s AND t.pert_dose <= %s",
                         combo$CombGeneIDs,
                         10^combo$DoseMin,
                         10^combo$DoseMax)
        filtered_df <- dbGetQuery(con, query)
        filtered_df <- filtered_df %>%
          mutate(text_label = paste0(pert_name, 
                                    "<br>Dose: ", signif(pert_dose, 2), "uM", 
                                    "<br>Log2FC: ", round(Diff, 2),
                                    "<br>P-Value: ", signif(pval, 2)),
                 GeneSymbol = combo$GeneSymbol,
                 CellLine = combo$CellLine,
                 Time = combo$Time,
                 Plot = i)
      })
    } else {
      filtered_df_lst <- list()
    }
    rv_GV$filtered_df_lst <- filtered_df_lst
  })
})

# Generate and display the interactive Gene View volcano plot
output$GVPlot <- renderPlotly({
  req(input$plotGVBtn, rv_GV$filtered_df_lst)
  isolate({
    combo_df <- selected_GV_combinations()
    if (nrow(combo_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_GV$filtered_df_lst)), function(i) {
        filtered_df <- rv_GV$filtered_df_lst[[i]] %>%
          arrange(pval) %>%
          slice_head(n = 1000)
        combo <- combo_df[i, ]
        
        if (nrow(filtered_df) > 0) {
          suppressWarnings({
            gg <- ggplot(filtered_df,
                         aes(x = Diff, y = -log10(pval)), color = "grey") +
              geom_point(aes(text = text_label), size = 2, alpha = 0.5) +
              geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
              annotate("text", x = 0, y = max(-log10(filtered_df$pval)) + 1,
                       label =  paste(combo$GeneSymbol,
                                      combo$CellLine,
                                      paste0(combo$Time, "h"),
                                      paste0(signif(10^combo$DoseMin, 2), "uM"),
                                      paste0(signif(10^combo$DoseMax, 2), "uM"),
                                      sep = " - "),
                       size = 3, hjust = 0.5) +
              theme_minimal() +
              theme(legend.position = "none") +
              scale_x_continuous(limits = c(-1.2, 1.2) * max(abs(filtered_df$Diff)))            
          })
        } else {
          gg <- ggplot() +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(limits = c(-1.2, 1.2)) +
            scale_y_continuous(limits = c(0, 4)) + 
            annotate("text", x = 0, y = 3.5,
                     label = paste(combo$GeneSymbol,
                                   combo$CellLine,
                                   paste0(combo$Time, "h"),
                                   paste0(signif(10^combo$DoseMin, 2), "uM"),
                                   paste0(signif(10^combo$DoseMax, 2), "uM"),
                                   sep = " - "),
                     size = 3, hjust = 0.5)
        }
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
                                         text = "-Log10(P-Value)",
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
output$downloadGV <- renderUI({
  if (length(rv_GV$filtered_df_lst) > 0) {
    downloadButton('GVOutputFile', 'Download Data')
  }
})

# Export result to output for downloading
output$GVOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_filtered_df <- do.call('rbind', lapply(rv_GV$filtered_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, GeneSymbol, CellLine, Time, pert_name, pert_dose, 
                      Diff, pval) %>%
        `colnames<-`(c("Plot", "Gene", "Cell Line", "Time (Unit: h)", "Compound", "Dose (Unit: uM)",
                       "Log2FC", "P-Value"))
    }))
    write.table(all_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)
