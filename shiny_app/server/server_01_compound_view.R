
# Create a reactive object with no values
rv_CV <- reactiveValues()
selected_CV_conditions <- reactiveVal(CV_default_data)

# Reactive flag to track whether the CV tab has been visited
visitedCV <- reactiveVal(TRUE)

# Update compound selection based on available data
observe({
  compound_choices_df <- dbGetQuery(con, "SELECT broad_id, cpd_name FROM pert")
  compound_choices <- paste(compound_choices_df$cpd_name, compound_choices_df$broad_id, sep = " : ")
  updateSelectizeInput(session, inputId = "compoundCV", 
                       choices = compound_choices,
                       server = TRUE, selected = "clomifene : BRD-K29950728")
})

# Update cell line selection based on the selected compound
observeEvent(input$compoundCV, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(input$compoundCV)
  rv_CV$selected_cpd_name <- strsplit(input$compoundCV, " : ")[[1]][1]
  rv_CV$selected_broad_id <- strsplit(input$compoundCV, " : ")[[1]][2]
  cell_line_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT ci.* FROM condition c 
                                                  JOIN cell_info ci ON c.cell_id = ci.cell_id 
                                                  WHERE broad_id = '%s'",
                                                  rv_CV$selected_broad_id))
  cell_line_choices <- paste(cell_line_choices_df$primary_site, cell_line_choices_df$cell_id, sep = " : ")
  updateSelectInput(session, "cellLineCV", choices = cell_line_choices, selected = ifelse("breast : MCF7" %in% cell_line_choices, "breast : MCF7", cell_line_choices[1]))
})

# Update time selection based on the selected compound and cell line
observeEvent(list(input$compoundCV, input$cellLineCV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(rv_CV$selected_broad_id, input$cellLineCV)
  rv_CV$selected_primary_site <- strsplit(input$cellLineCV, " : ")[[1]][1]
  rv_CV$selected_cell_id <- strsplit(input$cellLineCV, " : ")[[1]][2]
  time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition
                                             WHERE broad_id = '%s' AND cell_id = '%s'",
                                             rv_CV$selected_broad_id,
                                             rv_CV$selected_cell_id))
  time_choices <- time_choices_df$pert_time
  updateSelectInput(session, "timeCV", choices = time_choices, selected = ifelse(24 %in% time_choices, 24, time_choices[1]))
})

# Update batch selection based on the selected compound, cell line, and time
observeEvent(list(input$compoundCV, input$cellLineCV, input$timeCV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(rv_CV$selected_broad_id, rv_CV$selected_cell_id, input$timeCV)
  batch_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT batch_id FROM condition
                                             WHERE broad_id = '%s' AND cell_id = '%s' AND pert_time = %s",
                                             rv_CV$selected_broad_id,
                                             rv_CV$selected_cell_id,
                                             input$timeCV))
  batch_choices <- batch_choices_df$batch_id
  updateSelectInput(session, "batchCV", choices = batch_choices, selected = ifelse("REP.A009" %in% batch_choices, "REP.A009", batch_choices[1]))
})

# Determine the condition ID based on the selected compound, cell line, time, and batch
# Update dose selection based on the selected condition ID
observeEvent(list(input$compoundCV, input$cellLineCV, input$timeCV, input$batchCV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(rv_CV$selected_broad_id, rv_CV$selected_cell_id, input$timeCV, input$batchCV)
  selected_cond_id_df <- dbGetQuery(con, sprintf("SELECT cond_id FROM condition
                                                 WHERE broad_id = '%s' AND
                                                 cell_id = '%s' AND
                                                 pert_time = %s AND
                                                 batch_id = '%s'",
                                                 rv_CV$selected_broad_id,
                                                 rv_CV$selected_cell_id,
                                                 input$timeCV,
                                                 input$batchCV))
  rv_CV$selected_cond_id <- selected_cond_id_df$cond_id[1]

  req(rv_CV$selected_cond_id)
  dose_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_dose FROM condition_gene cg
                                             JOIN condition c ON cg.cond_id = c.cond_id
                                             JOIN test t ON cg.cond_gene_id = t.cond_gene_id
                                             WHERE c.cond_id = %s",
                                             rv_CV$selected_cond_id))
  dose_choices <- dose_choices_df$pert_dose
  if (is.numeric(dose_choices)) {
    names(dose_choices) <- signif(dose_choices, 2)
  }
  updateSelectInput(session, "doseCV", choices = dose_choices)
})

# Add selected condition to the list
observeEvent(input$addCVCombo, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(input$compoundCV)
  new_combo <- data.frame(
    Compound = input$compoundCV,
    CellLine = input$cellLineCV,
    Time = input$timeCV,
    Batch = input$batchCV,
    Dose = input$doseCV,
    CondID = rv_CV$selected_cond_id,
    stringsAsFactors = FALSE
  )

  current_combos <- selected_CV_conditions()

  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_CV_conditions(rbind(current_combos, new_combo))
  }
})

# Clear selected conditions
observeEvent(input$clearCVCombos, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  selected_CV_conditions(data.frame(
    Compound = character(),
    CellLine = character(),
    Time = character(),
    Batch = character(),
    Dose = character(),
    CondID = numeric(),
    stringsAsFactors = FALSE
  ))
})

# Display selected conditions
output$CVComboList <- renderUI({
  combo_df <- selected_CV_conditions()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(combo_df$Compound[i],
                          combo_df$CellLine[i],
                          paste0(combo_df$Time[i], "h"),
                          combo_df$Batch[i],
                          paste0(signif(as.numeric(combo_df$Dose[i]), 2), "uM"),
                          sep = " - ")),
          column(4, actionButton(inputId = paste0("remove_cv_", i),
                                 label = "Remove",
                                 class = "btn-danger"))
        )
      })
    )
  } else {
    "No conditions added yet."
  }
})

# Remove selected condition from the list
observe({
  combo_df <- selected_CV_conditions()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_cv_", i)]], {
      showLoading()
      on.exit(hideLoading(), add = TRUE)

      updated_combos <- combo_df[-i, ]
      selected_CV_conditions(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent({input$plotCVBtn; visitedCV()}, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  isolate({
    combo_df <- selected_CV_conditions()
    if (nrow(combo_df) > 0) {
      filtered_df_lst <- lapply(seq_len(nrow(combo_df)), function(i) {
        combo <- combo_df[i, ]

        selected_cond_gene_id_df <- dbGetQuery(con, sprintf("SELECT cond_gene_id FROM condition_gene
                                                            WHERE cond_id = %s",
                                                            combo$CondID))
        selected_cond_gene_id_lst <- selected_cond_gene_id_df$cond_gene_id

        query <- sprintf("SELECT t.*, gi.symbol FROM condition_gene cg
                         JOIN test t ON t.cond_gene_id = cg.cond_gene_id
                         JOIN gene_info gi ON cg.gene = gi.gene
                         WHERE t.cond_gene_id IN (%s) AND t.pert_dose > %s AND t.pert_dose < %s",
                         paste(selected_cond_gene_id_lst, collapse = ","),
                         as.numeric(combo$Dose) * 0.98, as.numeric(combo$Dose) * 1.02)
        filtered_df <- dbGetQuery(con, query)
        filtered_df <- filtered_df %>%
          mutate(adj.pval = p.adjust(pval, method = "fdr"),
                 color = ifelse(adj.pval < 0.05 & abs(Diff) > 1, "red", "grey"),
                 text_label = paste(symbol, "<br>Log2FC:", round(Diff, 2),
                                    "<br>Adj. P-Value:", signif(adj.pval, 2)),
                 Compound = combo$Compound,
                 CellLine = combo$CellLine,
                 Time = combo$Time,
                 Batch = combo$Batch,
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
  req(rv_CV$filtered_df_lst)
  isolate({
    combo_df <- selected_CV_conditions()
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
            geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") +
            annotate("text", x = 0, y = max(c(-log10(filtered_df$adj.pval), -log10(0.05))) + 1,
                     label = paste0(combo$Compound, " - \n",
                                    paste(combo$CellLine,
                                          paste0(combo$Time, "h"),
                                          combo$Batch,
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
        dplyr::select(Plot, Compound, CellLine, Time, Batch, Dose, symbol,
                      Diff, pval, adj.pval) %>%
        `colnames<-`(c("Plot", "Compound", "Cell Line", "Time (Unit: h)", "Batch", "Dose (Unit: uM)", "Gene",
                       "Log2FC", "P-Value", "Adj. P-Value"))
    }))
    write.table(all_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)

# Export result to data table
output$CVTable <- renderDataTable({
  # Concatenate the relevant data
  all_filtered_df <- do.call('rbind', lapply(rv_CV$filtered_df_lst, function(x) {
    x %>%
      dplyr::select(Plot, Compound, CellLine, Time, Batch, Dose, symbol,
                    Diff, pval, adj.pval) %>%
      mutate(Dose = signif(as.numeric(Dose), 2),
             Diff = round(Diff, 2),
             pval = signif(pval, 2),
             adj.pval = signif(adj.pval, 2)) %>%
      `colnames<-`(c("Plot", "Compound", "Cell Line", "Time (Unit: h)", "Batch", "Dose (Unit: uM)", "Gene",
                     "Log2FC", "P-Value", "Adj. P-Value"))
  }))
}, options = list(pageLength = 10, scrollX = TRUE),
rownames = FALSE)
