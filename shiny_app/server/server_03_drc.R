
# Create a reactive object with no values
rv_DRC <- reactiveValues()

# Reactive value to store selected efficacy vs potency combinations
selected_DRC_combinations <- reactiveVal(data.frame(
  Compound = character(),
  GeneSymbol = character(),
  CellLine = character(),
  Time = character(),
  PertID = numeric(),
  GeneID = numeric(),
  stringsAsFactors = FALSE
))

# Update compound selection based on available data
observe({
  compound_choices_df <- dbGetQuery(con, "SELECT pert_name FROM pert p 
                                         JOIN combination c ON p.pert_id = c.pert_id 
                                         WHERE c.model = 'gam'")
  compound_choices <- compound_choices_df$pert_name
  updateSelectizeInput(session, inputId = "compoundDRC", 
                       choices = compound_choices,
                       server = TRUE, selected = "bazedoxifene : BRD-K90195324 (REP.A026)")
})

# Update gene selection based on available data
observeEvent(input$compoundDRC, {
  req(input$compoundDRC)
  selected_pert_id_df <- dbGetQuery(con, sprintf("SELECT pert_id FROM pert
                                                 WHERE pert_name = '%s';", 
                                                 input$compoundDRC))
  rv_DRC$selected_pert_id <- selected_pert_id_df$pert_id[1]
  gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info")
  gene_choices <- gene_choices_df$symbol
  updateSelectInput(session, inputId = "geneDRC", choices = gene_choices, selected = "CTSD")
})

# Update cell line selection based on the selected compound
observeEvent(list(input$compoundDRC, input$geneDRC), {
  req(input$compoundDRC, input$geneDRC, rv_DRC$selected_pert_id)
  selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info 
                                              WHERE symbol = '%s';",
                                              input$geneDRC))
  rv_DRC$selected_gene <- selected_gene_df$gene[1]
  
  cell_line_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT cell_id FROM combination 
                                                  WHERE pert_id = %s AND model = 'gam';", 
                                                  rv_DRC$selected_pert_id))
  cell_line_choices <- cell_line_choices_df$cell_id
  updateSelectInput(session, "cellLineDRC", choices = cell_line_choices, selected = ifelse("MCF7" %in% cell_line_choices, "MCF7", cell_line_choices[1]))
})

# Update time selection based on the selected compound and cell line
observeEvent(list(input$compoundDRC, input$geneDRC, input$cellLineDRC), {
  req(input$compoundDRC, input$geneDRC, input$cellLineDRC, 
      rv_DRC$selected_pert_id, rv_DRC$selected_gene)
  time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM combination
                                             WHERE pert_id = %s AND cell_id = '%s'",
                                             rv_DRC$selected_pert_id,
                                             input$cellLineDRC))
  time_choices <- time_choices_df$pert_time
  updateSelectInput(session, "timeDRC", choices = time_choices)
})

# Add selected combination to the list
observeEvent(input$addDRCCombo, {
  new_combo <- data.frame(
    Compound = input$compoundDRC,
    GeneSymbol = input$geneDRC,
    CellLine = input$cellLineDRC,
    Time = input$timeDRC,
    PertID = rv_DRC$selected_pert_id,
    GeneID = rv_DRC$selected_gene,
    stringsAsFactors = FALSE
  )
  
  current_combos <- selected_DRC_combinations()
  
  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_DRC_combinations(rbind(current_combos, new_combo))
  }
})

# Clear selected DRC combinations
observeEvent(input$clearDRCCombos, {
  selected_DRC_combinations(data.frame(
    Compound = character(),
    GeneSymbol = character(),
    CellLine = character(),
    Time = character(),
    PertID = numeric(),
    GeneID = numeric(),
    stringsAsFactors = FALSE
  ))
})

# Display selected combinations
output$DRCComboList <- renderUI({
  combo_df <- selected_DRC_combinations()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(combo_df$Compound[i],
                          combo_df$CellLine[i],
                          paste0(combo_df$Time[i], "h"),
                          combo_df$GeneSymbol[i],
                          sep = " - ")),
          column(4, actionButton(inputId = paste0("remove_drc_", i),
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
  combo_df <- selected_DRC_combinations()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_drc_", i)]], {
      updated_combos <- combo_df[-i, ]
      selected_DRC_combinations(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent(input$plotDRCBtn, {
  isolate({
    combo_df <- selected_DRC_combinations()
    if (nrow(combo_df) > 0) {
      filtered_df_lst <- lapply(seq_len(nrow(combo_df)), function(i) {
        combo <- combo_df[i, ]
        
        # Get comb_gene_id and pert_time
        query_1 <- sprintf("SELECT cg.comb_gene_id, c.pert_time FROM combination c
                           JOIN combination_gene cg ON c.comb_id = cg.comb_id
                           WHERE c.pert_id = %s AND 
                           c.cell_id = '%s'",
                           combo$PertID,
                           combo$CellLine)
        filtered_df_1 <- dbGetQuery(con, query_1)
        selected_comb_gene_ids <- filtered_df_1$comb_gene_id
        
        query_2 <- sprintf("SELECT cg.comb_gene_id FROM combination_gene cg 
                           WHERE cg.comb_gene_id IN (%s) AND 
                           cg.gene = %s;",
                           paste(selected_comb_gene_ids, collapse = ","),
                           combo$GeneID)
        filtered_df_2 <- dbGetQuery(con, query_2)
        selected_comb_gene_ids_2 <- filtered_df_2$comb_gene_id
        
        # Get dose response data
        query_3 <- sprintf("SELECT * FROM drc 
                           WHERE comb_gene_id IN (%s)", 
                           paste(selected_comb_gene_ids_2, collapse = ","))
        filtered_df_3 <- dbGetQuery(con, query_3)
        filtered_df_3 <- filtered_df_3 %>%
          mutate(comb_gene_id = as.numeric(comb_gene_id))
        selected_plate_ids <- unique(filtered_df_3$plate_id)
        
        # Get DMSO data
        query_4 <- sprintf("SELECT * FROM dmso 
                           WHERE plate_id IN (%s) 
                           AND gene = %s",
                           paste(selected_plate_ids, collapse = ","),
                           combo$GeneID)
        filtered_df_4 <- dbGetQuery(con, query_4)
        
        # Get efficacy, potency, and pseudo concentration
        query_5 <- sprintf("SELECT * FROM interaction 
                           WHERE comb_gene_id IN (%s)", 
                           paste(selected_comb_gene_ids_2, collapse = ","))
        filtered_df_5 <- dbGetQuery(con, query_5)
        filtered_df_5 <- filtered_df_5 %>%
          mutate(comb_gene_id = as.numeric(comb_gene_id),
                 potency = 10^lpotency,
                 se_potency = 10^se_lpotency,
                 efficacy = 2^lefficacy * 100,
                 se_efficacy = 2^se_lefficacy)
        
        # Prepare DRC data for concatenation
        filtered_DRC_m <- filtered_df_1 %>%
          inner_join(filtered_df_2, by = "comb_gene_id") %>%
          inner_join(filtered_df_3, by = "comb_gene_id") %>%
          inner_join(filtered_df_5, by = "comb_gene_id") %>%
          dplyr::select(pert_time, pert_dose, 
                        abundance, pseudo_conc,
                        potency, se_potency,
                        efficacy, se_efficacy,
                        plate_id)
        
        # Prepare DMSO data for concatenation
        filtered_DMSO_m <- filtered_df_4 %>% 
          inner_join(filtered_DRC_m %>% 
                       dplyr::select(-c(pert_dose, abundance)) %>%
                       distinct(), 
                     by = "plate_id") %>%
          mutate(pert_dose = 0) %>%
          dplyr::select(pert_time, pert_dose, 
                        abundance, pseudo_conc,
                        potency, se_potency,
                        efficacy, se_efficacy,
                        plate_id)
        
        # Concatenate data
        filtered_df <- filtered_DRC_m %>%
          rbind(filtered_DMSO_m) %>%
          mutate(Plot = i, 
                 Compound = combo$Compound,
                 GeneSymbol = combo$GeneSymbol,
                 CellLine = combo$CellLine)
      })
    } else {
      filtered_df_lst <- list()
    }
    rv_DRC$filtered_df_lst <- filtered_df_lst
  })
})

# Generate and display the interactive DRC plot
output$DRCPlot <- renderPlotly({
  req(input$plotDRCBtn, rv_DRC$filtered_df_lst)
  isolate({
    combo_df <- selected_DRC_combinations()
    if (nrow(combo_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_DRC$filtered_df_lst)), function(i) {
        filtered_df <- rv_DRC$filtered_df_lst[[i]]
        combo <- combo_df[i, ]
        
        if (nrow(filtered_df) > 0) {
          # Fit GAM
          pred <- fit_GAM(filtered_df, combo$Time)
          
          # Keep selected time
          filtered_df_m <- filtered_df %>%
            filter(pert_time == combo$Time)
          CI95q <- qnorm(0.975)
          
          # Make plot
          gg <- ggplot() +
            geom_point(data = filtered_df_m,
                       aes(x = pert_dose + pseudo_conc,
                           y = abundance),
                       size = 0.5) +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(trans = "log10") +
            scale_y_continuous() +
            annotate("text",
                     x = sqrt(min(filtered_df_m$pert_dose + filtered_df_m$pseudo_conc) * 
                                max(filtered_df_m$pert_dose + filtered_df_m$pseudo_conc)),
                     y = max(filtered_df_m$abundance) + 1,
                     label = paste(paste0(combo$Compound, "\n"),
                                   combo$GeneSymbol,
                                   combo$CellLine,
                                   paste0(combo$Time, "h"),
                                   sep = " - "),
                     size = 3, hjust = 0.5) +
            annotate("text",
                     x = sqrt(min(filtered_df_m$pert_dose + filtered_df_m$pseudo_conc)^0.5 * 
                                max(filtered_df_m$pert_dose + filtered_df_m$pseudo_conc)^1.5),
                     y = max(filtered_df_m$abundance),
                     label = sprintf("Potency: %s; (%s, %s)\nEfficacy: %s%%; (%s%%, %s%%)", 
                                     signif(filtered_df_m$potency[1], 2),
                                     signif(filtered_df_m$potency[1] / (filtered_df_m$se_potency[1]^CI95q), 2),
                                     signif(filtered_df_m$potency[1] * (filtered_df_m$se_potency[1]^CI95q), 2),
                                     signif(filtered_df_m$efficacy[1], 2),
                                     signif(filtered_df_m$efficacy[1] / (filtered_df_m$se_efficacy[1]^CI95q), 2),
                                     signif(filtered_df_m$efficacy[1] * (filtered_df_m$se_efficacy[1]^CI95q), 2)),
                     size = 2.5,
                     hjust = 0.5)
          
          # Add trend line if fitting successful
          if (!is.null(pred)) {
            gg <- gg +
              geom_line(data = pred,
                        aes(x = exp(logConc),
                            y = fit),
                        linewidth = 0.5,
                        color = "lightblue")
          }
        } else {
          gg <- ggplot() +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(trans = "log10",
                               limits = c(0.1, 10)) +
            scale_y_continuous(limits = c(0, 5)) + 
            annotate("text", x = 1, y = 4.5,
                     label = paste(paste0(combo$Compound, "\n"),
                                   combo$GeneSymbol,
                                   combo$CellLine,
                                   paste0(combo$Time, "h"),
                                   sep = " - "),
                     size = 3, hjust = 0.5)
        }
        ggplotly(gg, tooltip = NULL)
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
                                         text = "Dose (Unit: uM)",
                                         showarrow = FALSE, xref = 'paper', yref = 'paper',
                                         font = list(size = 12)),
                                    list(x = -0.15 * 400 / total_width,
                                         y = 0.5,
                                         text = "Abundance (Log2)",
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
output$downloadDRC <- renderUI({
  if (length(rv_DRC$filtered_df_lst) > 0) {
    downloadButton('DRCOutputFile', 'Download Data')
  }
})

# Export result to output for downloading
output$DRCOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_filtered_df <- do.call('rbind', lapply(rv_DRC$filtered_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, Compound, GeneSymbol, CellLine, pert_time, pert_dose, abundance,
                      potency, se_potency, efficacy, se_efficacy, pseudo_conc) %>%
        `colnames<-`(c("Plot", "Compound", "Gene", "Cell Line", "Time (Unit: h)", "Dose (Unit: uM)", "Abundance (Log2)",
                       "Potency (Unit: uM)", "SE Potency", "Efficacy (% DMSO)", "SE Efficacy", "Pseudo Dose (Unit: uM)"))
    }))
    write.table(all_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)
