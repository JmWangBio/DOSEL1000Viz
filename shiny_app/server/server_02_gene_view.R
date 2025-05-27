
# Create a reactive object with no values
rv_GV <- reactiveValues()
selected_GV_conditions <- reactiveVal(GV_default_data)

# Reactive flag to track whether the GV tab has been visited
visitedGV <- reactiveVal(TRUE)

# Update gene selection based on available data
observe({
  gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info")
  gene_choices <- gene_choices_df$symbol
  updateSelectInput(session, inputId = "geneGV", choices = gene_choices, selected = "PAX8")
})

# Update cell line selection(s) based on available data and key to rank compounds by
observeEvent(list(input$geneGV, input$rankGV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  req(input$geneGV, input$rankGV)
  if (input$rankGV != "p-value (L2FC)") {
    cell_line_choices_df <- dbGetQuery(con, "SELECT DISTINCT ci.* FROM condition c
                                          JOIN cell_info ci ON c.cell_id = ci.cell_id WHERE model = 'gam'")
  } else {
    cell_line_choices_df <- dbGetQuery(con, "SELECT DISTINCT ci.* FROM condition c
                                          JOIN cell_info ci ON c.cell_id = ci.cell_id")
  }
  cell_line_choices <- paste(cell_line_choices_df$primary_site, cell_line_choices_df$cell_id, sep = " : ")
  updateSelectizeInput(session, "cellLineGV", choices = cell_line_choices, selected = "kidney : HA1E")
})

# Update time selection(s) based on the selected cell line(s)
observeEvent(list(input$geneGV, input$rankGV, input$cellLineGV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  req(input$geneGV, input$rankGV, input$cellLineGV)
  selected_split_list <- strsplit(input$cellLineGV, " : ")
  rv_GV$selected_primary_sites <- sapply(selected_split_list, `[`, 1)
  rv_GV$selected_cell_ids <- sapply(selected_split_list, `[`, 2)
  if (input$rankGV != "p-value (L2FC)") {
    time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition 
                                             WHERE cell_id IN ('%s') AND model = 'gam';", 
                                               paste(rv_GV$selected_cell_ids, collapse = "','")))    
  } else {
    time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition 
                                             WHERE cell_id IN ('%s');", 
                                               paste(rv_GV$selected_cell_ids, collapse = "','")))
  }
  time_choices <- time_choices_df$pert_time
  updateSelectizeInput(session, "timeGV", choices = time_choices, selected = ifelse(6 %in% time_choices, 6, time_choices[1]))
})

# Determine the gene ID, the list of condition IDs, and the list of condition gene IDs based on the selected gene, cell line(s), and time(s)
observeEvent(list(input$geneGV, input$cellLineGV, input$timeGV), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  req(input$geneGV, rv_GV$selected_cell_ids, input$timeGV)
  selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info
                                              WHERE symbol = '%s'",
                                              input$geneGV))
  rv_GV$selected_gene <- selected_gene_df$gene[1]
  
  selected_cond_id_df <- dbGetQuery(con, sprintf("SELECT cond_id FROM condition
                                                 WHERE cell_id IN ('%s') AND pert_time IN (%s)",
                                                 paste(rv_GV$selected_cell_ids, collapse = "','"),
                                                 paste(input$timeGV, collapse = ",")))
  selected_cond_id_lst <- selected_cond_id_df$cond_id
  
  req(rv_GV$selected_gene)
  selected_cond_gene_id_df <- dbGetQuery(con, sprintf("SELECT cond_gene_id FROM condition_gene cg 
                                                      WHERE cg.cond_id IN (%s) AND
                                                      cg.gene = %s",
                                                      paste(selected_cond_id_lst, collapse = ","),
                                                      rv_GV$selected_gene))
  selected_cond_gene_id_lst <- selected_cond_gene_id_df$cond_gene_id
  rv_GV$selected_cond_gene_ids <- paste(selected_cond_gene_id_lst, collapse = ",")
})

# Choose whether to keep results from GAMs only based on the selected list of condition gene IDs, if ranked by p-value (L2FC)
output$gamOnlyGV_ui <- renderUI({
  req(rv_GV$selected_cond_gene_ids)
  
  if (input$rankGV == "p-value (L2FC)") {
    checkboxInput("gamOnlyGV", "Results from GAMs Only", value = TRUE)
  } else {
    NULL
  }
})

# Add selected condition to the list
observeEvent(input$addGVCombo, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  req(input$geneGV)
  new_combo <- data.frame(
    GeneSymbol = input$geneGV,
    Key = input$rankGV,
    CellLine = paste(input$cellLineGV, collapse = ","),
    Time = paste(paste0(input$timeGV, "h"), collapse = ","),
    GeneID = rv_GV$selected_gene,
    CondGeneIDs = rv_GV$selected_cond_gene_ids,
    Effect = input$effect,
    GAMOnly = NA,
    stringsAsFactors = FALSE
  )
  if (input$rankGV == "p-value (L2FC)") {
    new_combo$GAMOnly <- input$gamOnlyGV
  }
  
  current_combos <- selected_GV_conditions()
  
  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_GV_conditions(rbind(current_combos, new_combo))
  }
})

# Clear selected conditions
observeEvent(input$clearGVCombos, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  null_GV_conditions_df <- data.frame(
    GeneSymbol = character(),
    Key = character(),
    CellLine = character(),
    Time = character(),
    GeneID = numeric(),
    CondGeneIDs = character(),
    Effect = character(),
    GAMOnly = logical(),
    stringsAsFactors = FALSE
  )
  selected_GV_conditions(null_GV_conditions_df)
})

# Display selected conditions
output$GVComboList <- renderUI({
  combo_df <- selected_GV_conditions()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(c(combo_df$GeneSymbol[i],
                            combo_df$Key[i],
                            combo_df$CellLine[i],
                            combo_df$Time[i],
                            combo_df$Effect[i],
                            if (combo_df$Key[i] == "p-value (L2FC)")
                              ifelse(combo_df$GAMOnly[i], "GAMs Only", "All Models")),
                          collapse = " - ")),
          column(4, actionButton(inputId = paste0("remove_gv_", i),
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
  combo_df <- selected_GV_conditions()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_gv_", i)]], {
      showLoading()
      on.exit(hideLoading(), add = TRUE)
      
      updated_combos <- combo_df[-i, ]
      selected_GV_conditions(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent({input$plotGVBtn; visitedGV()}, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  isolate({
    combo_df <- selected_GV_conditions()
    combo_PE_df <- combo_df %>% filter(Key %in% c("Efficacy", "Potency"))
    combo_L2FC_df <- combo_df %>% filter(Key == "p-value (L2FC)")
    
    if (nrow(combo_PE_df) > 0) {
      filtered_PE_df_lst <- lapply(seq_len(nrow(combo_PE_df)), function(i) {
        combo <- combo_PE_df[i, ]
        
        query <- sprintf("SELECT i.*, p.*, c.cell_id, c.pert_time, c.batch_id, mi.moa FROM condition_gene cg 
                           JOIN condition c ON cg.cond_id = c.cond_id 
                           JOIN interaction i ON cg.cond_gene_id = i.cond_gene_id 
                           JOIN pert p ON c.broad_id = p.broad_id 
                           LEFT JOIN moa_info mi ON p.cpd_name = mi.cpd_name 
                           WHERE cg.cond_gene_id IN (%s)",
                         combo$CondGeneIDs)
        
        filtered_PE_df <- dbGetQuery(con, query)
        filtered_PE_df <- filtered_PE_df %>%
          mutate(moa = ifelse(is.na(moa), "Unknown", moa),
                 GeneSymbol = combo$GeneSymbol,
                 CellLine = cell_id,
                 Time = pert_time,
                 Plot = i)
        
        CI95q <- qnorm(0.975)
        filtered_PE_df <- filtered_PE_df %>%
          mutate(potency = 10^lpotency,
                 se_potency = 10^se_lpotency,
                 efficacy = 2^lefficacy * 100,
                 se_efficacy = 2^se_lefficacy,
                 text_label = paste0(cpd_name, " : ", broad_id, " (", batch_id, ")",
                                     "<br>MoA: ", moa,
                                     "<br>Cell: ", cell_id,
                                     "<br>Time: ", pert_time, "h",
                                     "<br>Pot: ", signif(potency, 2), "; (", signif(potency / (se_potency^CI95q), 2), ",", signif(potency * (se_potency^CI95q), 2), ")",
                                     "<br>Eff: ", signif(efficacy, 2), "; (", signif(efficacy / (se_efficacy^CI95q), 2), ",", signif(efficacy * (se_efficacy^CI95q), 2), ")")) %>%
          filter(!is.na(potency) & !is.na(se_potency) & !is.na(efficacy) & !is.na(se_efficacy) &
                   is.finite(potency) & is.finite(se_potency) & is.finite(efficacy) & is.finite(se_efficacy))
        
        if (combo$Effect == "inhibition") {
          filtered_PE_df <- filtered_PE_df %>%
            mutate(color = ifelse(efficacy * (se_efficacy^CI95q) < 100, "red", "grey"))
        } else {
          filtered_PE_df <- filtered_PE_df %>%
            mutate(color = ifelse(efficacy / (se_efficacy^CI95q) > 100, "red", "grey"))
        }
        
        if (combo$Key == "Efficacy") {
          if (combo$Effect == "inhibition") {
            filtered_PE_df <- filtered_PE_df %>%
              arrange(lefficacy) %>%
              mutate(Rank = row_number())
          } else {
            filtered_PE_df <- filtered_PE_df %>%
              arrange(desc(lefficacy)) %>%
              mutate(Rank = row_number())
          }
        } else {
          filtered_PE_df <- filtered_PE_df %>%
            arrange(lpotency) %>%
            mutate(Rank = row_number())
        }
      })
    } else {
      filtered_PE_df_lst <- list()
    }
    
    if (nrow(combo_L2FC_df) > 0) {
      filtered_L2FC_df_lst <- lapply(seq_len(nrow(combo_L2FC_df)), function(i) {
        combo <- combo_df[i, ]
        
        query <- sprintf("SELECT t.*, p.*, c.cell_id, c.pert_time, c.batch_id, mi.moa FROM condition_gene cg
                         JOIN condition c ON cg.cond_id = c.cond_id
                         JOIN test t ON cg.cond_gene_id = t.cond_gene_id
                         JOIN pert p ON c.broad_id = p.broad_id
                         LEFT JOIN moa_info mi ON p.cpd_name = mi.cpd_name
                         WHERE t.cond_gene_id IN (%s)",
                         combo$CondGeneIDs)
        
        if (!is.na(combo$GAMOnly) & combo$GAMOnly) {
          query <- paste(query, "AND c.model = 'gam'")
        }
        
        filtered_L2FC_df <- dbGetQuery(con, query)
        filtered_L2FC_df <- filtered_L2FC_df %>%
          mutate(moa = ifelse(is.na(moa), "Unknown", moa),
                 GeneSymbol = combo$GeneSymbol,
                 CellLine = cell_id,
                 Time = pert_time,
                 Plot = i)
        
        filtered_L2FC_df <- filtered_L2FC_df %>%
          mutate(text_label = paste0(cpd_name, " : ", broad_id, " (", batch_id, ")",
                                     "<br>MoA: ", moa,
                                     "<br>Cell: ", cell_id,
                                     "<br>Time: ", pert_time, "h",
                                     "<br>Dose: ", signif(pert_dose, 2), "uM",
                                     "<br>Log2FC: ", round(Diff, 2),
                                     "<br>P-Value: ", signif(pval, 2))) %>%
          arrange(pval) %>%
          mutate(Rank = row_number())
      })
    } else {
      filtered_L2FC_df_lst <- list()
    }
    
    rv_GV$combo_PE_df <- combo_PE_df
    rv_GV$combo_L2FC_df <- combo_L2FC_df
    rv_GV$filtered_PE_df_lst <- filtered_PE_df_lst
    rv_GV$filtered_L2FC_df_lst <- filtered_L2FC_df_lst
  })
})

# Generate and display the interactive Gene View PE plot
output$GVPEPlot <- renderPlotly({
  req(rv_GV$filtered_PE_df_lst)
  isolate({
    if (nrow(rv_GV$combo_PE_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_GV$filtered_PE_df_lst)), function(i) {
        filtered_PE_df <- rv_GV$filtered_PE_df_lst[[i]] %>%
          slice_head(n = 1000)
        combo <- rv_GV$combo_PE_df[i, ]
        CI95q <- qnorm(0.975)
        
        if (nrow(filtered_PE_df) > 0) {
          suppressWarnings({
              gg <- ggplot(filtered_PE_df,
                           aes(x = potency, y = efficacy)) +
                geom_point(aes(text = text_label, color = color), size = 2, alpha = 0.5) +
                scale_color_manual(values = c("grey" = "grey", "red" = "red")) +
                annotate("text",
                         x = sqrt(min(filtered_PE_df$potency) * max(filtered_PE_df$potency)),
                         y = max(filtered_PE_df$efficacy) * 1.2,
                         label = paste(combo$GeneSymbol,
                                       sub("(.*?),.*", "\\1, ...", combo$CellLine),
                                       combo$Time,
                                       combo$Effect,
                                       sep = " - "),
                         size = 3, hjust = 0.5) +
                theme_minimal() +
                theme(legend.position = "none") +
                scale_x_continuous(trans = "log10",
                                   limits = c(min(filtered_PE_df$potency) / 1.2,
                                              max(filtered_PE_df$potency) * 1.2)) +
                scale_y_continuous(trans = "log10",
                                   limits = c(min(filtered_PE_df$efficacy) / 1.2,
                                              max(filtered_PE_df$efficacy) * 1.2))
          })
        } else {
          gg <- ggplot() +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(trans = "log10",
                               limits = c(0.001, 1000)) +
            scale_y_continuous(trans = "log10",
                               limits = c(0.01, 100)) +
            annotate("text", x = 0, y = 3.5,
                     label = paste(combo$GeneSymbol,
                                   sub("(.*?),.*", "\\1, ...", combo$CellLine),
                                   combo$Time,
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
                                         text = "Potency (Unit: uM)",
                                         showarrow = FALSE, xref = 'paper', yref = 'paper',
                                         font = list(size = 12)),
                                    list(x = -0.15 * 400 / total_width,
                                         y = 0.5,
                                         text = "Efficacy (% DMSO)",
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

# Generate and display the interactive Gene View L2FC plot
output$GVL2FCPlot <- renderPlotly({
  req(rv_GV$filtered_L2FC_df_lst)
  isolate({
    if (nrow(rv_GV$combo_L2FC_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_GV$filtered_L2FC_df_lst)), function(i) {
        filtered_L2FC_df <- rv_GV$filtered_L2FC_df_lst[[i]] %>%
          slice_head(n = 1000)
        combo <- rv_GV$combo_L2FC_df[i, ]
        
        if (nrow(filtered_L2FC_df) > 0) {
          suppressWarnings({
              gg <- ggplot(filtered_L2FC_df,
                           aes(x = Diff, y = -log10(pval)), color = "grey") +
                geom_point(aes(text = text_label), size = 2, alpha = 0.5) +
                geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
                annotate("text", x = 0, y = max(max(-log10(filtered_L2FC_df$pval)), -log10(0.05)) - 0.2,
                         label =  paste(combo$GeneSymbol,
                                        sub("(.*?),.*", "\\1, ...", combo$CellLine),
                                        combo$Time,
                                        sep = " - "),
                         size = 3, hjust = 0.5) +
                theme_minimal() +
                theme(legend.position = "none") +
                scale_x_continuous(limits = c(-1.2, 1.2) * max(abs(filtered_L2FC_df$Diff)))
          })
        } else {
          gg <- ggplot() +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(limits = c(-1.2, 1.2)) +
            scale_y_continuous(limits = c(0, 4)) +
            annotate("text", x = 0, y = 3.5,
                     label = paste(combo$GeneSymbol,
                                   sub("(.*?),.*", "\\1, ...", combo$CellLine),
                                   combo$Time,
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

# Activate download button for PE output
output$downloadGVPE <- renderUI({
  if (length(rv_GV$filtered_PE_df_lst) > 0) {
    downloadButton('GVPEOutputFile', 'Download Pot. Eff. Data')
  }
})

# Activate download button for L2FC output
output$downloadGVL2FC <- renderUI({
  if (length(rv_GV$filtered_L2FC_df_lst) > 0) {
    downloadButton('GVL2FCOutputFile', 'Download L2FC Data')
  }
})

# Export result to PE output for downloading
output$GVPEOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_PE_filtered_df <- do.call('rbind', lapply(rv_GV$filtered_PE_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, GeneSymbol, CellLine, Time, cpd_name, broad_id, batch_id, moa, potency, se_potency,
                      efficacy, se_efficacy, Rank) %>%
        `colnames<-`(c("Plot", "Gene", "Cell Line", "Time (Unit: h)", "Compound", "BROAD ID", "Batch", "MoA", "Potency (Unit: uM)", "SE Potency",
                       "Efficacy (% DMSO)", "SE Efficacy", "Rank"))
    }))
    write.table(all_PE_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)

# Export result to L2FC output for downloading
output$GVL2FCOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_L2FC_filtered_df <- do.call('rbind', lapply(rv_GV$filtered_L2FC_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, GeneSymbol, CellLine, Time, cpd_name, broad_id, batch_id, moa, pert_dose,
                      Diff, pval, Rank) %>%
        `colnames<-`(c("Plot", "Gene", "Cell Line", "Time (Unit: h)", "Compound", "BROAD ID", "Batch", "MoA", "Dose (Unit: uM)",
                       "Log2FC", "P-Value", "Rank"))
    }))
    write.table(all_L2FC_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)

# Export PE result to data table
output$GVPETable <- renderDataTable({
  # Concatenate the relevant data
  all_PE_filtered_df <- do.call('rbind', lapply(rv_GV$filtered_PE_df_lst, function(x) {
    x %>%
      dplyr::select(Plot, GeneSymbol, CellLine, Time, cpd_name, broad_id, batch_id, moa, potency, se_potency,
                    efficacy, se_efficacy, Rank) %>%
      mutate(potency = signif(potency, 2),
             se_potency = signif(se_potency, 2),
             efficacy = signif(efficacy, 2),
             se_efficacy = signif(se_efficacy, 2)) %>%
      `colnames<-`(c("Plot", "Gene", "Cell Line", "Time (Unit: h)", "Compound", "BROAD ID", "Batch", "MoA", "Potency (Unit: uM)", "SE Potency",
                     "Efficacy (% DMSO)", "SE Efficacy", "Rank"))
  }))
}, options = list(pageLength = 10, scrollX = TRUE),
rownames = FALSE)

# Export L2FC result to data table
output$GVL2FCTable <- renderDataTable({
  # Concatenate the relevant data
  all_L2FC_filtered_df <- do.call('rbind', lapply(rv_GV$filtered_L2FC_df_lst, function(x) {
    x %>%
      dplyr::select(Plot, GeneSymbol, CellLine, Time, cpd_name, broad_id, batch_id, moa, pert_dose,
                    Diff, pval, Rank) %>%
      mutate(pert_dose = signif(pert_dose, 2),
             Diff = round(Diff, 2),
             pval = signif(pval, 2)) %>%
      `colnames<-`(c("Plot", "Gene", "Cell Line", "Time (Unit: h)", "Compound", "BROAD ID", "Batch", "MoA", "Dose (Unit: uM)",
                     "Log2FC", "P-Value", "Rank"))
  }))
}, options = list(pageLength = 10, scrollX = TRUE),
rownames = FALSE)
