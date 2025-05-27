
# Create a reactive object with no values
rv_Eff <- reactiveValues()

# Reactive value to store selected efficacy vs potency conditions
selected_Eff_conditions <- reactiveVal(Eff_default_data)

# Reactive flag to track whether the Eff tab has been visited
visitedEff <- reactiveVal(TRUE)

# Update multipleInput based on the choice of which parameter allows multiple inputs
observeEvent(input$multipleInputChoice, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  if (input$multipleInputChoice == "Compound") {
    compound_choices_df <- dbGetQuery(con, "SELECT DISTINCT p.cpd_name, c.broad_batch_id FROM pert p 
                                           JOIN condition c ON p.broad_id = c.broad_id 
                                           WHERE c.model = 'gam';")
    compound_choices <- paste0(compound_choices_df$cpd_name, " : ", compound_choices_df$broad_batch_id)
    updateSelectizeInput(session,
                         inputId = "multipleInput",
                         label = "Select Compounds (Max: 10):",
                         choices = compound_choices,
                         server = TRUE, selected = c("toremifene : BRD-K51350053 (REP.A007)", "clomifene : BRD-K29950728 (REP.A009)", "bazedoxifene : BRD-K90195324 (REP.A026)"))
  } else if (input$multipleInputChoice == "Gene") {
    gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info;")
    gene_choices <- gene_choices_df$symbol
    updateSelectizeInput(session, 
                         inputId = "multipleInput", 
                         label = "Select Genes (Max 10):",
                         choices = NULL,
                         server = TRUE)
    updateSelectizeInput(session, 
                         inputId = "multipleInput",
                         label = "Select Genes (Max 10):",
                         choices = gene_choices,
                         server = TRUE, selected = NULL)
  } else if (input$multipleInputChoice == "Cell Line") {
    cell_line_choices_df <- dbGetQuery(con, "SELECT DISTINCT ci.* FROM condition c JOIN cell_info ci ON c.cell_id = ci.cell_id 
                                            WHERE model = 'gam';")
    cell_line_choices <- paste0(cell_line_choices_df$primary_site, " : ", cell_line_choices_df$cell_id)
    updateSelectizeInput(session,
                         inputId = "multipleInput",
                         label = "Select Cell Lines (Max: 10):",
                         choices = NULL,
                         server = TRUE)
    updateSelectizeInput(session, 
                         inputId = "multipleInput",
                         label = "Select Cell Lines (Max: 10):",
                         choices = cell_line_choices,
                         server = TRUE, selected = NULL)
  }
})

# Update singleInput1 based on multipleInputChoice and multipleInput
observeEvent(list(input$multipleInputChoice, input$multipleInput), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)
  
  if (is.null(input$multipleInput)) {
    if (input$multipleInputChoice == "Compound") {
      updateSelectizeInput(session,
                           inputId = "singleInput1",
                           label = "Select Gene:",
                           choices = NULL,
                           server = TRUE)
    } else {
      updateSelectizeInput(session,
                           inputId = "singleInput1",
                           label = "Select Compound:",
                           choices = NULL,
                           server = TRUE)
    }
  } else {
    if (input$multipleInputChoice == "Compound") {
      selected_broad_batch_ids <- sub(".*:\\s*", "", input$multipleInput)
      rv_Eff$selected_broad_batch_ids <- selected_broad_batch_ids
      
      gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info;")
      gene_choices <- gene_choices_df$symbol
      updateSelectizeInput(session,
                           inputId = "singleInput1",
                           label = "Select Gene:",
                           choices = gene_choices,
                           server = TRUE, selected = "CTSD")
    } else if (input$multipleInputChoice == "Gene") {
      selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info 
                                                  WHERE symbol IN ('%s');",
                                                  paste(input$multipleInput,
                                                        collapse = "','")))
      rv_Eff$selected_genes <- selected_gene_df$gene
      
      compound_choices_df <- dbGetQuery(con, "SELECT DISTINCT p.cpd_name, c.broad_batch_id from pert p 
                                             JOIN condition c ON p.broad_id = c.broad_id 
                                             WHERE c.model = 'gam';")
      compound_choices <- paste0(compound_choices_df$cpd_name, " : ", compound_choices_df$broad_batch_id)
      updateSelectizeInput(session,
                           inputId = "singleInput1",
                           label = "Select Compound:",
                           choices = compound_choices,
                           server = TRUE)
    } else if (input$multipleInputChoice == "Cell Line") {
      selected_cell_ids <- sub(".*:\\s*", "", input$multipleInput)
      rv_Eff$selected_cell_ids <- selected_cell_ids
      
      compound_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT p.cpd_name, c.broad_batch_id FROM pert p 
                                                     JOIN condition c ON p.broad_id = c.broad_id 
                                                     WHERE c.cell_id IN ('%s') AND c.model = 'gam' 
                                                     GROUP BY c.broad_id HAVING 
                                                     COUNT(DISTINCT c.cell_id) = %s;",
                                                     paste(rv_Eff$selected_cell_ids, collapse = "','"),
                                                     length(rv_Eff$selected_cell_ids)))
      compound_choices <- paste0(compound_choices_df$cpd_name, " : ", compound_choices_df$broad_batch_id)
      updateSelectizeInput(session,
                           inputId = "singleInput1",
                           label = "Select Compound:",
                           choices = compound_choices,
                           server = TRUE)
    }
  }
})

# Update singleInput2 based on multipleInputChoice, multipleInput, and singleInput1
observeEvent(list(input$multipleInputChoice, input$multipleInput, input$singleInput1), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  if (is.null(input$multipleInput)) {
    if (input$multipleInputChoice == "Cell Line") {
      updateSelectizeInput(session,
                           inputId = "singleInput2",
                           label = "Select Gene:",
                           choices = NULL,
                           server = TRUE)
    } else {
      updateSelectizeInput(session,
                           inputId = "singleInput2",
                           label = "Select Cell Line:",
                           choices = NULL,
                           server = TRUE)
    }
  } else {
    if (input$multipleInputChoice == "Compound") {
      req(input$multipleInput, input$singleInput1, rv_Eff$selected_broad_batch_ids)
      selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info
                                                  WHERE symbol = '%s';",
                                                  input$singleInput1))
      rv_Eff$selected_gene <- selected_gene_df$gene[1]

      cell_line_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT ci.* FROM condition c 
                                                      JOIN cell_info ci ON c.cell_id = ci.cell_id 
                                                      WHERE broad_batch_id IN ('%s') AND model = 'gam'
                                                      GROUP BY c.cell_id
                                                      HAVING COUNT(DISTINCT broad_batch_id) = %s;",
                                                      paste(rv_Eff$selected_broad_batch_ids, collapse = "','"),
                                                      length(rv_Eff$selected_broad_batch_ids)))
      cell_line_choices <- paste0(cell_line_choices_df$primary_site, " : ", cell_line_choices_df$cell_id)
      updateSelectizeInput(session,
                           inputId = "singleInput2",
                           label = "Select Cell Line:",
                           choices = cell_line_choices,
                           server = TRUE, selected = ifelse("breast : MCF7" %in% cell_line_choices, "breast : MCF7", cell_line_choices[1]))
    } else if (input$multipleInputChoice == "Gene") {
      req(input$multipleInput, input$singleInput1, rv_Eff$selected_genes)
      rv_Eff$selected_broad_batch_id <- sub(".*:\\s*", "", input$singleInput1)

      cell_line_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT ci.* FROM condition c 
                                                      JOIN cell_info ci ON c.cell_id = ci.cell_id 
                                                      WHERE broad_batch_id = '%s' AND model = 'gam';",
                                                      rv_Eff$selected_broad_batch_id))
      cell_line_choices <- paste0(cell_line_choices_df$primary_site, " : ", cell_line_choices_df$cell_id)
      updateSelectizeInput(session,
                           inputId = "singleInput2",
                           label = "Select Cell Line:",
                           choices = cell_line_choices,
                           server = TRUE)
    } else if (input$multipleInputChoice == "Cell Line") {
      req(input$multipleInput, input$singleInput1, rv_Eff$selected_cell_ids)
      rv_Eff$selected_broad_batch_id <- sub(".*:\\s*", "", input$singleInput1)

      gene_choices_df <- dbGetQuery(con, "SELECT symbol FROM gene_info")
      gene_choices <- gene_choices_df$symbol
      updateSelectizeInput(session,
                           inputId = "singleInput2",
                           label = "Select Gene:",
                           choices = gene_choices,
                           server = TRUE)
    }
  }
})

# Update time based on multipleInputChoice, multipleInput, singleInput1, and singleInput2
observeEvent(list(input$multipleInputChoice, input$multipleInput, input$singleInput1, input$singleInput2), {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  if (is.null(input$multipleInput)) {
    updateSelectizeInput(session,
                         inputId = "timeEff",
                         label = "Select Time (Unit: h):",
                         choices = NULL,
                         server = TRUE)
  } else {
    if (input$multipleInputChoice == "Compound") {
      req(input$multipleInput, input$singleInput1, input$singleInput2,
          rv_Eff$selected_broad_batch_ids, rv_Eff$selected_gene)
      rv_Eff$selected_cell_id <- sub(".*:\\s*", "", input$singleInput2)
      
      time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition
                                                 WHERE broad_batch_id IN ('%s') AND cell_id = '%s'
                                                 GROUP BY pert_time
                                                 HAVING COUNT(DISTINCT broad_batch_id) = %s;",
                                                 paste(rv_Eff$selected_broad_batch_ids, collapse = "','"),
                                                 rv_Eff$selected_cell_id,
                                                 length(rv_Eff$selected_broad_batch_ids)))
    } else if (input$multipleInputChoice == "Gene") {
      req(input$multipleInput, input$singleInput1, input$singleInput2,
          rv_Eff$selected_genes, rv_Eff$selected_broad_batch_id)
      rv_Eff$selected_cell_id <- sub(".*:\\s*", "", input$singleInput2)
      
      time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition
                                                 WHERE broad_batch_id = '%s' AND cell_id = '%s';",
                                                 rv_Eff$selected_broad_batch_id,
                                                 rv_Eff$selected_cell_id))
    } else if (input$multipleInputChoice == "Cell Line") {
      req(input$multipleInput, input$singleInput1, input$singleInput2,
          rv_Eff$selected_cell_ids, rv_Eff$selected_broad_batch_id)
      selected_gene_df <- dbGetQuery(con, sprintf("SELECT gene FROM gene_info
                                                  WHERE symbol = '%s';",
                                                  input$singleInput2))
      rv_Eff$selected_gene <- selected_gene_df$gene[1]

      time_choices_df <- dbGetQuery(con, sprintf("SELECT DISTINCT pert_time FROM condition
                                                 WHERE broad_batch_id = '%s' AND cell_id IN ('%s')
                                                 GROUP BY pert_time
                                                 HAVING COUNT(DISTINCT cell_id) = %s;",
                                                 rv_Eff$selected_broad_batch_id,
                                                 paste(rv_Eff$selected_cell_ids, collapse = "','"),
                                                 length(rv_Eff$selected_cell_ids)))
    }
    time_choices <- time_choices_df$pert_time
    updateSelectizeInput(session,
                         inputId = "timeEff",
                         label = "Select Time (h):",
                         choices = time_choices,
                         server = TRUE)
  }
})

# Add selected efficacy vs potency condition to the list
observeEvent(input$addEffCombo, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  req(input$multipleInput, input$singleInput1, input$singleInput2, input$timeEff)

  new_combo <- data.frame(
    Compound = if (input$multipleInputChoice == "Compound")
      paste(input$multipleInput, collapse = ",") else input$singleInput1,
    GeneSymbol = if (input$multipleInputChoice == "Gene") {
      paste(input$multipleInput, collapse = ",")
    } else if (input$multipleInputChoice == "Cell Line") {
      input$singleInput2
    } else {
      input$singleInput1
    },
    CellLine = if (input$multipleInputChoice == "Cell Line")
      paste(input$multipleInput, collapse = ",") else input$singleInput2,
    Time = input$timeEff,
    Multi = input$multipleInputChoice,
    BroadBatchID = if (input$multipleInputChoice == "Compound")
      paste(rv_Eff$selected_broad_batch_ids, collapse = ",") else rv_Eff$selected_broad_batch_id,
    GeneID = if (input$multipleInputChoice == "Gene")
      paste(rv_Eff$selected_genes, collapse = ",") else rv_Eff$selected_gene,
    CellID = if (input$multipleInputChoice == "Cell Line")
      paste(rv_Eff$selected_cell_ids, collapse = ",") else rv_Eff$selected_cell_id,
    stringsAsFactors = FALSE
  )

  current_combos <- selected_Eff_conditions()

  if (!any(duplicated(rbind(current_combos, new_combo))) & nrow(current_combos) < 6) {
    selected_Eff_conditions(rbind(current_combos, new_combo))
  }
})

# Clear selected efficacy vs potency conditions
observeEvent(input$clearEffCombos, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  selected_Eff_conditions(data.frame(
    Compound = character(),
    GeneSymbol = character(),
    CellLine = character(),
    Time = character(),
    Multi = character(),
    BroadBatchID = character(),
    GeneID = character(),
    CellID = character(),
    stringsAsFactors = FALSE
  ))
})

# Display selected efficacy vs potency conditions
output$EffComboList <- renderUI({
  combo_df <- selected_Eff_conditions()
  if (nrow(combo_df) > 0) {
    tagList(
      lapply(seq_len(nrow(combo_df)), function(i) {
        fluidRow(
          column(8, paste(combo_df$Compound[i],
                          combo_df$GeneSymbol[i],
                          combo_df$CellLine[i],
                          paste0(combo_df$Time[i], "h"),
                          sep = " - ")),
          column(4, actionButton(inputId = paste0("remove_eff_", i),
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
  combo_df <- selected_Eff_conditions()
  lapply(seq_len(nrow(combo_df)), function(i) {
    observeEvent(input[[paste0("remove_eff_", i)]], {
      showLoading()
      on.exit(hideLoading(), add = TRUE)

      updated_combos <- combo_df[-i, ]
      selected_Eff_conditions(updated_combos)
    }, ignoreInit = TRUE, once = TRUE)
  })
})

# Query the relevant data from the SQLite database
observeEvent({input$plotEffBtn; visitedEff()}, {
  showLoading()
  on.exit(hideLoading(), add = TRUE)

  isolate({
    combo_df <- selected_Eff_conditions()
    if (nrow(combo_df) > 0) {
      filtered_df_lst <- lapply(seq_len(nrow(combo_df)), function(i) {
        combo <- combo_df[i, ]

        # Query the relevant data from the SQLite database
        if (combo$Multi == "Compound") {
          query_1 <- sprintf("SELECT p.cpd_name AS id, cg.cond_gene_id FROM condition c
                             JOIN condition_gene cg ON c.cond_id = cg.cond_id
                             JOIN pert p ON c.broad_id = p.broad_id
                             WHERE c.broad_batch_id IN ('%s') AND
                             c.cell_id = '%s' AND
                             c.pert_time = %s;",
                             gsub(",", "','", combo$BroadBatchID),
                             combo$CellID,
                             combo$Time)
          filtered_df_1 <- dbGetQuery(con, query_1)
          selected_cond_gene_ids <- filtered_df_1$cond_gene_id

          query_2 <- sprintf("SELECT cg.cond_gene_id, i.lpotency, i.se_lpotency,
                             i.lefficacy, i.se_lefficacy FROM condition_gene cg
                             JOIN interaction i ON cg.cond_gene_id = i.cond_gene_id
                             WHERE cg.cond_gene_id IN (%s) AND
                             cg.gene = %s;",
                             paste(selected_cond_gene_ids, collapse = ","),
                             combo$GeneID)
          filtered_df_2 <- dbGetQuery(con, query_2)

          filtered_df <- filtered_df_1 %>%
            inner_join(filtered_df_2, by = "cond_gene_id") %>%
            mutate(Compound = id,
                   GeneSymbol = combo$GeneSymbol,
                   CellLine = combo$CellID,
                   Time = combo$Time)
        } else if (combo$Multi == "Gene") {
          query_1 <- sprintf("SELECT cg.cond_gene_id FROM condition c
                             JOIN condition_gene cg ON c.cond_id = cg.cond_id
                             WHERE c.broad_batch_id = '%s' AND
                             c.cell_id = '%s' AND
                             c.pert_time = %s;",
                             combo$BroadBatchID,
                             combo$CellID,
                             combo$Time)
          filtered_df_1 <- dbGetQuery(con, query_1)
          selected_cond_gene_ids <- filtered_df_1$cond_gene_id

          query_2 <- sprintf("SELECT gi.symbol AS id, cg.cond_gene_id, i.lpotency,
                             i.se_lpotency, i.lefficacy, i.se_lefficacy FROM condition_gene cg
                             JOIN interaction i ON cg.cond_gene_id = i.cond_gene_id
                             JOIN gene_info gi ON cg.gene = gi.gene
                             WHERE cg.cond_gene_id IN (%s) AND
                             cg.gene IN (%s);",
                             paste(selected_cond_gene_ids, collapse = ","),
                             paste(combo$GeneID, collapse = ","))
          filtered_df_2 <- dbGetQuery(con, query_2)

          filtered_df <- filtered_df_1 %>%
            inner_join(filtered_df_2, by = "cond_gene_id") %>%
            mutate(Compound = combo$Compound,
                   GeneSymbol = id,
                   CellLine = combo$CellID,
                   Time = combo$Time)
        } else if (combo$Multi == "Cell Line") {
          query_1 <- sprintf("SELECT c.cell_id AS id, cg.cond_gene_id FROM condition c
                             JOIN condition_gene cg ON c.cond_id = cg.cond_id
                             WHERE c.broad_batch_id = '%s' AND
                             c.cell_id IN ('%s') AND
                             c.pert_time = %s;",
                             combo$BroadBatchID,
                             gsub(",", "','", combo$CellID),
                             combo$Time)
          filtered_df_1 <- dbGetQuery(con, query_1)
          selected_cond_gene_ids <- filtered_df_1$cond_gene_id

          query_2 <- sprintf("SELECT cg.cond_gene_id, i.lpotency, i.se_lpotency,
                             i.lefficacy, i.se_lefficacy FROM condition_gene cg
                             JOIN interaction i ON cg.cond_gene_id = i.cond_gene_id
                             WHERE cg.cond_gene_id IN (%s) AND
                             cg.gene = %s;",
                             paste(selected_cond_gene_ids, collapse = ","),
                             combo$GeneID)

          filtered_df_2 <- dbGetQuery(con, query_2)

          filtered_df <- filtered_df_1 %>%
            inner_join(filtered_df_2, by = "cond_gene_id") %>%
            mutate(Compound = combo$Compound,
                   GeneSymbol = combo$GeneSymbol,
                   CellLine = id,
                   Time = combo$Time)
        }

        CI95q <- qnorm(0.975)
        filtered_df <- filtered_df %>%
          mutate(Plot = i,
                 potency = 10^lpotency,
                 se_potency = 10^se_lpotency,
                 efficacy = 2^lefficacy * 100,
                 se_efficacy = 2^se_lefficacy,
                 text_label = paste(id,
                                    sprintf("<br>Pot: %s; (%s,%s)",
                                            signif(potency, 2),
                                            signif(potency / (se_potency^CI95q), 2),
                                            signif(potency * (se_potency^CI95q), 2)),
                                    sprintf("<br>Eff: %s%%; (%s%%,%s%%)",
                                            signif(efficacy, 2),
                                            signif(efficacy / (se_efficacy^CI95q), 2),
                                            signif(efficacy * (se_efficacy^CI95q), 2)))) %>%
          filter(!is.na(potency) & !is.na(se_potency) & !is.na(efficacy) & !is.na(se_efficacy) &
                   is.finite(potency) & is.finite(se_potency) & is.finite(efficacy) & is.finite(se_efficacy))
      })
    } else {
      filtered_df_lst <- list()
    }
    rv_Eff$filtered_df_lst <- filtered_df_lst
  })
})

# Generate and display the interactive efficacy vs. potency plot
output$EffPlot <- renderPlotly({
  req(rv_Eff$filtered_df_lst)
  isolate({
    combo_df <- selected_Eff_conditions()
    if (nrow(combo_df) > 0) {
      # Create a list of plots
      plot_list <- lapply(seq_len(length(rv_Eff$filtered_df_lst)), function(i) {
        filtered_df <- rv_Eff$filtered_df_lst[[i]]
        combo <- combo_df[i, ]
        CI95q <- qnorm(0.975)

        if (nrow(filtered_df) > 0) {
          suppressWarnings({
            gg <- ggplot(filtered_df,
                         aes(x = potency,
                             y = efficacy,
                             color = id,
                             fill = id)) +
              geom_point(aes(text = text_label),
                         size = 2) +
              geom_errorbarh(aes(xmin = potency / (se_potency^CI95q),
                                 xmax = potency * (se_potency^CI95q)),
                             alpha = 0.5,
                             height = 0) +
              geom_errorbar(aes(ymin = efficacy / (se_efficacy^CI95q),
                                ymax = efficacy * (se_efficacy^CI95q)),
                            alpha = 0.5,
                            width = 0) +
              geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
              theme_minimal() +
              theme(legend.position = "none") +
              scale_x_continuous(trans = "log10",
                                 limits = c(min(filtered_df$potency / (filtered_df$se_potency^CI95q)) / 1.2,
                                            max(filtered_df$potency * (filtered_df$se_potency^CI95q)) * 1.2)) +
              scale_y_continuous(trans = "log10",
                                 limits = c(min(filtered_df$efficacy / (filtered_df$se_efficacy^CI95q)) / 1.2,
                                            max(filtered_df$efficacy * (filtered_df$se_efficacy^CI95q)) * 1.2))
          })

          if (combo$Multi == "Compound") {
            gg <- gg + annotate("text",
                                x = sqrt(min(filtered_df$potency / filtered_df$se_potency) *
                                           max(filtered_df$potency * filtered_df$se_potency)),
                                y = max(filtered_df$efficacy * (filtered_df$se_efficacy^CI95q)) * 1.2,
                                label = paste(combo$GeneSymbol,
                                              combo$CellLine,
                                              paste0(combo$Time, "h"),
                                              sep = " - "),
                                size = 3, hjust = 0.5)
          } else if (combo$Multi == "Gene") {
            gg <- gg + annotate("text",
                                x = sqrt(min(filtered_df$potency / filtered_df$se_potency) *
                                           max(filtered_df$potency * filtered_df$se_potency)),
                                y = max(filtered_df$efficacy * (filtered_df$se_efficacy^CI95q)) * 1.2,
                                label = paste0(combo$Compound, " - \n",
                                               combo$CellLine, " - ",
                                               paste0(combo$Time, "h")),
                                size = 3, hjust = 0.5)
          } else if (combo$Multi == "Cell Line") {
            gg <- gg + annotate("text",
                                x = sqrt(min(filtered_df$potency / filtered_df$se_potency) *
                                           max(filtered_df$potency * filtered_df$se_potency)),
                                y = max(filtered_df$efficacy * (filtered_df$se_efficacy^CI95q)) * 1.2,
                                label = paste0(combo$Compound, " - \n",
                                               combo$GeneSymbol, " - ",
                                               paste0(combo$Time, "h")),
                                size = 3, hjust = 0.5)
          }
        } else {
          gg <- ggplot() +
            theme_minimal() +
            theme(legend.position = "none") +
            scale_x_continuous(trans = "log10",
                               limits = c(0.1, 10)) +
            scale_y_continuous(trans = "log10",
                               limits = c(10, 1000))

          if (combo$Multi == "Compound") {
            gg <- gg + annotate("text", x = 1, y = 900,
                                label = paste(combo$GeneSymbol,
                                              combo$CellLine,
                                              paste0(combo$Time, "h"),
                                              sep = " - "),
                                size = 3, hjust = 0.5)
          } else if (combo$Multi == "Gene") {
            gg <- gg + annotate("text", x = 1, y = 900,
                                label = paste0(combo$Compound, " - \n",
                                               combo$CellLine, " - ",
                                               paste0(combo$Time, "h")),
                                size = 3, hjust = 0.5)
          } else if (combo$Multi == "Cell Line") {
            gg <- gg + annotate("text", x = 1, y = 900,
                                label = paste0(combo$Compound, " - \n",
                                               combo$GeneSymbol, " - ",
                                               paste0(combo$Time, "h")),
                                size = 3, hjust = 0.5)
          }
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

# Activate download button for output
output$downloadEff <- renderUI({
  if (length(rv_Eff$filtered_df_lst) > 0) {
    downloadButton('EffOutputFile', 'Download Data')
  }
})

# Export result to output for downloading
output$EffOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    # Concatenate the relevant data
    all_filtered_df <- do.call('rbind', lapply(rv_Eff$filtered_df_lst, function(x) {
      x %>%
        dplyr::select(Plot, Compound, GeneSymbol, CellLine, Time,
                      potency, se_potency, efficacy, se_efficacy) %>%
        `colnames<-`(c("Plot", "Compound", "Gene", "Cell Line", "Time (Unit: h)",
                       "Potency (Unit: uM)", "SE Potency", "Efficacy (% DMSO)", "SE Efficacy"))
    }))
    write.table(all_filtered_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)
