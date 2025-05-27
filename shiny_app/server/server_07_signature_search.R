
# Create a reactive object with no values
rv_Sig <- reactiveValues()

# Function to parse and validate genes
parse_and_validate_genes <- function(up_raw, down_raw, gene_bg) {
  up <- unique(trimws(unlist(strsplit(up_raw, "\n"))))
  down <- unique(trimws(unlist(strsplit(down_raw, "\n"))))
  
  # Filter to genes present in background
  up <- up[up %in% gene_bg]
  down <- down[down %in% gene_bg]
  
  list(up = up, down = down)
}

# Function to run Fisher's test
run_fisher <- function(user_set, cond_set, background) {
  A <- length(intersect(user_set, cond_set))
  B <- length(setdiff(user_set, cond_set))
  C <- length(setdiff(cond_set, user_set))
  D <- length(setdiff(background, union(user_set, cond_set)))
  mat <- matrix(c(A, B, C, D), nrow = 2)
  ft <- fisher.test(mat)
  list(p = ft$p.value, overlap = A)
}

# Load condition gene sets from  DB
cond_gene_sets <- reactive({
  dat <- dbGetQuery(con, "SELECT c.*, cgs.pert_dose, cgs.up_genes, cgs.down_genes, p.cpd_name FROM cond_gene_sets cgs
                    JOIN condition c ON c.cond_id = cgs.cond_id
                    JOIN pert p ON c.broad_id = p.broad_id")
  dat$up_genes <- lapply(dat$up_genes, fromJSON)
  dat$down_genes <- lapply(dat$down_genes, fromJSON)
  dat
})

# Background gene universe
gene_universe <- reactive({
  gene_info <- dbReadTable(con, "gene_info")
  unique(gene_info$symbol)
})

# Load example
observeEvent(input$exampleSig, {
  example_up_genes <- c("INTS3", "EDEM1", "WDR7")
  example_down_genes <- c("SMNDC1", "USP22", "UBE3C", "NRIP1", "POLR2K", "CCNB2", "MRPL19", "REEP5", "CYCS", "RAE1", "XPO7", "KIF1BP", "MAPK1IP1L", "BACE2", "MRPS2", "NOL3")
  
  updateTextAreaInput(session, "up_gene_symbols", value = paste(example_up_genes, collapse = "\n"))
  updateTextAreaInput(session, "down_gene_symbols", value = paste(example_down_genes, collapse = "\n"))
})

# Validate user gene input
observeEvent(input$validateSig, {
  bg <- gene_universe()
  validated <- parse_and_validate_genes(input$up_gene_symbols, input$down_gene_symbols, bg)
  
  # Update text areas
  updateTextAreaInput(session, "up_gene_symbols", value = paste(validated$up, collapse = "\n"))
  updateTextAreaInput(session, "down_gene_symbols", value = paste(validated$down, collapse = "\n"))
  
  # Update status message
  rv_Sig$status_msg <- sprintf("%s up and %s downregulated genes matched to the L1000 landmark gene alphabet", length(validated$up), length(validated$down))
})

# Run Fisher's exact test
observeEvent(input$submitSig, {
  background <- gene_universe()
  user_sets <- parse_and_validate_genes(input$up_gene_symbols, input$down_gene_symbols, background)
  
  # Update status message
  rv_Sig$status_msg <- sprintf("%s up and %s downregulated genes matched to the L1000 landmark gene alphabet", length(user_sets$up), length(user_sets$down))
  
  # Check if matched genes exist
  if (length(user_sets$up) == 0 && length(user_sets$down) == 0) return()
  
  # Load reference signatures
  withProgress(message = "Loading reference signatures...", value = 0, {
    dat <- cond_gene_sets()
  })
  
  # Preallocate numeric matrix for results
  res_mat <- matrix(NA_real_, nrow = nrow(dat), ncol = 4)
  colnames(res_mat) <- c("up_p", "up_overlap", "down_p", "down_overlap")
  
  # Run fisher test for each compound
  withProgress(message = "Running Fisher tests...", value = 0, {
    for (i in seq_len(nrow(dat))) {
      up_res <- run_fisher(user_sets$up, dat$up_genes[[i]], background)
      down_res <- run_fisher(user_sets$down, dat$down_genes[[i]], background)
      
      res_mat[i, ] <- c(up_res$p, up_res$overlap, down_res$p, down_res$overlap)
      
      incProgress(1 / nrow(dat), detail = paste(i, "/", nrow(dat)))
    }
  })
  
  # Reformat the data
  dat$sig_genes <- mapply(function(up, down) {
    paste0("Up: ", paste(up, collapse = ", "), "; Down: ",  paste(down, collapse = ", "))
  }, dat$up_genes, dat$down_genes)
  
  dat <- dat %>%
    dplyr::select(-c(up_genes, down_genes))
  
  # Combine condition info with matrix results
  res_df <- cbind(dat[, c("cpd_name", "broad_id", "batch_id", "cell_id", "pert_time", "pert_dose", "sig_genes")], as.data.frame(res_mat))
  
  # Keep two decimal places for numeric values; rename columns
  res_df <- res_df %>%
    mutate(up_p = p.adjust(up_p, method = "BH"),
           down_p = p.adjust(down_p, method = "BH")) %>%
    arrange(pmin(up_p, down_p)) %>%
    mutate(pert_dose = signif(pert_dose, 2),
           up_p = signif(up_p, 2),
           down_p = signif(down_p, 2)) %>%
    `colnames<-`(c("Compound", "BROAD ID", "Batch", "Cell Line", "Time (Unit: h)",
                   "Dose (Unit: uM)", "Signature", "Adj. P-Value (Up)", "# Overlap (Up)",
                   "Adj. P-Value (Down)", "# Overlap (Down)"))
  
  # Store results
  rv_Sig$results_df <- res_df
})

# Render message in UI
output$status_msg <- renderText({ rv_Sig$status_msg })

# Render table in UI
output$SigTable <- renderDT({
  req(rv_Sig$results_df)
  datatable(rv_Sig$results_df %>%
              dplyr::filter(`Adj. P-Value (Up)` < 0.05 | `Adj. P-Value (Down)` < 0.05),
            escape = FALSE,
            rownames = FALSE,
            options = list(
              columnDefs = list(
                list(
                  targets = 0,
                  className = "details-control",
                  render = JS("function(data) {
                                  return '<a href=\"#\">' + data + '</a>';
                                }")
                ),
                list(
                  targets = 6,
                  visible = FALSE
                )
              )
            ),
            callback = JS("
                table.on('click', 'td.details-control', function() {
                  var tr = $(this).closest('tr');
                  var row = table.row(tr);
                  if (row.child.isShown()) {
                    row.child.hide(); tr.removeClass('shown');
                  } else {
                    row.child(row.data()[6].replace(/; /g, '<br>')).show(); tr.addClass('shown');
                  }
                });
              ")
  )
})

# Activate download button for output
output$downloadSig <- renderUI({
  if (length(rv_Sig$results_df) > 0) {
    downloadButton('SigOutputFile', 'Download Data')
  }
})

# Export result to output for downloading
output$SigOutputFile <- downloadHandler(
  filename = function() {"output.txt"},
  content = function(fname) {
    write.table(rv_Sig$results_df,
                fname,
                sep = "\t",
                row.names = FALSE,
                na = "")
  }
)
