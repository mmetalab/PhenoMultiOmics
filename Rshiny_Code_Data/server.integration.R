########################################about
observeEvent(input$integration1, {
  print("integration1")
})

observeEvent(input$integration2, {
  print("integration2")
})

observeEvent(input$integration3, {
  print("integration3")
})


########################################upload data
observeEvent(input$sample_integrat_data, {
  if (input$sample_integrat_data) {
    updateCheckboxInput(session, "own_integrat_data", value = FALSE)
  }
})

observeEvent(input$own_integrat_data, {
  if (input$own_integrat_data) {
    updateCheckboxInput(session, "sample_integrat_data", value = FALSE)
  }
})

output$upload_integrat_options <- renderUI({
  if (input$own_integrat_data) {
    tagList(
    fileInput("file_gene", "Upload transcriptomics data", accept = ".csv"),
    fileInput("file_protein", "Upload proteomics data", accept = ".csv"),
    fileInput("file_metabolite", "Upload metabolomics data", accept = ".csv")
    )
  }
})

reactive_data_gene <- reactive({
  if (input$sample_integrat_data) {
    return(static_data_gene)
  } else if (!is.null(input$file_gene) && file.exists(input$file_gene$datapath)) {
    return(readr::read_csv(input$file_gene$datapath))
  } else{
    return(NULL)
  }
})

reactive_data_protein <- reactive({
  if (input$sample_integrat_data) {
    return(static_data_protein)
  } else if(!is.null(input$file_protein) && file.exists(input$file_protein$datapath)) {
    return(readr::read_csv(input$file_protein$datapath))
  }else{
    return(NULL)
  }
})

reactive_data_metabolite <- reactive({
  if (input$sample_integrat_data) {
    return(static_data_metabolite)
  } else if(!is.null(input$file_metabolite) && file.exists(input$file_metabolite$datapath)) {
    return(readr::read_csv(input$file_metabolite$datapath))
  }else{
    return(NULL)
  }
})

output$table_gene <- DT::renderDataTable({
  if (!is.null(reactive_data_gene())) {
    df <-reactive_data_gene()
    
    df_formatted <- df
    for (col_name in names(df)) {
      
      if (is.numeric(df[[col_name]])) {
        
        mask <- nchar(sub(".*\\.(.*)", "\\1", as.character(df[[col_name]]))) > 5
        
        df_formatted[[col_name]][mask] <- round(df[[col_name]][mask], 5)
      }
    }
    df_formatted
    
  }
}, options = list(pageLength = 5))


output$conditionalText_table_gene <- renderUI({
  if (!is.null(reactive_data_gene())) {
    h3("Uploaded transcriptomics data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

output$table_protein <- DT::renderDataTable({
  if (!is.null(reactive_data_protein())) {
    
    df <-reactive_data_protein()
    
    df_formatted <- df
    for (col_name in names(df)) {
      
      if (is.numeric(df[[col_name]])) {
        
        mask <- nchar(sub(".*\\.(.*)", "\\1", as.character(df[[col_name]]))) > 5
        
        df_formatted[[col_name]][mask] <- round(df[[col_name]][mask], 5)
      }
    }
    df_formatted
    
  }
}, options = list(pageLength = 5))

output$conditionalText_table_protein <- renderUI({
  if (!is.null(reactive_data_protein())) {
    h3("Uploaded proteomics data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

output$table_metabolite <- DT::renderDataTable({
  if (!is.null(reactive_data_metabolite())) {
    
    df <-reactive_data_metabolite()
    
    df_formatted <- df
    for (col_name in names(df)) {
      
      if (is.numeric(df[[col_name]])) {
        
        mask <- nchar(sub(".*\\.(.*)", "\\1", as.character(df[[col_name]]))) > 5
        
        df_formatted[[col_name]][mask] <- round(df[[col_name]][mask], 5)
      }
    }
    df_formatted
  }
}, options = list(pageLength = 5))

output$conditionalText_table_metabolite <- renderUI({
  if (!is.null(reactive_data_metabolite())) {
    h3("Uploaded metabolomics  data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

##############################################normalize
normalize_data <- function(data, method) {
  expression_data <- data %>% 
    dplyr::select(-Sample, -Group)%>%
    dplyr::mutate_all(as.numeric)
  
  if (method == "Min-Max") {
    normalized_data <- as.data.frame(lapply(expression_data, function(x) (x - min(x)) / (max(x) - min(x))))
  } else {
    normalized_data <- as.data.frame(scale(expression_data, center = TRUE, scale = TRUE))
  }
  normalized_data <- round(normalized_data,5)
  return(normalized_data)
}

# Reactive normalized data
reactive_normalized_data_gene <- eventReactive(input$data_choice=="Gene", {
  norm_method <- input$norm_method
  gene_data <- reactive_data_gene()
  norm_gene <- normalize_data(gene_data, norm_method)
  cbind(gene_data[, c("Sample", "Group")], norm_gene)
})

reactive_normalized_data_protein <- eventReactive(input$data_choice=="Protein", {
  norm_method <- input$norm_method
  protein_data <- reactive_data_protein()
  norm_protein <- normalize_data(protein_data, norm_method)
  cbind(protein_data[, c("Sample", "Group")], norm_protein)
})

reactive_normalized_data_metabolite <- eventReactive(input$data_choice=="Metabolite", {
  norm_method <- input$norm_method
  metabolite_data <- reactive_data_metabolite()
  norm_metabolite <- normalize_data(metabolite_data, norm_method)
  cbind(metabolite_data[, c("Sample", "Group")], norm_metabolite)
})

# Observe event for Action_Normalization button
observeEvent(input$Action_Normalization, {
  output$table_normalization_gene <- DT::renderDataTable({
    reactive_normalized_data_gene()
  }, options = list(pageLength = 5))

  output$table_normalization_protein <- DT::renderDataTable({
    reactive_normalized_data_protein()
  }, options = list(pageLength = 5))

  output$table_normalization_metabolite <- DT::renderDataTable({
    reactive_normalized_data_metabolite()
  }, options = list(pageLength = 5))
})

# Render single table conditionally
observeEvent(input$Action_Normalization, {
output$table_normalization <- DT::renderDataTable({
  if (input$data_choice == "Gene") {
    reactive_normalized_data_gene()
  } else if (input$data_choice == "Protein") {
    reactive_normalized_data_protein()
  } else if (input$data_choice == "Metabolite") {
    reactive_normalized_data_metabolite()
  } else {
    NULL
  }
}, options = list(pageLength = 5))
})




# Download handler for normalized data
output$download_table_normalization <- downloadHandler(
  # Define the name of the downloaded file
  filename = function() {
    if (input$data_choice == "All") {
      paste("normalized_data_", Sys.Date(), ".zip", sep = "")
    } else {
      paste("normalized_data_", input$data_choice, "_", Sys.Date(), ".csv", sep = "")
    }
  },
  
  # Define the content of the downloaded file
  content = function(file) {
    if (input$data_choice == "All") {
      # Create a temporary directory for storing the files
      temp_dir <- tempdir()
      
      # Define file paths for each data type
      gene_file <- file.path(temp_dir, "normalized_data_gene.csv")
      protein_file <- file.path(temp_dir, "normalized_data_protein.csv")
      metabolite_file <- file.path(temp_dir, "normalized_data_metabolite.csv")
      
      # Write each dataset to its respective file
      write.csv(reactive_normalized_data_gene(), gene_file, row.names = FALSE)
      write.csv(reactive_normalized_data_protein(), protein_file, row.names = FALSE)
      write.csv(reactive_normalized_data_metabolite(), metabolite_file, row.names = FALSE)
      
      # Zip the files together
      zip::zipr(file, files = c(gene_file, protein_file, metabolite_file))
    } else {
      # Select the correct data to download based on user input
      data_to_download <- switch(input$data_choice,
                                 "Gene" = reactive_normalized_data_gene(),
                                 "Protein" = reactive_normalized_data_protein(),
                                 "Metabolite" = reactive_normalized_data_metabolite())
      
      # Write the selected data to a CSV file
      write.csv(data_to_download, file, row.names = FALSE)
    }
  }
)


################################################################
# Function to prepare and combine datasets with unique feature spaces
prepare_combined_data <- function(gene_data, protein_data, metabolite_data) {
  # Add prefix to each data type for unique column names
  colnames(gene_data)[3:ncol(gene_data)] <- paste0("gene_", colnames(gene_data)[3:ncol(gene_data)])
  colnames(protein_data)[3:ncol(protein_data)] <- paste0("protein_", colnames(protein_data)[3:ncol(protein_data)])
  colnames(metabolite_data)[3:ncol(metabolite_data)] <- paste0("metabolite_", colnames(metabolite_data)[3:ncol(metabolite_data)])
  
  # Add Type column to each dataset
  gene_data <- gene_data %>% mutate(Type = "Gene")
  protein_data <- protein_data %>% mutate(Type = "Protein")
  metabolite_data <- metabolite_data %>% mutate(Type = "Metabolite")
  
  # Merge all data by Sample and Group
  combined_data <- bind_rows(gene_data, protein_data, metabolite_data)
  
  # Replace NA with 0 (assuming missing features imply absence or no measurement)
  combined_data[is.na(combined_data)] <- 0
  
  return(combined_data)
}

combined_data1 <- reactive({
  if (input$data_choice == "All") {
    gene_data <- reactive_data_gene()
    protein_data <- reactive_data_protein()
    metabolite_data <- reactive_data_metabolite()
    
    combined_data <- prepare_combined_data(gene_data, protein_data, metabolite_data)
    
    return(combined_data)
  } else {
    NULL
  }
})

combined_data2 <- reactive({
  if (input$data_choice == "All") {
    gene_data <- reactive_normalized_data_gene()
    protein_data <- reactive_normalized_data_protein()
    metabolite_data <- reactive_normalized_data_metabolite()
    
    combined_data2 <- prepare_combined_data(gene_data, protein_data, metabolite_data)
    
    return(combined_data2)
  } else {
    NULL
  }
})

# t-SNE analysis
tsne_result <- reactive({
  if (input$data_choice == "All") {
    if(input$data_select=="Normalization data"){
      data <- combined_data2() }
    else if(input$data_select=="Row data"){
      data <- combined_data1()}
  } else if (input$data_choice == "Gene") {
    if(input$data_select=="Normalization data"){
      data <- reactive_normalized_data_gene() %>% mutate(Type = "Gene")}
    else if(input$data_select=="Row data"){
      data <- reactive_data_gene() %>% mutate(Type = "Gene")}
  } else if (input$data_choice == "Protein") {
    if(input$data_select=="Normalization data"){
      data <- reactive_normalized_data_protein() %>% mutate(Type = "Protein")}
    else if(input$data_select=="Row data"){
      data <- reactive_data_protein() %>% mutate(Type = "Protein")}
  } else if (input$data_choice == "Metabolite") {
    if(input$data_select=="Normalization data"){
      data <- reactive_normalized_data_metabolite() %>% mutate(Type = "Metabolite")}
    else if(input$data_select=="Row data"){
      data <- reactive_data_metabolite() %>% mutate(Type = "Metabolite")}
  } else {
    return(NULL)
  }
  
  numeric_data <- data %>% dplyr::select(where(is.numeric)) %>% na.omit()
  
  # Set a suitable perplexity
  num_samples <- nrow(numeric_data)
  perplexity <- min(5, max(1, floor(num_samples / 5)))
  
  # Perform t-SNE
  tsne <- Rtsne(numeric_data, dims = 2, perplexity = perplexity, verbose = TRUE, max_iter = 500)
  tsne_df <- data.frame(tsne$Y)
  tsne_df$Group <- data$Group
  tsne_df$Type <- data$Type
  
  return(tsne_df)
})

tsne_plot_generate <- eventReactive(input$action_tsne,{
  tsne_df <- tsne_result()
  
  ggplot(tsne_df, aes(x = X1, y = X2, color = interaction(Type, Group))) +
    geom_point(size = 4) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    labs(color = "Type and Group")
})

  output$tsne_plot <- renderPlot({
    tsne_plot_generate()})


output$download_umap_plot <- downloadHandler(
  filename = function() {
    paste("Umap_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = tsne_plot_generate(), device = "png", width = 8, height = 6, units = "in", dpi = 300)
  }
)

##############################################umap
# UMAP analysis
umap_result <- reactive({
  if (input$data_choice == "All") {
    if(input$data_select2=="Normalization data"){
      data <- combined_data2() }
    else if(input$data_select2=="Row data"){
      data <- combined_data1()}
  } else if (input$data_choice == "Gene") {
    if(input$data_select2=="Normalization data"){
      data <- reactive_normalized_data_gene() %>% mutate(Type = "Gene")}
    else if(input$data_select2=="Row data"){
      data <- reactive_data_gene() %>% mutate(Type = "Gene")}
  } else if (input$data_choice == "Protein") {
    if(input$data_select2=="Normalization data"){
      data <- reactive_normalized_data_protein() %>% mutate(Type = "Protein")}
    else if(input$data_select2=="Row data"){
      data <- reactive_data_protein() %>% mutate(Type = "Protein")}
  } else if (input$data_choice == "Metabolite") {
    if(input$data_select2=="Normalization data"){
      data <- reactive_normalized_data_metabolite() %>% mutate(Type = "Metabolite")}
    else if(input$data_select2=="Row data"){
      data <- reactive_data_metabolite() %>% mutate(Type = "Metabolite")}
  } else {
    return(NULL)
  }

  numeric_data <- data %>% dplyr::select(where(is.numeric)) %>% na.omit()
  
  umap_config <- umap::umap.defaults
  umap_config$n_neighbors <- min(15, nrow(numeric_data) - 1)
  umap_result <- umap::umap(numeric_data, config = umap_config)
  
  umap_df <- data.frame(umap_result$layout)
  umap_df$Group <- data$Group
  umap_df$Type <- data$Type
  
  return(umap_df)
})

umap_plot_generate <- eventReactive(input$action_umap,{
  umap_df <- umap_result()
  
  ggplot(umap_df, aes(x = X1, y = X2, color = interaction(Type, Group))) +
    geom_point(size = 4) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(color = "Type and Group")
})


 output$umap_plot <- renderPlot({
    umap_plot_generate()
  })

eventReactive(input$action_tsne,{
  output$umap_plot <- renderPlot({
    umap_plot_generate()
  })
})

output$download_umap_plot <- downloadHandler(
  filename = function() {
    paste("Umap_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # ggsave(file, plot = umap_plot_generate(), device = "png")
    ggsave(file, plot = umap_plot_generate(), device = "png", width = 8, height = 6, units = "in", dpi = 300)
  }
)