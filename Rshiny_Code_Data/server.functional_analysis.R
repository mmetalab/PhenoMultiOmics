load("9.All_edge.RData")
########################################about
observeEvent(input$biomarker1, {
  print("biomarker1")
})

observeEvent(input$biomarker2, {
  print("biomarker2")
})

observeEvent(input$biomarker3, {
  print("biomarker3")
})
##############################################33
sample_data3_1 <- reactiveVal(NULL)

observeEvent(input$sample_data3_1, {
  if (input$sample_data3_1) {
    load("3.DE_analysis.RData")
    sample_data3_1(dat3_1)
    updateCheckboxInput(session, "own_data3_1", value = FALSE)
  }
})

observeEvent(input$own_data3_1, {
  if (input$own_data3_1) {
    updateCheckboxInput(session, "sample_data3_1", value = FALSE)
  }
})

output$upload_options3_1 <- renderUI({
  if (input$own_data3_1) {
    fileInput("file3_1", "Choose csv file", accept = ".csv")
  }
})

own_data3_1 <- reactive({
  req(input$file3_1)
  readr::read_csv(input$file3_1$datapath)
})

output$table3_1 <- DT::renderDataTable({
  
  data <- NULL
  
  if (input$sample_data3_1) {
    data <- sample_data3_1()
  } else if (input$own_data3_1 && !is.null(input$file3_1)) {
    data <- own_data3_1()
  }
  
  if (!is.null(data)) {
    if (ncol(data) > 100) {
      data <- data[, 1:70]
    }
  }
  
}, options = list(pageLength = 5))

output$conditionalText_table3_1 <- renderUI({
  if (input$sample_data3_1 || !is.null(input$file3_1)) {
    h3("Uploaded data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

############################univariate analysis
datacontent3_2 <- reactive({
  
  data <- if (input$sample_data3_1) {
    sample_data3_1()
  } else if (input$own_data3_1 && !is.null(input$file3_1)) {
    own_data3_1()
  } else {
    return(NULL)
  }
  
  dat3 <- t(data[,-1])
  group3 <- data[,1]
  
  dat3 <- dat3 %>% data.frame(stringsAsFactors = FALSE) %>%
    data.matrix()
  
  design1 <- model.matrix(~0 + factor(group3))
  
  colnames(design1) <- c("control", "treat")
  rownames(design1) <- colnames(dat3)
  
  contrast.matrix1 <- makeContrasts(treat - control, levels = design1)
  
  fit1 <- lmFit(dat3, design1)
  fit1_2 <- eBayes(contrasts.fit(fit1, contrast.matrix1))
  temoutput1 <- limma::topTable(fit1_2, n = Inf, adjust = "fdr")
  temoutput1 <- na.omit(temoutput1)
  
  temoutput1 <- dplyr::mutate(temoutput1, g = ifelse(temoutput1$adj.P.Val < input$Pvalue3_1 & abs(temoutput1$logFC) >= input$logFC3_1,
                                                     ifelse(temoutput1$logFC > input$logFC3_1, 'Up', 'Down'), 'Stable'))
  
  return(temoutput1)
})

observeEvent(input$action3_1, {
  output$table3_2 <- DT::renderDataTable({
    df <- datacontent3_2()
    df <- df %>%
      mutate(across(where(is.numeric), ~ ifelse(. < 0.01, formatC(., format = "e", digits = 2), round(., 5))))
    df
    
  }, options = list(pageLength = 5))
  
  output$summary_input <- renderPrint({
    xx <- datacontent3_2()
    table(xx$g)
  })
  
})

output$conditionalText_table3_2 <- renderUI({
  if (input$action3_1) {
    h3("The univariate analysis result is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

output$download3_2 <-downloadHandler(
  filename = function() {
    paste("DE_gene",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    readr::write_csv(datacontent3_2(),file)},#,row.names = FALSE)}
  contentType = "csv"
)

############################volcano plot
plot3_1 <- reactive({
  
  table3_3 <- datacontent3_2()
  professional_colors <- c("Down" = "#377EB8", "Stable" = "#E41A1C", "Up" = "#4DAF4A")
  
  ggplot(table3_3, aes(x=logFC, y=-log10(adj.P.Val))) +
    geom_hline(yintercept = -log10(0.05), linetype = 'dashed', size = 0.5, color = "grey60") +
    geom_vline(xintercept = c(-1,1), linetype = 'dashed', size = 0.5, color = "grey60") +
    geom_point(aes(color = g), size = 2.5, alpha = 0.7) +
    scale_color_manual(values = professional_colors) +
    labs(title = "Volcano Plot", x = "Log Fold Change (logFC)", y = "-Log10 P-value", colour = "Group") +
    theme_classic(base_family = "Arial") +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(size = 28, hjust = 0.5, margin = margin(b = 80, unit = "pt")),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 18),
      legend.position = "right",
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 16),
      legend.key.size = unit(3, "lines"),
      plot.margin = margin(1.5, 1, 1.5, 1, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
})

observeEvent(input$action3_2, {
  output$plot3_1 <- renderPlot({
    plot3_1()
  })
})

output$download_plot3_1 <- downloadHandler(
  filename = function() {
    paste("Volcano_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot3_1(), device = "png",width = 8, height = 6, units = "in", dpi = 300)
  }
)
############################################PCA analysis
sample_data4_1 <- reactiveVal(NULL)

observeEvent(input$sample_data4_1, {
  if (input$sample_data4_1) {
    load("4.PCA_sample.RData")
    sample_data4_1(sample_iris)
    updateCheckboxInput(session, "own_data4_1", value = FALSE)
  }
})

observeEvent(input$own_data4_1, {
  if (input$own_data4_1) {
    updateCheckboxInput(session, "sample_data4_1", value = FALSE)
  }
})

output$upload_options4_1 <- renderUI({
  if (input$own_data4_1) {
    fileInput("file4_1", "Choose csv file", accept = ".csv")
  }
})

own_data4_1 <- reactive({
  req(input$file4_1)
  readr::read_csv(input$file4_1$datapath)
})

output$table4_1 <- DT::renderDataTable({
  if (input$sample_data4_1) {
    sample_data4_1()
  } else if (input$own_data4_1 && !is.null(input$file4_1)) {
    own_data4_1()
  } else {
    NULL
  }
},options = list(pageLength = 5,scrollX = TRUE))

output$conditionalText_table4_1 <- renderUI({
  if (input$sample_data4_1 || !is.null(input$file4_1)) {
    h3("Uploaded data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

data_pca_reactive <- reactiveVal()
pca_result <- reactiveVal()

observeEvent(input$action4_1, {
  data <- if (input$sample_data4_1) {
    sample_data4_1()
  } else if (input$own_data4_1 && !is.null(input$file4_1)) {
    own_data4_1()
  } else {
    return(NULL)
  }
  
  data4_1 <- data %>%
    mutate(across(!c(Group, Sample), as.numeric))
  
  data_pca <- PCA(dplyr::select(data4_1, -c(Group, Sample)), graph = FALSE)
  pca_result(data_pca)
  
  pca_scores <- data_pca$ind$coord %>%
    data.frame() %>%
    mutate(Group = data4_1$Group) %>% dplyr::select(Group,everything())
  
  
  data_pca_reactive(pca_scores)
  
  output$table4_2 <- renderDataTable({
    # pca_scores
    pca_scores <- pca_scores %>%
      mutate(across(where(is.numeric), ~ ifelse(. < 0.01, formatC(., format = "e", digits = 2), round(., 5))))
    pca_scores
  }, options = list(pageLength = 5))
  
  groups <- unique(pca_scores$Group)
})

output$conditionalText_table4_2 <- renderUI({
  if (input$action4_1) {
    h3("The PCA result is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})


output$download4_1 <- downloadHandler(
  filename = function() {
    paste("PCA_result", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data_pca_reactive(), file)
  }
)

plot4_1 <- reactive({
  table4_3 <- data_pca_reactive()
  data_pca <- pca_result()
  
  groups <- c("case","control")
  colors <- c("red","blue")
  
  ggplot(table4_3, aes(x = Dim.1, y = Dim.2, color =factor(Group) )) +#
    geom_point(size = 4) +
    stat_ellipse(type = "t", linetype = 2) +
    labs(
      x = paste("Dim 1 (", round(data_pca$eig[1, 2], 2), "%)", sep = ""),
      y = paste("Dim 2 (", round(data_pca$eig[2, 2], 2), "%)", sep = "")
    ) +
    # labs(title = "PCA plot") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = colors,name = "Group") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)+
    xlab("PC-1") +  
    ylab("PC-2")
})

observeEvent(input$action4_2, {
  output$plot4_1 <- renderPlot({
    plot4_1()
  })
})

output$download_plot4_1 <- downloadHandler(
  filename = function() {
    paste("PCA_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot4_1(), device = "png",width = 8, height = 6, units = "in", dpi = 300)
  }
)

##################################################PLS-DA
sample_data5_1 <- reactiveVal(NULL)

observeEvent(input$sample_data5_1, {
  if (input$sample_data5_1) {
    load("4.PCA_sample.RData")
    sample_data5_1(sample_iris)
    updateCheckboxInput(session, "own_data5_1", value = FALSE)
  }
})

observeEvent(input$own_data5_1, {
  if (input$own_data5_1) {
    updateCheckboxInput(session, "sample_data5_1", value = FALSE)
  }
})

output$upload_options5_1 <- renderUI({
  if (input$own_data5_1) {
    fileInput("file5_1", "Choose csv file", accept = ".csv")
  }
})

own_data5_1 <- reactive({
  req(input$file5_1)
  readr::read_csv(input$file5_1$datapath)
})

output$table5_1 <- DT::renderDataTable({
  if (input$sample_data5_1) {
    sample_data5_1()
  } else if (input$own_data5_1 && !is.null(input$file5_1)) {
    own_data5_1()
  } else {
    NULL
  }
},options = list(pageLength = 5))

output$conditionalText_table5_1 <- renderUI({
  if (input$sample_data5_1 || !is.null(input$file5_1)) {
    h3("Uploaded data is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

data_plsda_reactive <- reactiveVal()
plsda_result <- reactiveVal()

observeEvent(input$action5_1,{
  data <- if (input$sample_data5_1) {
    sample_data5_1()
  } else if (input$own_data5_1 && !is.null(input$file5_1)) {
    own_data5_1()
  } else {
    return(NULL)
  }
  
  data5_1 <- data %>% dplyr::mutate(across(!c(Group, Sample), as.numeric))
  
  plsda_result <- plsda(dplyr::select(data5_1, -c("Group", "Sample")),
                        data5_1$Group, ncomp = 2)
  
  plsda_scores <- plsda_result$variates$X %>%
    data.frame()%>%
    mutate(Group = data5_1$Group)
  
  data_plsda_reactive(plsda_scores)
  
  output$table5_2 <- renderDataTable({
    data <- as.data.frame(plsda_scores)
    
    data <- data %>%
      mutate(across(where(is.numeric), ~ ifelse(. < 0.01, formatC(., format = "e", digits = 2), round(., 5))))
    data
  }, options = list(pageLength = 5))
  
  output$conditionalText_table5_2 <- renderUI({
      h3("The PLS-DA result is shown below.", class = "light-gray-text", style = "text-align: center;")
  })
  
  
  groups <- unique(plsda_scores$Group)
})

output$download5_1 <- downloadHandler(
  filename = function() {
    paste("PLSDA_result", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data_plsda_reactive(), file)
  }
)

plot5_1 <- reactive({
  table5_2 <- data_plsda_reactive()
  data_plsda <- plsda_result()
  
  groups <- c("case","control")
  colors <-c("red","blue") 
    
  ggplot(table5_2, aes(x = comp1, y = comp2, color = factor(Group))) +
    geom_point(size = 4) +
    stat_ellipse(type = "t", linetype = 2) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values = colors) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0)+
    xlab("PLS-DA-1") +  
    ylab("PLS-DA-2")
})


observeEvent(input$action5_2, {
  output$plot5_1 <- renderPlot({
    plot5_1()
  })
})

output$download_plot5_1 <- downloadHandler(
  filename = function() {
    paste("PLS-DA_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, plot = plot5_1(), device = "png", width = 8, height = 6, units = "in", dpi = 300)
  }
)
#################################################enrichment analysis
sample_data6_1 <- reactiveVal(NULL)

observeEvent(input$sample_data6_1, {
  if (input$sample_data6_1) {
    load("6.sample_gene.RData")
    sample_data6_1(sample_gene)
    updateCheckboxInput(session, "own_data6_1", value = FALSE)
  }
})


output$upload_options6_1 <- renderUI({
  if (input$own_data6_1) {
    tagList(
      conditionalPanel(
        condition = "input.gene_enrich == true",
        fileInput("file6_1_gene", "Choose gene csv file", accept = ".csv")
        ),
      
      conditionalPanel(
        condition = "input.protein_enrich == true",
        fileInput("file6_1_protein", "Choose protein csv file", accept = ".csv")
      ),
      
      conditionalPanel(
        condition = "input.metabolite_enrich == true",
        fileInput("file6_1_metabolite", "Choose metabolite csv file", accept = ".csv")
      )
    )
  }
})

own_data_gene <- reactive({
  req(input$file6_1_gene)
  readr::read_csv(input$file6_1_gene$datapath)
})

own_data_protein <- reactive({
  req(input$file6_1_protein)
  readr::read_csv(input$file6_1_protein$datapath)
})

own_data_metabolite <- reactive({
  req(input$file6_1_metabolite)
  readr::read_csv(input$file6_1_metabolite$datapath)
})

observeEvent(input$own_data6_1, {
  if (input$own_data6_1) {
    updateCheckboxInput(session, "sample_data6_1", value = FALSE)
  }
})


# Data display logic in the table
output$table6_1 <- DT::renderDataTable({
  if (input$sample_data6_1) {
    sample_data6_1()
  } else if (input$own_data6_1) {
    if (input$gene_enrich && !is.null(input$file6_1_gene)) {
      own_data_gene()
    } else if (input$protein_enrich && !is.null(input$file6_1_protein)) {
      own_data_protein()
    } else if (input$metabolite_enrich && !is.null(input$file6_1_metabolite)) {
      own_data_metabolite()
    }
  } else {
    NULL
  }
}, options = list(pageLength = 5))

##############GO
plot_enrich_result <- function(enrich_result) {
  barplot(enrich_result, showCategory = 20)
}

get_identifiers <- reactive( {
  if (input$sample_data6_1) {
    sample_data6_1()$Name
  } else if (input$own_data6_1) {
    if (input$gene_enrich && !is.null(input$file6_1_gene)) {
      own_data_gene()$Name
    } else if (input$protein_enrich && !is.null(input$file6_1_protein)) {
      own_data_protein()$Name
    } else if (input$metabolite_enrich && !is.null(input$file6_1_metabolite)) {
      own_data_metabolite()$Name  # Assuming metabolite data has a 'Name' column
    }
  } else {
    NULL
  }
})

##############################
enrich_data <- reactive({
  if(!is.null(input$gene_enrich)){
    data <- get_identifiers()
  }
  if(!is.null(input$protein_enrich)){
    data <- All_edge %>% 
      dplyr::filter(`Association Type`=="gene-enzyme") %>%
      dplyr::select(`Source ID`)
    colnames(data) <- "Name"
    data <- data$Name
  }else if(!is.null(input$metabolite_enrich)){
    data <- All_edge %>% 
      dplyr::filter(`Association Type`=="gene-metabolite") %>%
      dplyr::select(`Source ID`)
    colnames(data) <- "Name"
    data <- data$Name
  }
  return(data)
})

enrichment_GO <- function() {
  enrich_data <- enrich_data()
  enrichment_result <- enrichGO(
    gene = enrich_data,
    OrgDb = org.Hs.eg.db,
    keyType = "SYMBOL",  # 确保使用正确的keyType
    ont = input$tab6_1_1,
    pAdjustMethod = input$tab6_1_2,
    pvalueCutoff = as.numeric(input$tab6_1_3),
    qvalueCutoff = input$tab6_1_4
  )
  return(enrichment_result)
}


observeEvent(input$tab6_1_5, {
  output$progressText <- renderText("Performing GO enrichment analysis...")

  enrichment_result <- enrichment_GO()
  output$progressText <- renderText("Processing results...")
  
  invalidateLater(1000, session)
  
  output$table6_1_1 <- DT::renderDataTable({
    if (!is.null(enrichment_result)) {
      as.data.frame(enrichment_result)
    }
  }, options = list(pageLength = 5))

  output$plot6_1_1 <- renderPlot({
    if (!is.null(enrichment_result)) {
      plot_enrich_result(enrichment_result)
    }
  })

  output$download_table6_1_1 <- downloadHandler(
    filename = function() {
      paste("GO_analysis", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(enrichment_result)) {
        write.csv(as.data.frame(enrichment_result), file)
      }
    }
  )

  output$download_plot6_1_1 <- downloadHandler(
    filename = function() {
      paste("GO_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (!is.null(enrichment_result)) {
        P6_1 <- plot_enrich_result(enrichment_result)
        ggsave(file, plot = P6_1, device = "png", width = 10, height = 8, dpi = 300)
      }
    }
  )
  output$progressText <- renderText("Analysis complete!")
  })


##############KEGG
enrichment_KEGG <- eventReactive(input$tab6_2_5, {
  withProgress(message = 'Performing KEGG enrichment analysis...', value = 0, {
    
  enrich_data<- enrich_data()
  setProgress(value = 0.5, message = 'Processing results...')
  
    diffgene<-bitr(enrich_data,fromType = "SYMBOL",toType = "ENTREZID",OrgDb = "org.Hs.eg.db")
    genes_entrez <- diffgene$ENTREZID
  
  
  enrichKEGG(gene = genes_entrez,
           organism = "hsa",
           keyType = "kegg", 
           pAdjustMethod = input$tab6_2_2,
           pvalueCutoff = as.numeric(input$tab6_2_3),
           qvalueCutoff = as.numeric(input$tab6_2_4))
})

output$table6_2_1 <- DT::renderDataTable({
  if (input$tab6_2_5 > 0) {
    enrich_result <- enrichment_KEGG()
    as.data.frame(enrich_result)
  }
}, options = list(pageLength = 5))

output$download_table6_2_1 <- downloadHandler(
  filename = function() {
    paste("KEGG_analysis", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    enrich_result <- enrichment_KEGG()
    if (!is.null(enrich_result)) {
      write.csv(as.data.frame(enrich_result), file)
    }
  }
)

output$plot6_2_1 <- renderPlot({
  enrich_result <- enrichment_KEGG()
  if (!is.null(enrich_result)) {
    plot_enrich_result(enrich_result)
  }
})

output$download_plot6_2_1 <- downloadHandler(
  filename = function() {
    paste("KEGG_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    enrich_result <- enrichment_KEGG()
    P6_2 <- plot_enrich_result(enrich_result)
    ggsave(file, plot = P6_2, device = "png", width = 10, height = 8, dpi = 300)
  }
)
})

