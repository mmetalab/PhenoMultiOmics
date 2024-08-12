output$Download_sample_data1 <- downloadHandler(
  filename = function() {
    "Multi-Omics Network.csv"
  },
  content = function(file) {
    load("2.omicsnetwork.RData") 
   
    if (exists("sample_data")) {
      
      write.csv(sample_data, file, row.names = FALSE)
    } else {
      stop("Not found sample data")
    }
  }
)


output$Download_sample_data2 <- downloadHandler(
  filename = function() {
    "Multi-Omics Data Integrtion.zip"
  },
  content = function(file) {
    load("8.tsne.RData")
    
    gene_file <- tempfile(fileext = ".csv")
    protein_file <- tempfile(fileext = ".csv")
    metabolite_file <- tempfile(fileext = ".csv")
    
    write.csv(static_data_gene, gene_file, row.names = FALSE)
    write.csv(static_data_protein, protein_file, row.names = FALSE)
    write.csv(static_data_metabolite, metabolite_file, row.names = FALSE)
    zip::zipr(file, files = c(gene_file, protein_file, metabolite_file))
     
  }
)


output$Download_sample_data3 <- downloadHandler(
  filename = function() {
    "Univariate analysis.csv"
  },
  content = function(file) {
    load("3.DE_analysis.RData")
      write.csv(dat3_1, file, row.names = FALSE)
  }
)


output$Download_sample_data4 <- downloadHandler(
  filename = function() {
    "Statistical analysis.csv"
  },
  content = function(file) {
    load("4.PCA_sample.RData")

      write.csv(sample_iris, file, row.names = FALSE)
  }
)

output$Download_sample_data5 <- downloadHandler(
  filename = function() {
    "Functional analysis.csv"
  },
  content = function(file) {
    load("6.sample_gene.RData")
    
    write.csv(sample_gene, file, row.names = FALSE)
  }
)
