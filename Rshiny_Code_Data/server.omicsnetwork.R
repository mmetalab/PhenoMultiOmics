
################################################About
observeEvent(input$network1, {
  print("network1")
})

observeEvent(input$network2, {
  print("network2")
})

observeEvent(input$network3, {
  print("network3")
})
################################################omics network
input_data <- eventReactive(input$action2_2,{
  input_value <- input$tab2_1_3

  if (input_value=="breast cancer"){
    load("1.database_breast_cancer.RData")
    }
  if (input_value=="lung cancer"){
    load("1.database_lung_cancer.RData")
    }
  if (input_value=="prostate cancer"){
    load("1.database_prostate_cancer.RData")
    }
  if (input_value=="hematologic cancer"){
    load("1.database_hematologic_cancer.RData")
    }
  if (input_value=="liver cancer"){
    load("1.database_liver_cancer.RData")
    }
  if (input_value=="stomach cancer"){
    load("1.database_stomach_cancer.RData")
    }
  if (input_value=="kidney cancer"){
    load("1.database_kidney_cancer.RData")
   }
  if (input_value=="colon cancer"){
    load("1.database_colon_cancer.RData")
    }
  if (input_value=="ovarian cancer"){
    load("1.database_ovarian_cancer.RData")
    }
  if (input_value=="esophageal cancer"){
    load("1.database_esophageal_cancer.RData")
    }
  if (input_value=="pancreatic cancer"){
    load("1.database_pancreatic_cancer.RData")
    }
  if (input_value=="urinary bladder cancer"){
    load("1.database_urinary_bladder_cancer.RData")
    }
  if (input_value=="uterine cancer"){
    load("1.database_uterine_cancer.RData")
    }
  if (input_value=="bone cancer"){
    load("1.database_bone_cancer.RData")
    }
  if (input_value=="thyroid cancer"){
    load("1.database_thyroid_cancer.RData")
    }
  if (input_value=="head and neck cancer"){
    load("1.database_head_and_neck_cancer.RData")
    }
  if (input_value=="brain cancer"){
    load("1.database_brain_cancer.RData")
    }
  if (input_value=="cervical cancer"){
    load("1.database_cervical_cancer.RData")
    }
  if (input_value=="skin cancer"){
    load("1.database_skin_cancer.RData")
    }
  if (input_value=="larynx cancer"){
    load("1.database_larynx_cancer.RData")
    }
  if (input_value=="bile duct cancer"){
    load("1.database_bile_duct_cancer.RData")
    }
  if (database1=="germ cell cancer"){
    load("1.database_germ_cell_cancer.RData")
    }
  if (input_value=="adrenal gland cancer"){
    load("1.database_adrenal_gland_cancer.RData")
    }
  if (database1=="ocular cancer"){
    load("1.database_ocular_cancer.RData")
    }
  if (input_value=="muscle cancer"){
    load("1.database_muscle_cancer.RData")
    }
  if (database1=="testicular cancer"){
    load("1.database_testicular_cancer.RData")
    }
  if (input_value=="rectum cancer"){
    load("1.database_rectum_cancer.RData")
    }
  if (input_value=="gallbladder cancer"){
    load("1.database_gallbladder_cancer.RData")
    }
  if (input_value=="salivary gland cancer"){
    load("1.database_salivary_gland_cancer.RData")
    }
  if (input_value=="thymus cancer"){
    load("1.database_thymus_cancer.RData")
    }
  if (input_value=="pharynx cancer"){
    load("1.database_pharynx_cancer.RData")
    }
  if (input_value=="vulva cancer"){
    load("1.database_vulva_cancer.RData")
    }
  if (input_value=="lymphatic system cancer"){
    load("1.database_lymphatic_system_cancer.RData")
    }
  if (input_value=="peritoneum cancer"){
    load("1.database_peritoneum_cancer.RData")
    }
  if (input_value=="duodenum cancer"){
    load("1.database_duodenum_cancer.RData")
    }
  if (input_value=="appendix cancer"){
    load("1.database_appendix_cancer.RData")
    }
  if (input_value=="spinal cancer"){
    load("1.database_spinal_cancer.RData")
    }
  if (input_value=="penile cancer"){
    load("1.database_penile_cancer.RData")
    }
  if (input_value=="vascular cancer"){
    load("1.database_vascular_cancer.RData")
    }
  if (input_value=="vaginal cancer"){
    load("1.database_vaginal_cancer.RData")
    }
  if (input_value=="fallopian tube cancer"){
    load("1.database_fallopian_tube_cancer.RData")
    }
  if (input_value=="tracheal cancer"){
    load("1.database_tracheal_cancer.RData")
    }
  if (input_value=="mediastinal cancer"){
    load("1.database_mediastinal_cancer.RData")
    }
  if (input_value=="retroperitoneal cancer"){
    load("1.database_retroperitoneal_cancer.RData")
    }
  if (input_value=="ureter cancer"){
    load("1.database_ureter_cancer.RData")
   }

})


datacontent2 <- eventReactive(input$action2_2,{

  infile1=input$tab2_1_3
  if (!is.null(infile1) && infile1 != "") {
    
    if (infile1=="breast cancer"){
      load("2_1.dot_edge_breast cancer.RData")
      return(nodes_inva=`breast cancer_node`)
    }
    if (infile1=="lung cancer"){
      load("2_1.dot_edge_lung cancer.RData")
      return(nodes_inva=`lung cancer_node`)
    }
    if (infile1=="prostate cancer"){
      load("2_1.dot_edge_prostate cancer.RData")
      return(nodes_inva=`prostate cancer_node`)
    }
    if (infile1=="hematologic cancer"){
      load("2_1.dot_edge_hematologic cancer.RData")
      return(nodes_inva=`hematologic cancer_node`)
    }
    if (infile1=="liver cancer"){
      load("2_1.dot_edge_liver cancer.RData")
      return(nodes_inva=`liver cancer_node`)
    }
    if (infile1=="stomach cancer"){
      load("2_1.dot_edge_stomach cancer.RData")
      return(nodes_inva=`stomach cancer_node`)
    }
    if (infile1=="kidney cancer"){
      load("2_1.dot_edge_kidney cancer.RData")
      return(nodes_inva=`kidney cancer_node`)
    }
    if (infile1=="colon cancer"){
      load("2_1.dot_edge_colon cancer.RData")
      return(nodes_inva=`colon cancer_node`)
    }
    if (infile1=="ovarian cancer"){
      load("2_1.dot_edge_ovarian cancer.RData")
      return(nodes_inva=`ovarian cancer_node`)
    }
    if (infile1=="esophageal cancer"){
      load("2_1.dot_edge_esophageal cancer.RData")
      return(nodes_inva=`esophageal cancer_node`)
    }
    if (infile1=="pancreatic cancer"){
      load("2_1.dot_edge_pancreatic cancer.RData")
      return(nodes_inva=`pancreatic cancer_node`)
    }
    if (infile1=="urinary bladder cancer"){
      load("2_1.dot_edge_urinary bladder cancer.RData")
      return(nodes_inva=`urinary bladder cancer_node`)
    }
    if (infile1=="uterine cancer"){
      load("2_1.dot_edge_uterine cancer.RData")
      return(nodes_inva=`uterine cancer_node`)
    }
    if (infile1=="bone cancer"){
      load("2_1.dot_edge_bone cancer.RData")
      return(nodes_inva=`bone cancer_node`)
    }
    if (infile1=="thyroid cancer"){
      load("2_1.dot_edge_thyroid cancer.RData")
      return(nodes_inva=`thyroid cancer_node`)
    }
    if (infile1=="head and neck cancer"){
      load("2_1.dot_edge_colon cancer.RData")
      return(nodes_inva=`head and neck cancer_node`)
    }
    if (infile1=="brain cancer"){
      load("2_1.dot_edge_head and neck cancer.RData")
      return(nodes_inva=`brain cancer_node`)
    }
    if (infile1=="cervical cancer"){
      load("2_1.dot_edge_cervical cancer.RData")
      return(nodes_inva=`cervical cancer_node`)
    }
    if (infile1=="skin cancer"){
      load("2_1.dot_edge_skin cancer.RData")
      return(nodes_inva=`skin cancer_node`)
    }
    if (infile1=="larynx cancer"){
      load("2_1.dot_edge_larynx cancer.RData")
      return(nodes_inva=`larynx cancer_node`)
    }
    if (infile1=="bile duct cancer"){
      load("2_1.dot_edge_bile duct cancer.RData")
      return(nodes_inva=`bile duct cancer_node`)
    }
    if (infile1=="germ cell cancer"){
      load("2_1.dot_edge_germ cell cancer.RData")
      return(nodes_inva=`germ cell cancer_node`)
    }
    if (infile1=="adrenal gland cancer"){
      load("2_1.dot_edge_adrenal gland cancer.RData")
      return(nodes_inva=`adrenal gland cancer_node`)
    }
    if (infile1=="ocular cancer"){
      load("2_1.dot_edge_ocular cancer.RData")
      return(nodes_inva=`ocular cancer_node`)
    }
    if (infile1=="muscle cancer"){
      load("2_1.dot_edge_muscle cancer.RData")
      return(nodes_inva=`muscle cancer_node`)
    }
    if (infile1=="testicular cancer"){
      load("2_1.dot_edge_testicular cancer.RData")
      return(nodes_inva=`testicular cancer_node`)
    }
    if (infile1=="rectum cancer"){
      load("2_1.dot_edge_rectum cancer.RData")
      return(nodes_inva=`rectum cancer_node`)
    }
    if (infile1=="gallbladder cancer"){
      load("2_1.dot_edge_gallbladder cancer.RData")
      return(nodes_inva=`gallbladder cancer_node`)
    }
    if (infile1=="salivary gland cancer"){
      load("2_1.dot_edge_salivary gland cancer.RData")
      return(nodes_inva=`salivary gland cancer_node`)
    }
    if (infile1=="thymus cancer"){
      load("2_1.dot_edge_thymus cancer.RData")
      return(nodes_inva=`thymus cancer_node`)
    }
    if (infile1=="pharynx cancer"){
      load("2_1.dot_edge_pharynx cancer.RData")
      return(nodes_inva=`pharynx cancer_node`)
    }
    if (infile1=="vulva cancer"){
      load("2_1.dot_edge_vulva cancer.RData")
      return(nodes_inva=`vulva cancer_node`)
    }
    if (infile1=="lymphatic system cancer"){
      load("2_1.dot_edge_lymphatic system cancer.RData")
      return(nodes_inva=`lymphatic system cancer_node`)
    }
    if (infile1=="peritoneum cancer"){
      load("2_1.dot_edge_peritoneum cancer.RData")
      return(nodes_inva=`peritoneum cancer_node`)
    }
    if (infile1=="duodenum cancer"){
      load("2_1.dot_edge_duodenum cancer.RData")
      return(nodes_inva=`duodenum cancer_node`)
    }
    if (infile1=="appendix cancer"){
      load("2_1.dot_edge_appendix cancer.RData")
      return(nodes_inva=`appendix cancer_node`)
    }
    if (infile1=="spinal cancer"){
      load("2_1.dot_edge_spinal cancer.RData")
      return(nodes_inva=`spinal cancer_node`)
    }
    if (infile1=="penile cancer"){
      load("2_1.dot_edge_penile cancer.RData")
      return(nodes_inva=`penile cancer_node`)
    }
    if (infile1=="vascular cancer"){
      load("2_1.dot_edge_vascular cancer.RData")
      return(nodes_inva=`vascular cancer_node`)
    }
    if (infile1=="vaginal cancer"){
      load("2_1.dot_edge_vaginal cancer.RData")
      return(nodes_inva=`vaginal cancer_node`)
    }
    if (infile1=="fallopian tube cancer"){
      load("2_1.dot_edge_fallopian tube cancer.RData")
      return(nodes_inva=`fallopian tube cancer_node`)
    }
    if (infile1=="tracheal cancer"){
      load("2_1.dot_edge_tracheal cancer.RData")
      return(nodes_inva=`tracheal cancer_node`)
    }
    if (infile1=="mediastinal cancer"){
      load("2_1.dot_edge_mediastinal cancer.RData")
      return(nodes_inva=`mediastinal cancer_node`)
    }
    if (infile1=="retroperitoneal cancer"){
      load("2_1.dot_edge_retroperitoneal cancer.RData")
      return(nodes_inva=`retroperitoneal cancer_node`)
    }
    if (infile1=="ureter cancer"){
      load("2_1.dot_edge_ureter cancer.RData")
      return(nodes_inva=`ureter cancer_node`)
    }
    }
})


datacontent3 <-  eventReactive(input$action2_2,{

  infile2=input$tab2_1_3

  if (!is.null(infile2)&& infile2 != "") {
    
    if (infile2=="breast cancer"){
      load("2_1.dot_edge_breast cancer.RData")
      return(links_inva=`breast cancer_edge`)
    }
    if (infile2=="lung cancer"){
      load("2_1.dot_edge_lung cancer.RData")
      return(links_inva=`lung cancer_edge`)
    }
    if (infile2=="prostate cancer"){
      load("2_1.dot_edge_prostate cancer.RData")
      return(links_inva=`prostate cancer_edge`)
    }
    if (infile2=="hematologic cancer"){
      load("2_1.dot_edge_hematologic cancer.RData")
      return(links_inva=`hematologic cancer_edge`)
    }
    if (infile2=="liver cancer"){
      load("2_1.dot_edge_liver cancer.RData")
      return(links_inva=`liver cancer_edge`)
    }
    if (infile2=="stomach cancer"){
      load("2_1.dot_edge_stomach cancer.RData")
      return(links_inva=`stomach cancer_edge`)
    }
    if (infile2=="kidney cancer"){
      load("2_1.dot_edge_kidney cancer.RData")
      return(links_inva=`kidney cancer_edge`)
    }
    if (infile2=="colon cancer"){
      load("2_1.dot_edge_colon cancer.RData")
      return(links_inva=`colon cancer_edge`)
    }
    if (infile2=="ovarian cancer"){
      load("2_1.dot_edge_ovarian cancer.RData")
      return(links_inva=`ovarian cancer_edge`)
    }
    if (infile2=="esophageal cancer"){
      load("2_1.dot_edge_esophageal cancer.RData")
      return(links_inva=`esophageal cancer_edge`)
    }
    if (infile2=="pancreatic cancer"){
      load("2_1.dot_edge_pancreatic cancer.RData")
      return(links_inva=`pancreatic cancer_edge`)
    }
    if (infile2=="urinary bladder cancer"){
      load("2_1.dot_edge_urinary bladder cancer.RData")
      return(links_inva=`urinary bladder cancer_edge`)
    }
    if (infile2=="uterine cancer"){
      load("2_1.dot_edge_uterine cancer.RData")
      return(links_inva=`uterine cancer_edge`)
    }
    if (infile2=="bone cancer"){
      load("2_1.dot_edge_bone cancer.RData")
      return(links_inva=`bone cancer_edge`)
    }
    if (infile2=="thyroid cancer"){
      load("2_1.dot_edge_thyroid cancer.RData")
      return(links_inva=`thyroid cancer_edge`)
    }
    if (infile2=="head and neck cancer"){
      load("2_1.dot_edge_head and neck cancer.RData")
      return(links_inva=`head and neck cancer_edge`)
    }
    if (infile2=="brain cancer"){
      load("2_1.dot_edge_brain cancer.RData")
      return(links_inva=`brain cancer_edge`)
    }
    if (infile2=="cervical cancer"){
      load("2_1.dot_edge_cervical cancer.RData")
      return(links_inva=`cervical cancer_edge`)
    }
    if (infile2=="skin cancer"){
      load("2_1.dot_edge_skin cancer.RData")
      return(links_inva=`skin cancer_edge`)
    }
    if (infile2=="larynx cancer"){
      load("2_1.dot_edge_larynx cancer.RData")
      return(links_inva=`larynx cancer_edge`)
    }
    if (infile2=="bile duct cancer"){
      load("2_1.dot_edge_bile duct cancer.RData")
      return(links_inva=`bile duct cancer_edge`)
    }
    if (infile2=="germ cell cancer"){
      load("2_1.dot_edge_germ cell cancer.RData")
      return(links_inva=`germ cell cancer_edge`)
    }
    if (infile2=="adrenal gland cancer"){
      load("2_1.dot_edge_adrenal gland cancer.RData")
      return(links_inva=`adrenal gland cancer_edge`)
    }
    if (infile2=="ocular cancer"){
      load("2_1.dot_edge_ocular cancer.RData")
      return(links_inva=`ocular cancer_edge`)
    }
    if (infile2=="muscle cancer"){
      load("2_1.dot_edge_muscle cancer.RData")
      return(links_inva=`muscle cancer_edge`)
    }
    if (infile2=="testicular cancer"){
      load("2_1.dot_edge_testicular cancer.RData")
      return(links_inva=`testicular cancer_edge`)
    }
    if (infile2=="rectum cancer"){
      load("2_1.dot_edge_rectum cancer.RData")
      return(links_inva=`rectum cancer_edge`)
    }
    if (infile2=="gallbladder cancer"){
      load("2_1.dot_edge_gallbladder cancer.RData")
      return(links_inva=`gallbladder cancer_edge`)
    }
    if (infile2=="salivary gland cancer"){
      load("2_1.dot_edge_salivary gland cancer.RData")
      return(links_inva=`salivary gland cancer_edge`)
    }
    if (infile2=="thymus cancer"){
      load("2_1.dot_edge_thymus cancer.RData")
      return(links_inva=`thymus cancer_edge`)
    }
    if (infile2=="pharynx cancer"){
      load("2_1.dot_edge_pharynx cancer.RData")
      return(links_inva=`pharynx cancer_edge`)
    }
    if (infile2=="vulva cancer"){
      load("2_1.dot_edge_vulva cancer.RData")
      return(links_inva=`vulva cancer_edge`)
    }
    if (infile2=="lymphatic system cancer"){
      load("2_1.dot_edge_lymphatic system cancer.RData")
      return(links_inva=`lymphatic system cancer_edge`)
    }
    if (infile2=="peritoneum cancer"){
      load("2_1.dot_edge_peritoneum cancer.RData")
      return(links_inva=`peritoneum cancer_edge`)
    }
    if (infile2=="duodenum cancer"){
      load("2_1.dot_edge_duodenum cancer.RData")
      return(links_inva=`duodenum cancer_edge`)
    }
    if (infile2=="appendix cancer"){
      load("2_1.dot_edge_appendix cancer.RData")
      return(links_inva=`appendix cancer_edge`)
    }
    if (infile2=="spinal cancer"){
      load("2_1.dot_edge_spinal cancer.RData")
      return(links_inva=`spinal cancer_edge`)
    }
    if (infile2=="penile cancer"){
      load("2_1.dot_edge_penile cancer.RData")
      return(links_inva=`penile cancer_edge`)
    }
    if (infile2=="vascular cancer"){
      load("2_1.dot_edge_vascular cancer.RData")
      return(links_inva=`vascular cancer_edge`)
    }
    if (infile2=="vaginal cancer"){
      load("2_1.dot_edge_vaginal cancer.RData")
      return(links_inva=`vaginal cancer_edge`)
    }
    if (infile2=="fallopian tube cancer"){
      load("2_1.dot_edge_fallopian tube cancer.RData")
      return(links_inva=`fallopian tube cancer_edgeancer_node`)
    }
    if (infile2=="tracheal cancer"){
      load("2_1.dot_edge_tracheal cancer.RData")
      return(links_inva=`tracheal cancer_edge`)
    }
    if (infile2=="mediastinal cancer"){
      load("2_1.dot_edge_mediastinal cancer.RData")
      return(links_inva=`mediastinal cancer_edge`)
    }
    if (infile2=="retroperitoneal cancer"){
      load("2_1.dot_edge_retroperitoneal cancer.RData")
      return(links_inva=`retroperitoneal cancer_edge`)
    }
    if (infile2=="ureter cancer"){
      load("2_1.dot_edge_ureter cancer.RData")
      return(links_inva=`ureter cancer_edge`)
    }
  }
})


observeEvent(input$sample_data, {
  if (input$sample_data) {
    updateCheckboxInput(session, "own_data", value = FALSE)
  }
})

observeEvent(input$own_data, {
  if (input$own_data) {
    updateCheckboxInput(session, "sample_data", value = FALSE)
  }
})

output$upload_options <- renderUI({
  if (input$own_data) {
    fileInput("file3", "Choose csv file", accept = ".csv")
  }
})

sample_data <- sample_data

own_data <- reactive({
  req(input$file3)
  data <- readr::read_csv(input$file3$datapath)
  
  if (!"Abundance" %in% colnames(data)) {
    data <- data %>% mutate(Abundance = 1)
  }
  
  return(data)
})

output$table2_4 <- DT::renderDataTable({
  if (input$sample_data) {
    sample_data
  } else if (input$own_data && !is.null(input$file3)) {
    own_data()
  } else {
    NULL
  }
},options = list(pageLength = 5))

output$conditionalText2_4 <- renderUI({
  if ((input$sample_data) || (input$own_data)) {
    h3("Uploaded file is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

table2_4_data <- reactive({
  if (input$sample_data) {
    sample_data
  } else if (input$own_data && !is.null(input$file3)) {
    own_data()
  } else {
    NULL
  }
})

observeEvent(input$action2_3, {
  if (input$own_data && !is.null(input$file3)) {
    data <- own_data()
    if (!"Name" %in% colnames(data)) {
      showModal(modalDialog(
        title = "Error",
        "The uploaded file does not contain a 'Name' column. Please  upload the file again.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Success",
        "The uploaded file is valid and the node file and edge file are generating. Please click the blanck space for waiting.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  }
  
})

datacontent5 <- eventReactive(input$action2_3,{
  old <- datacontent3()  #datacontent3()edge
  upload_edge <- table2_4_data()
  if (is.null(upload_edge) || is.null(old)) {
    return(data.frame())
  }
  
  edges_select <- dplyr::filter(old, tolower(gsub(" ", "", old$Source)) %in% tolower(gsub(" ", "", upload_edge$Name)) | 
                                  tolower(gsub(" ", "", old$Target)) %in% tolower(gsub(" ", "", upload_edge$Name)))
  edges_select <- dplyr::distinct(edges_select,.keep_all=TRUE)
  return(edges_select)
})

output$table2_5 <- DT::renderDataTable({
  data=datacontent5()},options = list(pageLength = 5))

output$conditionalText2_5 <- renderUI({
  if (!is.null(datacontent5())) {
    h3("The edge table for Multi-Omics network is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

output$download2_5 <-downloadHandler(
  filename = function() {
    paste("edges",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datacontent5(),file,row.names = FALSE)}
)

datacontent6 <- eventReactive(input$action2_3,{
  old1 <- datacontent5()
  upload_node <- datacontent2()
  upload_orig <- table2_4_data()
  
  if (is.null(upload_node) || is.null(old1)) {
    return(data.frame())}  

  nodes_select <-dplyr::filter(upload_node,tolower(gsub(" ", "",upload_node$Name)) %in% tolower(gsub(" ", "",old1$Source)) | tolower(gsub(" ", "",upload_node$Name)) %in% tolower(gsub(" ", "",old1$Target)))
  nodes_select <- left_join(nodes_select, upload_orig[, c("Name", "Abundance")], by = c("Name"))
  nodes_select <- nodes_select %>% mutate(Abundance = replace_na(Abundance, 1))
  nodes_select <- dplyr::select(nodes_select,Name,everything())
  return(nodes_select)})

output$table2_6 <- DT::renderDataTable({
  data=datacontent6()},options = list(pageLength = 5))

output$conditionalText2_6 <- renderUI({
  if (!is.null(datacontent6())) {
    h3("The node table for Multi-Omics network is shown below.", class = "light-gray-text", style = "text-align: center;")
  }
})

output$download2_6 <-downloadHandler(
  filename = function() {
    paste("nodes",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datacontent6(),file,row.names = FALSE)}
)

# Hide download buttons if tables are not available
output$downloadButtonUI <- renderUI({
  downloadButtons <- tagList()
  if (!is.null(datacontent6()) && nrow(datacontent6()) > 0) {
    downloadButtons <- tagAppendChild(downloadButtons, downloadButton("download2_6", "Download node table"))
  }
  
  if (!is.null(datacontent5()) && nrow(datacontent5()) > 0) {
    downloadButtons <- tagAppendChild(downloadButtons, downloadButton("download2_5", "Download edge table"))
  }
  downloadButtons
})

#########################################
  network_data <- reactiveValues(nodes = NULL, edges = NULL)
  
  observeEvent(input$action2_1, {
    edges_new <- datacontent5() 
    nodes_new <- datacontent6()
    
    colnames(edges_new) <- c("from", "to")
    
    if (!"Name" %in% colnames(nodes_new)) {
      stop("The column 'Name' is missing in nodes_new")
    }
    
    nodes_new$Name <- as.character(nodes_new$Name)
    edges_new$from <- as.character(edges_new$from)
    edges_new$to <- as.character(edges_new$to)
    
    nodes_new$Name <- trimws(nodes_new$Name)
    edges_new$from <- trimws(edges_new$from)
    edges_new$to <- trimws(edges_new$to)
    
    net <- graph_from_data_frame(d = edges_new, vertices = nodes_new, directed = FALSE)
    nodesize <- igraph::degree(net, mode = "total")
    nodes_new$`Size degree` <- nodesize
    
    if (!"Abundance" %in% colnames(nodes_new)) {
      nodes_new$Abundance <- 1
    }
   
    new_min_size <- 10  
    new_max_size <- 30
    
    size_orig <- if (input$tab2_1_2 == "Degree") {
      nodes_new$`Size degree`
    } else {
      nodes_new$Abundance
    }
    
    scale_factor <- (new_max_size - new_min_size) / (max(size_orig) - min(size_orig))
    nodes_new$Size <- round((size_orig - min(size_orig)) * scale_factor + new_min_size, 2)
    
    Betweenness <- betweenness(net)
    Closeness <- closeness(net)
    Eigenvector <- eigen_centrality(net)$vector
    Pagerank <- page_rank(net)$vector
    
    nodes_new <- cbind(nodes_new,Betweenness,Closeness,Eigenvector,Pagerank)
    
    nodes_new <- nodes_new %>%
      mutate(across(where(is.numeric), ~ ifelse(round(., 3) < 0.01, 0, round(., 3))))
    
    network_data$edges <- edges_new
    network_data$nodes <- nodes_new
    

    output$table2_3 <- DT::renderDataTable({
      nodes_new
    }, options = list(pageLength = 5))
    
    output$download2_1 <- downloadHandler(
      filename = function() {
        paste("multi-omics", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(nodes_new, file)
      }
    )
    
    output$networkStats <- renderUI({
      Density <- edge_density(net)
      'Average path length' <- mean_distance(net, directed = TRUE)
      Diameter <- diameter(net)
      'Clustering coefficient' <- transitivity(net, type = "global")
      'Connected components' <- components(net)$no
      Assortativity <- assortativity_degree(net)
      
      HTML(paste0(
        "<div class='stats-container'>",
        "<div class='stats-header'>Network Summary Statistics</div>",
        "<p class='stats-item'><strong>Density:</strong> ",format(Density,digits=2), "</p>",
        "<p class='stats-item'><strong>Average path length:</strong> ", format('Average path length',digits=2), "</p>",
        "<p class='stats-item'><strong>Clustering coefficient:</strong> ", 'Clustering coefficient', "</p>",
        "<p class='stats-item'><strong>Diameter:</strong> ", Diameter, "</p>",
        "<p class='stats-item'><strong>Connected components:</strong> ", 'Connected components', "</p>",
        "<p class='stats-item'><strong>Assortativity:</strong> ", format(Assortativity,digits=2), "</p>",
        "</div>"
      ))
    })
    
  })

  output$networkid1 <- renderVisNetwork({
    req(network_data$nodes, network_data$edges)
    
    edges_new <- network_data$edges
    nodes_new <- network_data$nodes
    nodes_new$size <- nodes_new$Size
    nodes_new$title <- nodes_new$Name
    names(nodes_new)[names(nodes_new) == "Name"] <- "id"
    color <- c("#B62024", "#2E6E9E", "#D2B020")
    
    nodes_new$color.background <- ifelse(nodes_new$Group == "Gene", color[1], 
                                         ifelse(nodes_new$Group == "Metabolite", color[2], 
                                                ifelse(nodes_new$Group == "Enzyme", color[3], "#DDDDDD")))
    set.seed(142)
    visNetwork(nodes = nodes_new, edges = edges_new,  width = "100%", height = "700px", background = "white") %>%
      visNodes(shape = "dot", 
               size = nodes_new$size,
               color = list(background = nodes_new$color.background, border = "transparent", highlight = "black")) %>%
      visEdges(color = "#DDDDDD") %>%
      visOptions(width = "100%", height = "100%",
                 autoResize = FALSE,
                 highlightNearest = list(hover = TRUE, degree = 1, enabled = FALSE),
                 nodesIdSelection = list(enabled = TRUE, useLabels = TRUE,
                                         style = 'width: 200px; height: 26px; background: #f8f8f8; color: #2E6E9E; border: none; outline: none;'),
                 selectedBy = list(variable = "Group", highlight = TRUE, multiple = TRUE, highlight = TRUE,
                                   style = 'width: 200px; height: 26px; background: #f8f8f8; color: #2E6E9E; border: none; outline: none;'),
                 collapse = list(enabled = TRUE, fit = TRUE),
                 manipulation = TRUE) %>%
      visIgraphLayout(layout = input$tab2_1_1) %>%
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based",
                 barnesHut = list(avoidOverlap = 1, gravitationalConstant = -10000, centralGravity = 0.1),
                 hierarchicalRepulsion = list(avoidOverlap = 1),
                 stabilization = FALSE,
                 forceAtlas2Based = list(gravitationalConstant = -5)) %>%
      visEdges(smooth = FALSE) %>%
      visLayout(randomSeed = 301) %>%
      visLegend(addNodes = data.frame(label = c("Gene", "Metabolite", "Enzyme"), 
                                      color = color),
                useGroups = FALSE, 
                position = "right")%>%
    
    visExport(type = "jpeg", name = "export-network", float = "right", label = "Save network", background = "white", style = "")
  })
  

  