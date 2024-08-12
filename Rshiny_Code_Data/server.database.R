###SERVER.database

observeEvent(input$construct, {
  print("Workflowplot")
})


datasetInput1_1 <- reactive({
  database1 <- input$tab1_4
  if (database1=="breast cancer"){
    load("1.database_breast_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="lung cancer"){
    load("1.database_lung_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="prostate cancer"){
    load("1.database_prostate_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="hematologic cancer"){
    load("1.database_hematologic_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="liver cancer"){
    load("1.database_liver_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="stomach cancer"){
    load("1.database_stomach_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="kidney cancer"){
    load("1.database_kidney_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="colon cancer"){
    load("1.database_colon_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="ovarian cancer"){
    load("1.database_ovarian_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="esophageal cancer"){
    load("1.database_esophageal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="pancreatic cancer"){
    load("1.database_pancreatic_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="urinary bladder cancer"){
    load("1.database_urinary_bladder_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="uterine cancer"){
    load("1.database_uterine_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="bone cancer"){
    load("1.database_bone_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="thyroid cancer"){
    load("1.database_thyroid_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="head and neck cancer"){
    load("1.database_head_and_neck_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="brain cancer"){
    load("1.database_brain_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="cervical cancer"){
    load("1.database_cervical_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="skin cancer"){
    load("1.database_skin_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="larynx cancer"){
    load("1.database_larynx_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="bile duct cancer"){
    load("1.database_bile_duct_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="germ cell cancer"){
    load("1.database_germ_cell_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="adrenal gland cancer"){
    load("1.database_adrenal_gland_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="ocular cancer"){
    load("1.database_ocular_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="muscle cancer"){
    load("1.database_muscle_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="testicular cancer"){
    load("1.database_testicular_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="rectum cancer"){
    load("1.database_rectum_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="gallbladder cancer"){
    load("1.database_gallbladder_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="salivary gland cancer"){
    load("1.database_salivary_gland_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="thymus cancer"){
    load("1.database_thymus_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="pharynx cancer"){
    load("1.database_pharynx_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="vulva cancer"){
    load("1.database_vulva_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="lymphatic system cancer"){
    load("1.database_lymphatic_system_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="peritoneum cancer"){
    load("1.database_peritoneum_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="duodenum cancer"){
    load("1.database_duodenum_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="appendix cancer"){
    load("1.database_appendix_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="spinal cancer"){
    load("1.database_spinal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="penile cancer"){
    load("1.database_penile_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="vascular cancer"){
    load("1.database_vascular_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="vaginal cancer"){
    load("1.database_vaginal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="fallopian tube cancer"){
    load("1.database_fallopian_tube_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="tracheal cancer"){
    load("1.database_tracheal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="mediastinal cancer"){
    load("1.database_mediastinal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="retroperitoneal cancer"){
    load("1.database_retroperitoneal_cancer.RData")
    return(b1=ALL_2  )}
  if (database1=="ureter cancer"){
    load("1.database_ureter_cancer.RData")
    return(b1=ALL_2  )}
  else{
    return(b1 )
  }
})

datasetInput1 <- reactive({
  datasetInput1_1() %>%
    {data <- .; list(
      data_with_no_na = data %>% filter(complete.cases(.)) %>% arrange(`Cancer`),
      data_with_na = data %>% filter(!complete.cases(.))
    )} %>%
    {rbind(.$data_with_no_na, .$data_with_na)} %>%
    mutate(
      Gene = paste0('<a href="https://www.ncbi.nlm.nih.gov/gene/?term=', Gene, '/entry" target="_blank">', Gene, '</a>'),
      Uniprot = paste0('<a href="https://www.uniprot.org/uniprotkb/', Uniprot, '/entry" target="_blank">', Uniprot, '</a>'),
      `HMDB ID`=paste0('<a href="https://hmdb.ca/metabolites/', `HMDB ID`, '/entry" target="_blank">', `HMDB ID`, '</a>')
    )
})




output$table1 <- DT::renderDataTable({
  data <- datasetInput1()[, -ncol(datasetInput1())]  
  data <- data[sample(nrow(data)), ]  
  
  datatable(data=data,
            escape = FALSE, 
            options=list(pageLength = 5)
            )})

output$download1 <-downloadHandler(
  filename = function() {
    paste("basic_data",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    original_data <- datasetInput1()
    original_data$Gene <- gsub('<a href="[^"]+" target="_blank">([^<]+)</a>', '\\1', original_data$Gene)
    original_data$Uniprot <- gsub('<a href="[^"]+" target="_blank">([^<]+)</a>', '\\1', original_data$Uniprot)
    original_data$`HMDB ID` <- gsub('<a href="[^"]+" target="_blank">([^<]+)</a>', '\\1', original_data$`HMDB ID`)
    write.csv(original_data, file, row.names = FALSE)}
  )

feature_met$link1_1 <- paste0('<a href="https://hmdb.ca/metabolites/', feature_met$`HMDB ID`, '" target="_blank">', feature_met$`HMDB ID`, '</a>')
feature_met$`HMDB ID` <- feature_met$link1_1

output$table1_1 <- DT::renderDataTable({
  datatable(data=feature_met[, -ncol(feature_met)],escape = FALSE, options=list(pageLength = 5,scrollX = TRUE)
            
  )})

output$download1_1 <-downloadHandler(
  filename = function() {
    paste("feature_met",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    original_feature_met <- feature_met
    original_feature_met$HMDB <- gsub('<a href="[^"]+" target="_blank">([^<]+)</a>', '\\1', feature_met$HMDB)
    write.csv(original_feature_met[, -which(names(feature_met) == "link1_1")], file, row.names = FALSE)
    }
)

protein$link1_2 <- paste0('<a href="https://www.uniprot.org/uniprotkb/', protein$UniProt, '/entry" target="_blank">', protein$UniProt, '</a>')
protein$UniProt <- protein$link1_2

output$table1_2 <- DT::renderDataTable({
  datatable(data=protein[, -ncol(protein)],escape = FALSE, options=list(pageLength = 5,scrollX = TRUE)
  
  )})
output$download1_2 <-downloadHandler(
  filename = function() {
    paste("protein",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    original_protein <- protein
    original_protein$UniProt <- gsub('<a href="[^"]+" target="_blank">([^<]+)</a>', '\\1', protein$UniProt)
    write.csv(original_protein[, -which(names(original_protein) == "link1_2")],file)}#,row.names = FALSE)}
)


output$image2_1 <- renderPlot({
  category_data <- protein %>%
    group_by(Category) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = Count / sum(Count)) %>%
    arrange(desc(Percentage))
  
  colors <- brewer.pal(n = n_distinct(category_data$Category), name = "Paired")
  
  ggplot(category_data, aes(x = "", y = Percentage, fill = Category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 14)
    ) 

})
