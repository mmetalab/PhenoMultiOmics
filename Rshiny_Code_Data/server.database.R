###SERVER.database

datasetInput1_1 <- reactive({
  database1 <- input$tab1_4
  if (database1=="Leukemia"){
    load("1.database_baixuebing.RData")
    return(b1=ALL_2  )}
  if (database1 == "Lung cancer") {
    load("1.database_feiai.RData") 
    return(b1=ALL_2)
  } 
  if (database1 == "Esophagus cancer") {
    load("1.database_shiguanai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Pancreatic cancer") {
    load("1.database_yixianai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Liver cancer") {
    load("1.database_ganai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Intestinal cancer") {
    load("1.database_changai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Bladder cancer") {
    load("1.database_pangguangai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Kidney cancer") {
    load("1.database_shenai.RData")
    return(b1=ALL_2)
  } 
  if (database1 == "Prostatic cancer") {
    load("1.database_qianliexian.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Metrocarcinoma") {
    load("1.database_zigongai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Throat cancer") {
    load("1.database_houai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Oral cancer") {
    load("1.database_kouqiangai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Skin cancer") {
    load("1.database_pifuai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Lymphoma cancer") {
    load("1.database_linbaai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Brain cancer") {
    load("1.database_naoai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Breast cancer") {
    load("1.database_ruxianai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Ovarian cancer") {
    load("1.database_luanchaoai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "testicular cancer") {
    load("1.database_gaowanai.RData")
    return(b1=ALL_2)
  }
  if (database1 == "Adrenal cancer") {
    load("1.database_shenshangxian.RData")
    return(b1=ALL_2)
  }else{
    return(b1)
  }
}
  
)

datasetInput1 <- reactive({
  data1=datasetInput1_1()
  # data2=data1 %>% dplyr::select(-any_of(c("Metabolite","HMDB ID"))) %>% dplyr::distinct(.keep_all = TRUE)
  data2 = data1 %>% dplyr::select(-any_of(c("Metabolite", "HMDB ID"))) %>% dplyr::distinct(.keep_all = TRUE)
  
  data=data2
    # dplyr::distinct(data1[,-"Metabolite"],.keep_all = TRUE)
  if(input$tab1_1!="All"){
    data=data[data$`Gene symbol`==input$tab1_1,]}
  if(input$tab1_2!="All"){
    data=data[data$Enzyme==input$tab1_2,]}
  if ("Metabolite" %in% names(data1)) {
    updateSelectInput(session, "tab1_3", choices = c("All", unique(data1$Metabolite)))
  }
  if(input$tab1_3!="All"){
  data=filter(data,str_detect(`Enzymatic reactions`,input$tab1_3))}
  
  data_with_no_na <- data %>%
    filter(complete.cases(.)) %>%
    arrange(`Disease ID`)
  data_with_na <- data2 %>%
    filter(!complete.cases(.))
  data_final <- rbind(data_with_no_na, data_with_na)
  data_final
})
  
output$table1 <-  DT::renderDataTable({
  datasetInput1()},options = list(pageLength = 5))

output$download1 <-downloadHandler(
  filename = function() {
    paste("basic_data",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datasetInput1(),file)}#,row.names = FALSE)}
  )

observe({
  updateSelectInput(session,"tab1_1",choices = c("All", unique(datasetInput1()$`Gene symbol`)))
  updateSelectInput(session,"tab1_2",choices = c("All", unique(datasetInput1()$Enzyme)))
})


output$table1_1 <- DT::renderDataTable({
  data=feature_met},options = list(pageLength = 5)
  )
output$download1_1 <-downloadHandler(
  filename = function() {
    paste("feature_met",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(feature_met[,1:4],file)}
)

output$table1_2 <- DT::renderDataTable({
  data=protein},options=list(pageLength = 5)
  )
output$download1_2 <-downloadHandler(
  filename = function() {
    paste("protein",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(protein,file)}#,row.names = FALSE)}
)


output$image2_1 <- renderPlot({
  # 统计 Category 列中各类别的频率并按照百分比降序排序
  category_data <- protein %>%
    group_by(Category) %>%
    summarise(Count = n()) %>%
    mutate(Percentage = Count / sum(Count)) %>%
    arrange(desc(Percentage))
  
  colors <- brewer.pal(n = n_distinct(category_data$Category), name = "Paired")
  
  # 绘制饼图
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
    ) +
    labs(title = "Distribution of Protein Categories", fill = "Category")

})

output$image2_2 <- renderPlot({
  protein <- protein %>%
    mutate(Length = as.numeric(as.character(Length)))
  
  # 绘制条形图
  ggplot(protein, aes(x = cut(Length, breaks = seq(min(Length, na.rm = TRUE), max(Length, na.rm = TRUE), by = 100)))) +
    geom_bar(fill = "#3074AE", color = "#10334A") +
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "#10334A"),
      axis.title.x = element_text(face = "bold", size = 16, color = "#10334A"),
      axis.title.y = element_text(face = "bold", size = 16, color = "#10334A"),
      axis.text.x = element_text(angle = 45, hjust = 1, color = "#10334A", size = 12),
      axis.text.y = element_text(color = "#10334A"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.position = "none"
    ) +
    labs(
      title = "Protein Length Distribution",
      x = "Protein Length Range",
      y = "Frequency"
    ) +
    scale_y_continuous(name = "Frequency", labels = scales::comma)
  
})

