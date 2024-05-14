datacontent3_1 <- reactive({
  infile3_1=input$file3_1
  if (is.null(infile3_1))
    return(dat_pro)
  input_table1=readr::read_delim(infile3_1$datapath,delim =input$sep,escape_double = FALSE, trim_ws = TRUE)
  return(input_table1)
})
output$table3_1 <- DT::renderDataTable({
  data=datacontent3_1()},options = list(pageLength = 5))


datacontent3_2 <- reactive({
  infile3_2=input$file3_2
  if (is.null(infile3_2))
    return(group)
  input_table2=readr::read_delim(infile3_2$datapath,delim =input$sep,escape_double = FALSE, trim_ws = TRUE)
  
  # updateSelectInput(session,"colname3_1",choices = unique(input_table2$group))
  # updateSelectInput(session,"colname3_2",choices = unique(input_table2$group))
  return(input_table2)
})
output$table3_2 <- DT::renderDataTable({
  data=datacontent3_2()},options = list(pageLength = 5))

########################################################################################

table3_3 <- eventReactive(input$action3_1, {
  
})

datacontent3_3 <- reactive({
  table3_3()
  
  dat3 <- datacontent3_1() 
  group3 <- datacontent3_2()
  
  dat3=data.frame(dat3,stringsAsFactors = FALSE)
  dat3 <- data.matrix(dat3)
  
  group3 <- group3[,1]
  
  design1=model.matrix(~0+factor(group3))
  
  colnames(design1)=c("treat","control")#levels(factor(group3))
  rownames(design1)=colnames(dat3)
  
  contrast.matrix1=makeContrasts(treat-control,
                                 levels = design1)
  
  fit1=lmFit(dat3,design1)
  fit1_2=eBayes(contrasts.fit(fit1,contrast.matrix1))
  temoutput1=limma::topTable(fit1_2,n=Inf,adjust = "fdr")
  temoutput1=na.omit(temoutput1)
  
  # temoutput2 <- dplyr::mutate(temoutput1,g=ifelse(temoutput1$adj.P.Val>= input$Pvalue3_1,"stable",
  #                                                 ifelse(temoutput1$logFC > input$logFC3_1,"up",
  #                                                        ifelse(temoutput1$logFC< -(input$logFC3_1),"down","stable"))))
  # 
  # 
  temoutput2 <- temoutput1
  
  temoutput2 <- dplyr::mutate(temoutput1,g=ifelse(temoutput1$adj.P.Val < input$Pvalue3_1 & abs(temoutput1$logFC) >= input$logFC3_1,
                                                  ifelse(temoutput1$logFC > input$logFC3_1 ,'Up','Down'),
                                                  'Stable'))  
  
  
  return(temoutput2)
})

output$table3_3 <- DT::renderDataTable({
  data=datacontent3_3()},options = list(pageLength = 5))

output$download3_3 <-downloadHandler(
  filename = function() {
    paste("DE_gene",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datacontent3_3(),file)}#,row.names = FALSE)}
)

output$summary_input <- renderPrint({
  xx=datacontent3_3()
  table(xx$g)
})

######################################################################
datacontent3_4 <- reactive({
  infile3_3=input$file3_3
  if (is.null(infile3_3))
    return(Volcano)
  input_table1=readr::read_delim(infile3_3$datapath,delim =input$sep,escape_double = FALSE, trim_ws = TRUE)
  return(input_table1)
})

action3_3 <- eventReactive(input$action3_3, {
})


plot3_1 <- reactive({
  action3_3()
  
  table3_4 <- datacontent3_4()
  
  professional_colors <- c("#377EB8", "#E41A1C", "#4DAF4A")
  
  ggplot(table3_4, aes(x=logFC, y=-log10(adj.P.Val))) +
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

output$plot3_3 <- renderPlot({
  plot3_1()
})
#################################################################################################################
datacontent4_1 <- reactive({
  infile4_1=input$file4_1
  if (is.null(infile4_1))
    return(diff1)
  input_table1=readr::read_delim(infile4_1$datapath,delim =input$sep,escape_double = FALSE, trim_ws = TRUE)
  return(input_table1)
})

output$table4_1 <- DT::renderDataTable({
  data=datacontent4_1()},options = list(pageLength = 10))

act4_1 <- eventReactive(input$action4_1, {
  
  
})

plot4_1 <- reactive({
  act4_1()
  
  
  diff <- datacontent4_1()
  diffgene<-bitr(diff$SYMBOL,fromType = "SYMBOL",toType = "ENTREZID",OrgDb = "org.Hs.eg.db")
  kk <- clusterProfiler::enrichKEGG(gene = diffgene$ENTREZID,
                                    keyType = "kegg",
                                    organism= "human", 
                                    qvalueCutoff = as.numeric(input$P4_1) , 
                                    pvalueCutoff=as.numeric(input$q4_1),
                                    pAdjustMethod = input$j4_1)
  
  return(kk)
})

table4_2 <- reactive({
  act4_1()
  
  kk2=plot4_1()
  kk3=summary(kk2)
  return(kk3)
  
})

output$table4_2 <- DT::renderDataTable({
  data=table4_2()},options = list(pageLength = 5))


act4_2 <- eventReactive(input$action4_2, {
  
})

output$plot4_1 <- renderPlot({
  act4_2()
  
  kkk <- plot4_1()
  ek.rt <- data.frame(kkk@result)
  
  ek.rt <- separate(data=ek.rt, col=GeneRatio, into = c("GR1", "GR2"), sep = "/") #劈分GeneRatio为2列（GR1、GR2）
  ek.rt <- separate(data=ek.rt, col=BgRatio, into = c("BR1", "BR2"), sep = "/") #劈分BgRatio为2列（BR1、BR2）
  ek.rt <- mutate(ek.rt, enrichment_factor = (as.numeric(GR1)/as.numeric(GR2))/(as.numeric(BR1)/as.numeric(BR2))) #计算Enrichment Factor
  
  ek.rt10 <- ek.rt %>% filter(row_number() >= 1,row_number() <= as.numeric(input$number4_1))
  
  gg <- ggplot(ek.rt10, aes(enrichment_factor, Description)) + 
    geom_point(aes(size=Count, color=p.adjust)) +
    scale_color_gradient(low="blue", high="red") + 
    labs(color="P Adjust", size="Count", x="Enrichment Factor", y="KEGG Term", title="KEGG Enrichment Analysis") + 
    theme_minimal() +
    theme(
      text = element_text(size=14), 
      axis.title = element_text(size=16, face="bold"), 
      axis.text = element_text(size=12), 
      legend.title = element_text(size=14), 
      legend.text = element_text(size=12),
      axis.text.x = element_text(size = 14),  
      axis.text.y = element_text(size = 14),
      plot.title = element_text(size=18, face="bold", hjust=0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  return(gg)
  
})

# ggsave("er.rt10_KEGG.pdf",width=6,height=4)

output$plot4_2 <- renderPlot({
  act4_2()
  
  kkk2 <- plot4_1()
  # hh <- as.data.frame(kkk2)
  
  hh_1=kkk2[1:as.numeric(input$number4_1),]
  
  # gg2 <- ggplot(hh_1,aes(y=Description,x=Count,fill=p.adjust))+
  #   geom_bar(stat = "identity",width=0.7)+####柱子宽度
  #   #coord_flip()+##颠倒横纵轴
  #   scale_fill_gradient(low = "red",high ="blue" )+#颜色自己可以换
  #   labs(title = "KEGG Pathways Enrichment",
  #        x = "Gene numbers",
  #        y = "Pathways")+
  #   theme(axis.title.x = element_text(face = "bold",size = 16),
  #         axis.title.y = element_text(face = "bold",size = 16),
  #         legend.title = element_text(face = "bold",size = 16))+
  #   theme_bw()
  gg2 <- ggplot(hh_1, aes(y=Description, x=Count, fill=p.adjust)) +
    geom_bar(stat = "identity", width=0.7) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "KEGG Pathways Enrichment", x = "Gene Numbers", y = "Pathways") +
    theme_minimal() +
    theme(
      text = element_text(size = 14),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),  # 增大横轴字体
      axis.text.y = element_text(size = 14),  # 增大纵轴字体
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
  return(gg2)
  
})

#######################
act4_3 <- eventReactive(input$action4_3, {
  
})

output$plot4_3 <- renderPlot({
  act4_3()
  datlog <- datacontent4_1()
  ek <- plot4_1()
  
  logFC <- datlog$logFC %>%as.data.frame()
  names(logFC) <- logFC$SYMBOL
  
  ek3 = setReadable(ek, #前面分析的结果
                    OrgDb = "org.Hs.eg.db", #人类数据库
                    keyType = "ENTREZID")
  
  p3 <- cnetplot(ek3, showCategory =as.numeric(input$number4_3), #选择top8的pathway ，这里也可以用包含pathway名称的向量
                 layout = input$P4_3, 
                 color.params = list(foldChange = logFC, #用上面的logFC值标注基因的颜色
                                     edge = T),
                 cex.params = list(category_node = 1, gene_node = 1, category_label = 1, gene_label = 1),
                 node_label ="all")
  return(p3)
  
})
