###SERVER.omicsnetwork

load("2.omicsnetwork.RData")


input_data <- eventReactive(input$action2_2,{
  input_value <- input$tab2_1_3
  
  if (input_value == "Leukemia") {
   load("2_1.dot_edge_baixuebing.RData")
  } 
  if (input_value == "Lung cancer") {
    load("2_1.dot_edge_feiai.RData")  
  } 
  if (input_value == "Esophagus cancer") {
    load("2_1.dot_edge_shiguanai.RData")
  } 
  if (input_value == "Pancreatic cancer") {
    load("2_1.dot_edge_yixianai.RData")
  } 
  if (input_value == "Liver cancer") {
    load("2_1.dot_edge_ganai.RData")
  } 
  if (input_value == "Intestinal cancer") {
    load("2_1.dot_edge_changai.RData")
  } 
  if (input_value == "Bladder cancer") {
    load("2_1.dot_edge_pangguangai.RData")
  } 
  if (input_value == "Kidney cancer") {
    load("2_1.dot_edge_shenai.RData")
  } 
  if (input_value == "Prostatic cancer") {
    load("2_1.dot_edge_qianliexian.RData")
  }
  if (input_value == "Metrocarcinoma") {
    load("2_1.dot_edge_zigongai.RData")
  }
  if (input_value == "Throat cancer") {
    load("2_1.dot_edge_houai.RData")
  }
  if (input_value == "Oral cancer") {
    load("2_1.dot_edge_kouqiangai.RData")
  }
  if (input_value == "Skin cancer") {
    load("2_1.dot_edge_pifuai.RData")
  }
  if (input_value == "Lymphoma cancer") {
    load("2_1.dot_edge_linbaai.RData")
  }
  if (input_value == "Brain cancer") {
    load("2_1.dot_edge_naoai.RData")
  }
  if (input_value == "Breast cancer") {
    load("2_1.dot_edge_ruxianai.RData")
  }
  if (input_value == "Ovarian cancer") {
    load("2_1.dot_edge_luanchaoai.RData")
  }
  if (input_value == "testicular cancer") {
    load("2_1.dot_edge_gaowanai.RData")
  }
  if (input_value == "Adrenal cancer") {
    load("2_1.dot_edge_shenshangxian.RData")
  }
  if (input_value == "Gastric cancer") {
   return(NULL)
  }
  
})
  
default_data2 <- reactive({
  
  return(id_all_stomach)
})

datacontent2 <- eventReactive(input$action2_2,{
  
  infile1=input$tab2_1_3
  
  if (!is.null(infile1) && infile1 != "") {
    
    if (infile1=="Leukemia"){
      load("2_1.dot_edge_baixuebing.RData")
      return(nodes_inva=dt_dot_baixuebing)
    }
    if (infile1 == "Lung cancer") {
      load("2_1.dot_edge_feiai.RData") 
      return(nodes_inva=dt_dot_feiai)
    } 
    if (infile1 == "Esophagus cancer") {
      load("2_1.dot_edge_shiguanai.RData")
      return(nodes_inva=dt_dot_shiguanai)
    } 
    if (infile1 == "Pancreatic cancer") {
      load("2_1.dot_edge_yixianai.RData")
      return(nodes_inva=dt_dot_yixianai)
    } 
    if (infile1 == "Liver cancer") {
      load("2_1.dot_edge_ganai.RData")
      return(nodes_inva=dt_dot_ganai)
    } 
    if (infile1 == "Intestinal cancer") {
      load("2_1.dot_edge_changai.RData")
      return(nodes_inva=dt_dot_changai)
    } 
    if (infile1 == "Bladder cancer") {
      load("2_1.dot_edge_pangguangai.RData")
      return(nodes_inva=dt_dot_pangguangai)
    } 
    if (infile1 == "Kidney cancer") {
      load("2_1.dot_edge_shenai.RData")
      return(nodes_inva=dt_dot_shenai)
    } 
    if (infile1 == "Prostatic cancer") {
      load("2_1.dot_edge_qianliexian.RData")
      return(nodes_inva=dt_dot_qianliexian)
    }
    if (infile1 == "Metrocarcinoma") {
      load("2_1.dot_edge_zigongai.RData")
      return(nodes_inva=dt_dot_zigongai)
    }
    if (infile1 == "Throat cancer") {
      load("2_1.dot_edge_houai.RData")
      return(nodes_inva=dt_dot_houai)
    }
    if (infile1 == "Oral cancer") {
      load("2_1.dot_edge_kouqiangai.RData")
      return(nodes_inva=dt_dot_kouqiangai)
    }
    if (infile1 == "Skin cancer") {
      load("2_1.dot_edge_pifuai.RData")
      return(nodes_inva=dt_dot_pifuai)
    }
    if (infile1 == "Lymphoma cancer") {
      load("2_1.dot_edge_linbaai.RData")
      return(nodes_inva=dt_dot_linbaai)
    }
    if (infile1 == "Brain cancer") {
      load("2_1.dot_edge_naoai.RData")
      return(nodes_inva=dt_dot_naoai)
    }
    if (infile1 == "Breast cancer") {
      load("2_1.dot_edge_ruxianai.RData")
      return(nodes_inva=dt_dot_ruxianai)
    }
    if (infile1 == "Ovarian cancer") {
      load("2_1.dot_edge_luanchaoai.RData")
      return(nodes_inva=dt_dot_luanchaoai)
    }
    if (infile1 == "testicular cancer") {
      load("2_1.dot_edge_gaowanai.RData")
      return(nodes_inva=dt_dot_gaowanai)
    }
    if (infile1 == "Adrenal cancer") {
      load("2_1.dot_edge_shenshangxian.RData")
      return(nodes_inva=dt_dot_shenshangxian)
    }}
    if (infile1 == "Gastric cancer") {
      return(nodes_inva=id_all_stomach)
    }
})

output$table2_1 <- DT::renderDataTable({
  if (input$action2_2 > 0) datacontent2() else default_data2()},options = list(pageLength = 5))


default_data3 <- reactive({
 
  return(edges_all_stomach)
})

datacontent3 <-  eventReactive(input$action2_2,{
    
  infile2=input$tab2_1_3
  
  if (!is.null(infile2)&& infile2 != "") {
    if (infile2=="Leukemia"){
      load("2_1.dot_edge_baixuebing.RData")
      return(links_inva=dt_edge_baixuebing)}
    if (infile2 == "Lung cancer") {
      load("2_1.dot_edge_feiai.RData") 
      return(links_inva=dt_edge_feiai)
    } 
    if (infile2 == "Esophagus cancer") {
      load("2_1.dot_edge_shiguanai.RData")
      return(links_inva=dt_edge_shiguanai)
    } 
    if (infile2 == "Pancreatic cancer") {
      load("2_1.dot_edge_yixianai.RData")
      return(links_inva=dt_edge_yixianai)
    } 
    if (infile2 == "Liver cancer") {
      load("2_1.dot_edge_ganai.RData")
      return(links_inva=dt_edge_ganai)
    } 
    if (infile2 == "Intestinal cancer") {
      load("2_1.dot_edge_changai.RData")
      return(links_inva=dt_edge_changai)
    } 
    if (infile2 == "Bladder cancer") {
      load("2_1.dot_edge_pangguangai.RData")
      return(links_inva=dt_edge_pangguangai)
    } 
    if (infile2 == "Kidney cancer") {
      load("2_1.dot_edge_shenai.RData")
      return(links_inva=dt_edge_shenai)
    } 
    if (infile2 == "Prostatic cancer") {
      load("2_1.dot_edge_qianliexian.RData")
      return(links_inva=dt_edge_qianliexian)
    }
    if (infile2 == "Metrocarcinoma") {
      load("2_1.dot_edge_zigongai.RData")
      return(links_inva=dt_edge_zigongai)
    }
    if (infile2 == "Throat cancer") {
      load("2_1.dot_edge_houai.RData")
      return(links_inva=dt_edge_houai)
    }
    if (infile2 == "Oral cancer") {
      load("2_1.dot_edge_kouqiangai.RData")
      return(links_inva=dt_edge_kouqiangai)
    }
    if (infile2 == "Skin cancer") {
      load("2_1.dot_edge_pifuai.RData")
      return(links_inva=dt_edge_pifuai)
    }
    if (infile2 == "Lymphoma cancer") {
      load("2_1.dot_edge_linbaai.RData")
      return(links_inva=dt_edge_linbaai)
    }
    if (infile2 == "Brain cancer") {
      load("2_1.dot_edge_naoai.RData")
      return(links_inva=dt_edge_naoai)
    }
    if (infile2 == "Breast cancer") {
      load("2_1.dot_edge_ruxianai.RData")
      return(links_inva=dt_edge_ruxianai)
    }
    if (infile2 == "Ovarian cancer") {
      load("2_1.dot_edge_luanchaoai.RData")
      return(links_inva=dt_edge_luanchaoai)
    }
    if (infile2 == "testicular cancer") {
      load("2_1.dot_edge_gaowanai.RData")
      return(links_inva=dt_edge_gaowanai)
    }
    if (infile2 == "Adrenal cancer") {
      load("2_1.dot_edge_shenshangxian.RData")
      return(links_inva=dt_edge_shenshangxian)
    }
    if (infile2 == "Gastric cancer") {
      return(links_inva=edges_all_stomach)
    }
  } 
})
output$table2_2 <- DT::renderDataTable({
  if (input$action2_2 > 0) datacontent3() else default_data3()},options = list(pageLength = 5))

datacontent4 <- reactive({
  infile3=input$file3
  if (is.null(infile3))
    return()
  input_table3=readr::read_delim(infile3$datapath,delim =input$sep3,escape_double = FALSE, trim_ws = TRUE)
  return(input_table3)
})
output$table2_4 <- DT::renderDataTable({
  data=datacontent4()},options = list(pageLength = 5))
########?????######################

datacontent5 <- reactive({
  old <- datacontent4()
  upload_edge <- datacontent3()
  # old_1 <- c(old)
  if (is.null(upload_edge) || is.null(old)) {
    return(data.frame())  
  }
  
  edges_select=dplyr::filter(upload_edge,tolower(gsub(" ", "",upload_edge$from)) %in% tolower(gsub(" ", "",old$name))|tolower(gsub(" ", "",upload_edge$to)) %in% tolower(gsub(" ", "",old$name)))
  return(edges_select)})

output$table2_5 <- DT::renderDataTable({
  data=datacontent5()},options = list(pageLength = 5))

output$download2_5 <-downloadHandler(
  filename = function() {
    paste("edges",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datacontent5(),file,row.names = FALSE)}
)

datacontent6 <- reactive({
  old <- datacontent5()
  upload_node <- datacontent2()
  if (is.null(upload_node) || is.null(old)) {
    return(data.frame())}  # 返回一个空的数据框架
    
  nodes_select=dplyr::filter(upload_node,tolower(gsub(" ", "",upload_node$id)) %in% tolower(gsub(" ", "",old$from)) | tolower(gsub(" ", "",upload_node$id)) %in% tolower(gsub(" ", "",old$to)))
  return(nodes_select)})

output$table2_6 <- DT::renderDataTable({
  data=datacontent6()},options = list(pageLength = 5))

output$download2_6 <-downloadHandler(
  filename = function() {
    paste("nodes",Sys.Date(),".csv",sep ="")},
  content = function(file) {
    write.csv(datacontent6(),file,row.names = FALSE)}
)

# # ###############################
network_data <- reactiveValues(nodes = NULL, edges = NULL)

observe({
  network_data$nodes <- nodes_inva # 替换为默认节点数据
  network_data$edges <- links_inva # 替换为默认边数据
})

observeEvent(input$action2_1, {
  if(input$tab2_1_2 == "Sample data"){
    network_data$nodes <- nodes_inva
    network_data$edges <- links_inva
  } else if(input$tab2_1_2 == "Upload nodes data") {
    network_data$edges <- datacontent5() 
    network_data$nodes <- datacontent6()
  }
})


output$networkid1 <- renderVisNetwork({
  
  color <- c("#B62024","#2E6E9E","#D2B020")
  shape <- c("dot","dot","dot")
  
  nodes_new <- network_data$nodes
  edges_new <- network_data$edges
  
  net <- graph_from_data_frame(d=edges_new,vertices = nodes_new,directed = T)
  nodesize <- igraph::degree(net,mode = "total")
  nodes_new$size_orign <- nodesize
  
  output$table2_3 <- DT::renderDataTable({
    data=nodes_new},options = list(pageLength = 5))
  
  output$download2_1 <-downloadHandler(
    filename = function() {
      paste("multi-omics",Sys.Date(),".csv",sep ="")},
    content = function(file) {
      write.csv(nodes_new,file)}#,row.names = FALSE)}
  )
  
  new_min_size <-  input$range[1]
  new_max_size <- input$range[2]
  scale_factor <- (new_max_size - new_min_size) / (max(nodes_new$size_orign) - min(nodes_new$size_orign))
  nodes_new$size <- (nodes_new$size_orign - min(nodes_new$size_orign)) * scale_factor + new_min_size
#####################
  

  
  visNetwork(nodes=nodes_new,edges=edges_new,main = "MultiOmics Network",width ="100%",height ="700px",background = "white") %>%
    visEdges(color ="#DDDDDD")%>%
    visNodes(shape ="dot", 
             size =nodes_new$size ,
             title = nodes_new$id,
             color = list(highlight="black"))%>% #highlight貌似颜色不管用
    visGroups(groupname = "gene",color=color[1]) %>%
    visGroups(groupname = "Metabolite", color =color[2])%>%
    visGroups(groupname = "enzyme", color =color[3])%>%
    visLegend(useGroups = TRUE,width = 0.15,position = "right",main="Group")%>%
              # title.font.size = 14,  # 设置标题字体大小
              # title.font.color = "blue")%>%
    visOptions(width = "100%", height = "100%",
               autoResize = FALSE,
               highlightNearest = list(hover = TRUE,degree=1,enabled = FALSE),
               nodesIdSelection = list(enabled = TRUE,useLabels=TRUE,
                                       style = 'width: 200px; height: 26px;background: #f8f8f8;color: #2E6E9E;border:none;outline:none;'),#useLables，有的没标签，这个需要和后边的lable对照
               selectedBy = list(variable="group",highlight=TRUE,multiple=TRUE,highlight=TRUE,
                                 style = 'width: 200px; height: 26px;background: #f8f8f8;color: #2E6E9E;border:none;outline:none;'),
               collapse = list(enabled=TRUE,fit=TRUE),
               manipulation =TRUE)%>%
    visIgraphLayout(layout =input$tab2_1_1)%>%# layout_on_sphere,"layout_on_grid","layout_in_circle","layout_with_fr","layout_nicely","layout_with_sugiyama",layout_with_dh)
    visInteraction(dragNodes = TRUE,
                   dragView = TRUE, 
                   zoomView = TRUE)%>%
    visPhysics(solver="forceAtlas2Based",
               barnesHut = list(avoidOverlap=1,gravitationalConstant=-10000,centralGravity=0.1),
               hierarchicalRepulsion=list(avoidOverlap=1),
               stabilization = FALSE,
               forceAtlas2Based = list(gravitationalConstant = -5))%>%
    visEdges(smooth = FALSE)%>%
    visLayout(randomSeed=301)%>%
    visExport(type = "jpeg", name = "export-network", float = "right",label = "Save network", background = "white", style= "")%>%
    visIgraphLayout()
  
  
  
})

