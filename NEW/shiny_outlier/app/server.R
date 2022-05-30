#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(glue)
library(grDevices)
library(EnvStats)
library(knitr)
library(dplyr)
# library(kableExtra)
library(formattable)
library(opensilexClientToolsR)
library(plotly)
library(stringr)
library(forecast)
library(future)
library(ipc)
library(parallel)
library(promises)
library(rhandsontable)


source(file = "~/code/ANSWER-code/NEW/helper/r6_to_dataframe.R")

future::plan(future::multiprocess)

#    _____       _ _   _       _ _           _   _             
#   |_   _|     (_) | (_)     | (_)         | | (_)            
#     | |  _ __  _| |_ _  __ _| |_ ___  __ _| |_ _  ___  _ __  
#     | | | '_ \| | __| |/ _` | | / __|/ _` | __| |/ _ \| '_ \ 
#    _| |_| | | | | |_| | (_| | | \__ \ (_| | |_| | (_) | | | |
#   |_____|_| |_|_|\__|_|\__,_|_|_|___/\__,_|\__|_|\___/|_| |_|
#                                                              
#                                                              

connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", 
                     url = "http://138.102.159.36:8081/rest")


# obj = data.frame(getScientificObjects()$data[1:6])


# exp = data.frame(getExperiments2()$data[c(1:3,5,7)])
# experiments_api <- ExperimentsApi$new()
# experiments <- experiments_api$search_experiments()$data[c(1:3,5,7)]
# experiments_df <- EnvironmentList_to_dataframe(experiments)


#df0 = left_join(obj, exp, by = c("experiment" = "uri"))
ontology_api <- OntologyApi$new()
scientific_objects_type <- ontology_api$get_sub_classes_of(parent_type = "http://www.opensilex.org/vocabulary/oeso#ScientificObject")$data[[1]]
so_type_uris <- scientific_objects_type$children[[1]]$uri
so_type_name <- scientific_objects_type$children[[1]]$name
specific <- startsWith(so_type_uris, 'http://www.opensilex.org/vocabulary/oeso-weis#' )
init_so_rdf_type <- so_type_uris[specific]
init_so_name <- so_type_name[specific]

searchAvailableType <- function(experiment_uri) {
  connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", url = "http://138.102.159.36:8081/rest")
  scientific_objects_api <- ScientificObjectsApi$new()
  scientific_objects_dto <-scientific_objects_api$search_scientific_objects(experiment = experiment_uri, page_size = 100000)$data
  scientific_objects_df <-EnvironmentList_to_dataframe(scientific_objects_dto)
  rdf_types_names <- setNames(unique(scientific_objects_df$rdf_type), sub(".*#", "", unique(scientific_objects_df$rdf_type)))
  return(rdf_types_names)
}

searchAvailableSO <- function(experiment_uri, scientificObject_rdfType) {
  connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", url = "http://138.102.159.36:8081/rest")
  scientific_objects_api <- ScientificObjectsApi$new()
  scientific_objects_dto <-scientific_objects_api$search_scientific_objects(experiment = experiment_uri, page_size = 20, rdf_type = scientificObject_rdfType)$data
  scientific_objects_df <-EnvironmentList_to_dataframe(scientific_objects_dto)
  SO_choices <- setNames(scientific_objects_df$uri,scientific_objects_df$name)
  return(SO_choices)
} 

data_df <- data.frame()
### Debug env
{
  experiments_uri <- 'http://www.opensilex.org/weis/WS1996-2'
  scientificObject_uri <- 'http://www.opensilex.org/weis/2019/o19000002' 
}
searchAvailableVariable <- function(experiment_uri, scientificObject_uri) {
  connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", url = "http://138.102.159.36:8081/rest")
  data_api <- DataApi$new()
  data_dto <- data_api$search_data_list(experiment = experiment_uri, scientific_objects = scientificObject_uri)$data
  ## Use <<- to set the variable in the outter scope / GlobEnv
  data_df <<-EnvironmentList_to_dataframe(data_dto)
  variable_choices <- setNames(data_df$uri,data_df$name)
}


shinyServer(function(input, output, session) {
  
  output$choix_so<-renderUI({
    selectInput(inputId = "study_object",
                label = "Choose a type of study object:",
                choices = setNames(init_so_rdf_type, init_so_name)#,
                # choices = setNames(unique(df0[df0$experiment == unname(input$experiment), "rdfType"]), sub(".*#", "", unique(df0[df0$experiment == unname(input$experiment), "rdfType"])))#,
                #selected = setNames(unique(df0[df0$experiment == unname(input$experiment), "rdfType"]), sub(".*#", "", unique(df0[df0$experiment == unname(input$experiment), "rdfType"])))[2]
                )
  })
  
  output$choix_solabel<-renderUI({
    selectizeInput("study_object_label",
                 "Choose a study object:",
                  choices = c("Label A" = "label_a", "Label B" = "label_b")                 
                  # choices = unique(df0[df0$experiment == unname(input$experiment) & df0$rdfType == unname(input$study_object), "label"])#,
                 #selected = unique(df0[df0$experiment == unname(input$experiment) & df0$rdfType == unname(input$study_object), "label"])[7]
                )
  })
  output$choix_variable<-renderUI({
    selectizeInput("variable",
                  "Choose a variable:",
                  choices = c("Variable A" = "variable_a", "Variable B" = "variable_b")  
                  # choices = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label)#,
                  #selected = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label)[2]
                  )
  })

  #variable = isolate(input$variable)
  #sol = isolate(input$study_object_label)
  observeEvent(input$experiment, {
    rdf_types_and_names <- searchAvailableType(input$experiment)
    updateSelectInput(session, "study_object", choices = rdf_types_and_names)
    output$console <- renderPrint({
      print(paste0("experiment: ", input$experiment))
    })
  })

  observeEvent(input$study_object, {
    SO_choices <- searchAvailableSO(input$experiment, input$study_object)
    updateSelectInput(session, "study_object_label", choices = SO_choices)
    output$console <- renderPrint({
      print(paste0("so_types: ", input$study_object))
    })
  })
  
  observeEvent(input$study_object_label, {
    output$console <- renderPrint({
      print(paste0("You have chosen: ", input$variable))
    })
  })


  df_sel <- eventReactive(list(input$variable,input$study_object_label),{
    # output$x6 <- renderPrint({
    #   print(paste0("You have chosen: ", input$variable))
    #   print(paste0("You have chosen: ", input$study_object_label))
    # }) 
    #  connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", 
    #                     url = "http://138.102.159.36:8081/rest")
    # data = data.frame(getData(paste(input$variable), pageSize = getData(variableUri = paste(input$variable))$totalCount)$data)
    # df1 = left_join(data, obj, by = c("object.uri" = "uri")) 
    # df2 = left_join(df1, exp, by = c("experiment" = "uri")) 
    # df2$Year = substr(df2$date, 1, 4) 
    # df2$Month = substr(df2$date, 6, 7)
    # df2$date = str_replace(df2$date, pattern = "T", replacement = " ") 
    # df2$date = as.POSIXct(df2$date) 
    # df2 = as.data.frame(df2[df2$object.labels == unname(input$study_object_label),])
    # df2 = df2[!is.na(df2$value), ]
    # df2 
  })
   
  # smoothing <- function(df_value){
  #   smp_size <- floor(0.70 * length(df_value))
  #   df.train <-  window(ts(df_value), end = smp_size) 
  #   df.test <-  window(ts(df_value), start = smp_size+1) 
  #   alpha <- seq(.01, .99, by = .01)
  #   RMSE <- NA
  #   for(i in seq_along(alpha)) {
  #     fit <- ses(df.train, alpha = alpha[i], h = 100)
  #     RMSE[i] <- accuracy(fit, df.test)[2,2]
  #   }
  #   # convert to a data frame and idenitify min alpha value
  #   alpha.fit <- data_frame(alpha, RMSE)
  #   alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
  #   #alpha.mean <- filter(alpha.fit, RMSE == mean(RMSE))
  #   fit_ses_opt = ses(df_value, alpha = alpha.min$alpha)
  #   return(fit_ses_opt)
  # }
  
  # test_rosner <- function(df){
  # # outlier_var = as.numeric(as.character(df[,"value"]))[which(as.numeric(as.character(df[,"value"])) %in% 
  # #                                                              boxplot.stats(as.numeric(as.character(df[,"value"])))$out)]
  # # test = rosnerTest(as.numeric(as.character(df[,"value"])), k = ifelse(length(outlier_var)==0,1,length(outlier_var)), warn = F)
  # # a = test$all.stats$Obs.Num
  # # a = as.integer(a)
  # # b = test$all.stats$Outlier
  # # (list(a,b,test))
  #   diff = as.numeric(df[,"value"]) - as.numeric(fitted(smoothing(df[,"value"])))
  #   vr = abs(diff) > ifelse(nrow(df_sel()) >= 50000, 6*mean(df[,"value"]), 1*mean(df[,"value"]))
  #   # vr = case_when(
  #   #   height < 200 | mass > 200      ~ "large",
  #   #   height > 200 | mass > 200      ~ "large",
  #   #   height > 200 | mass > 200      ~ "large"
  #   # )
  #   return(vr)
  # }
  
  
  queue <- shinyQueue()
  queue$consumer$start(100) # Execute signals every 100 milliseconds
  
#   # A reactive value to hold output
#   result_val <- reactiveVal()
#   result_val2 <- reactiveVal()
#  # input$study_object_label, input$variable
#   observeEvent(input$go, {
#     df = isolate(df_sel())
    
#     future({
#       connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", 
#                          url = "http://138.102.159.36:8081/rest", reconnection = TRUE)
#       result = test_rosner(df)
#       queue$producer$fireAssignReactive("result_val", result)
#     })
    
#     future({
#       connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", 
#                          url = "http://138.102.159.36:8081/rest", reconnection = TRUE)
#       sm = smoothing(df[,"value"])
#       queue$producer$fireAssignReactive("result_val2", sm)
#     }) 
#   })  
  
#   ptendency1 <- function(station, object){
#     obj = "value"  #N.col de object 
#     s = input$distTable_rows_selected
    
#     # a = test_rosner(df_sel())[[1]]
#     # b = test_rosner(df_sel())[[2]]
#     # c = test_rosner(df_sel())[[3]]$all.stats$lambda.i
#     # print(a)
#     #diff = as.numeric(df_sel()[,"value"]) - as.numeric(fitted(smoothing(df_sel()[,"value"])))
#     #vr = abs(diff) > 3.5*mean(df_sel()[,"value"])
#     vr = test_rosner(df_sel())
#     #vr = as.vector(as.numeric(as.character(df_sel()[vr1,"value"])))
#     #print((vr1))
#     a <- seq(0, 1, 0.1)
#     for(i in 1:11){
#       assign(paste0("fit", i), ses(as.numeric(as.character(df_sel()[,"value"])), alpha=a[i], initial="simple", h=1))
#     }
    
#     if(length(vr)){
#       #fit_ses = smoothing(as.numeric(na.omit(df_sel()[,"value"])))
#       fig <- plot_ly(data = df_sel()[,],x = df_sel()[,"date"], y = as.numeric(as.character(df_sel()[,"value"])), type = 'scatter', 
#                     name = "Raw data", visible =ifelse(nrow(df_sel()) >= 9000, "legendonly", TRUE)) %>% 
#             add_trace(x = df_sel()[which(vr),"date"], y = as.numeric(as.character(df_sel()[which(vr),"value"])), name = 'Outlier', type = 'scatter', 
#                     marker = list(color = 'rgb(205, 12, 24)'), visible = TRUE) %>%
#             add_trace(y = ~fitted(fit2), mode='lines', line = list(color = 'rgb(120, 207, 67)'), name = "Smoothing data", visible = TRUE) %>%
#             add_trace(x = df_sel()[s,"date"], y = as.numeric(as.character(df_sel()[s,"value"])), mode='markers', marker = list(color = 'rgb(29, 214, 255)'), name = "Selected row", visible = TRUE)%>%
#             layout(title=paste("Distribution of", df_sel()[,"variable.label"][1]), scene = list(yaxis = df_sel()[,"variable.label"][1])) 
#     }else{
#       #fit_ses = smoothing(as.numeric(na.omit(df_sel()[,"value"])))
#       fig <- plot_ly(data = df_sel(), x = df_sel()[,"date"], y = as.numeric(as.character(df_sel()[,"value"])), type = 'scatter', name = "Raw data",
#                       visible = ifelse(length(df_sel()) >= 9000, "legendonly", TRUE)) %>%
#             add_trace(y = ~fitted(fit2), mode='lines', line = list(color = 'rgb(120, 207, 67)'), name = "Smoothing data", visible = TRUE) %>%
#             add_trace(x = df_sel()[s,"date"], y = ~as.numeric(as.character(df_sel()[s,"value"])), mode='markers', marker = list(color = 'rgb(29, 214, 255)'), name = "Selected row", visible = TRUE)%>%
#             layout(title=paste("Distribution of", df_sel()[,"variable.label"][1]), scene = list(yaxis = df_sel()[,"variable.label"][1]))
#       }
#     fig
#   }
    
#   # calling of the plot
#   output$distPlot <- renderPlotly({
#     ptendency1(input$study_object_label,input$variable)
#   }) 
  
  
#   #data table
#   output$distTable <- renderDataTable({
#     choice = "value"
#     #station = input$study_object_label

#     #test = test_rosner()()
#     # a = test_rosner(df_sel())[[1]]
#     # b = test_rosner(df_sel())[[2]]
#     # 
#     # vec = as.vector(as.numeric(as.character(df_sel()[a[b],choice])))
#     df_sel = data.frame(df_sel())
    
#     vr = test_rosner(df_sel())
#     vec = as.vector(as.numeric(as.character(df_sel()[vr,choice])))
    
#     checked <- paste0('
#       <button type="button" class="btn btn-secondary modify" id=modify_',1:nrow(df_sel),'>Annotate</button>
#       <br>')

#     if(length(vec)){
#       datatable(cbind(df_sel[,c("date","object.labels","value")], check = checked), options = list(pageLength = 10
#       ),escape = FALSE)%>%
#         formatStyle(columns =c(names(df_sel[,c("date","object.labels","value")])),
#                     target='row',
#                     backgroundColor = styleEqual(vec, rep('red',length(vec))))
#     }
#     else{datatable(cbind(df_sel[,c("date","object.labels","value")], check = checked), options = list(pageLength = 10),escape = FALSE)
#     }
#   })

#   # text on the side of the app
#   # output$x4 = renderPrint({
#   #   obj = "value"
#   #   s = input$distTable_rows_selected
#   # 
#   #   a = test_rosner(df_sel())[[1]]
#   #   b = test_rosner(df_sel())[[2]]
#   # 
#   #   lst = list(test_rosner(df_sel())[[3]]$all.stats$i, test_rosner(df_sel())[[3]]$all.stats$Obs.Num, test_rosner(df_sel())[[3]]$all.stats$R.i - test_rosner(df_sel())[[3]]$all.stats$lambda.i)
#   #   if (length(s)) {
#   #     cat('Row selected and the difference between \nstatistical value and critic value \n(Rosner test, outlier if superior to 0):\n \n')
#   #     for (j in (1:length(s))){
#   #       cat("Row ", s[j], ":", test_rosner(df_sel())[[3]]$all.stats$R.i[match(s[j], test_rosner(df_sel())[[3]]$all.stats$Obs)] - test_rosner(df_sel())[[3]]$all.stats$lambda.i[match(s[j],test_rosner(df_sel())[[3]]$all.stats$Obs)]
#   #           , "\n", sep = ' ')
#   #     }
#   #   }
#   # })

#   output$x5 = renderPrint({})

#   #creation of data for annotation
#   df_sel1 <- reactive({
#     df_s = df_sel()
#     choice = "value"
#     station = input$study_object_label
#     #df_s$conf = test_rosner()[[3]]$all.stats$R.i[match(1:nrow(df), test_rosner()[[3]]$all.stats$Obs)] - test_rosner()[[3]]$all.stats$lambda.i[match(1:nrow(df), test_rosner()[[3]]$all.stats$Obs)]
#     df_s
#   })

#   output$lignes <- renderDataTable({
#     as.data.frame(df_sel1()[as.numeric(gsub("modify_","",input$lastClickId)),c("Year","Month","object.labels","value","variable.label")])
#   #retrait de la colonne "conf"
#     })

#   modal_modify <- modalDialog(
#     fluidPage(
#       h3(strong("Row annotation"), align="center"),
#       hr(),
#       dataTableOutput('lignes'),
#       textInput("inputText", "Annotation", value = ""),
#       actionButton("send", "Submit")
#     ),
#     size="l"
#   )

#   observeEvent(input$send, {
#     # newA = AnnotationDTO$new(
#     #   motivatedBy = "http://www.w3.org/ns/oa#assessing",
#     #   creator = "http://www.opensilex.org/demo/id/agent/admin_phis",
#     #   list(paste(df_sel1()[as.numeric(gsub("modify_","",input$lastClickId)),"uri"])),
#     #   bodyValues =  list(paste0("confidence = " , df_sel1()[as.numeric(gsub("modify_","",input$lastClickId)),"conf"],
#     #                            "\nprocess = " , Sys.getpid(),
#     #                            "\ndescription = " , input$inputText))
#     # )
#     # postAnnotations2(list(newA))
#     removeModal()
#   })

#   observeEvent(input$lastClick,{showModal(modal_modify)})

#   output$x6 = renderPrint({#fitted(result_val2())[1]
#     # print(system.time(test_rosner()),
#            sessionInfo()
#     })
})
