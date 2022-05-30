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
library(kableExtra)
library(formattable)
library(phisWSClientR)
library(plotly)
library(stringr)
library(forecast)
library(tidyr)
library(scales)
library(opensilexWSClientR)
library(TTR)
library(RColorBrewer)
library(caret)
library(Smisc)

connectToOpenSILEXWS(username = "admin@opensilex.org", password = "admin", 
                     url = "http://www.opensilex.org:8080/weisAPI/rest/")

obj = data.frame(getScientificObjects()$data[1:6])
exp = data.frame(getExperiments2()$data[c(1:3,5,7)])
df0 = left_join(obj, exp, by = c("experiment" = "uri"))

shinyServer(function(input, output) {
  
  output$choix_so <- renderUI({
    selectInput(inputId = "study_object",
                label = "Choose a type of study object:",
                choices = sub(".*#", "", unique(getScientificObjects(experiment = "http://www.opensilex.org/weis/WS2018-2", rdfType = "http://www.opensilex.org/vocabulary/oeso-weis#Observation_point")$data$rdfType))#,
                #selected = setNames(unique(df0[df0$experiment == unname(input$experiment), "rdfType"]), sub(".*#", "", unique(df0[df0$experiment == unname(input$experiment), "rdfType"])))[2]
    )
  })
  
  output$choix_solabel <- renderUI({
    selectizeInput("study_object_label",
                 "Choose a study object:",
                 choices = getScientificObjects(experiment = input$experiment, rdfType = "http://www.opensilex.org/vocabulary/oeso-weis#Observation_point")$data$label #,
                 #selected = unique(df0[df0$experiment == unname(input$experiment) & df0$rdfType == unname(input$study_object), "label"])[7]
                )
  })
  
  output$choix_variable <- renderUI({
    selectizeInput("variable",
                  "Choose a variable:",
                  choices = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label)#,
                  #selected = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label)[2]
                  )
  })
  
  df_sel <- reactive({
    data = data.frame(getData(input$variable, pageSize = getData(variableUri = input$variable)$totalCount)$data)
    df1 = left_join(data, obj, by = c("object.uri" = "uri"))
    df2 = left_join(df1, exp, by = c("experiment" = "uri"))
    df2$Year = substr(df2$date, 1, 4)
    df2$Month = substr(df2$date, 6, 7)
    df2$date = str_replace(df2$date, pattern = "T", replacement = " ")
    df2$date = as.POSIXct(df2$date)
    df2 = as.data.frame(df2[df2$object.labels == (input$study_object_label), ])
    #print(head(df2))
    #grep("MODEL",df_sel()[,"provenance.label"] )
    as.data.frame(df2) 
  })
  
  output$distPlot <- renderPlotly({
    p = plot_ly(x = df_sel()[,"date"], y = as.numeric(as.character(df_sel()[,"value"])), type="scatter", mode="lines", 
            color=~df_sel()[,"provenance.label"], colors = brewer.pal(n = 5, name = 'Set1')[c(2,3,5)], linetype = ifelse(grepl('MODEL', df_sel()[,"provenance.label"]),'solid','dot'), 
            name = ~df_sel()[,"provenance.label"]) %>%
      layout(hovermode="x")%>%
      layout(title= paste("Modeling of", df_sel()[,"variable.label"][1]), scene = list(yaxis = df_sel()[,"variable.label"][1]))
    p
  })
  
  fonction_evaluation <- function(model_name){
    reel = df_sel()[df_sel()$provenance.label == "LEESU",]
    model = df_sel()[df_sel()$provenance.label == model_name,]
    val = (reel$value - model$value)^2 / (reel$value)^2
    return(val)
  }
  
  output$models <- renderUI({
    reel = df_sel()[df_sel()$provenance.label == "LEESU",]
    lapply(grep('MODEL', unique(df_sel()$provenance.label), value = TRUE), function(i) { 
      tags$div(id = i,
      valueBox(round(timeIntegration(fonction_evaluation(i), time = sort(reel$date), 
               lower = min(reel$date), upper = max(reel$date), 
               check.plot = TRUE, units = c("hours"))/length(reel$date), 4), 
               subtitle = paste("Evaluation of", i), icon = icon("chart-line"), 
               href = NULL)
      )
      })
    
  })
  
  output$style <- renderUI({
    lapply(grep('MODEL', unique(df_sel()$provenance.label)), function(i)
      {
      textStyle = paste0("#", unique(df_sel()$provenance.label)[i]," .small-box.bg-aqua { background-color: ", brewer.pal(n = 5, name = 'Set1')[c(2,3,5)][i], " !important; color: #FFFFFF !important; }")
      tags$style(textStyle)
        })
   
    })
  
  output$x6 = renderPrint({print(sessionInfo())})
  
})
