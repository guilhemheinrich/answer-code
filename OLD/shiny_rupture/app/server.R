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
#library(glue)
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
#library(opensilexWSClientR)
library(changepoint)
library(TTR)
library(future)
library(R.utils)
library(devtools)

connectToOpenSILEXWS(username = "admin@opensilex.org", password = "admin", 
                     url = "http://www.opensilex.org:8080/weisAPI/rest/")

obj = data.frame(getScientificObjects()$data[1:6])
exp = data.frame(getExperiments2()$data[c(1:3,5,7)])
df0 = left_join(obj, exp, by = c("experiment" = "uri"))

shinyServer(function(input, output) {
  
  output$choix_so<-renderUI({
    selectInput(inputId = "study_object",
                label = "Choose a type of study object:",
                choices = setNames(unique(df0[df0$experiment == unname(input$experiment), "rdfType"]), sub(".*#", "", unique(df0[df0$experiment == unname(input$experiment), "rdfType"]))),
                selected = setNames(unique(df0[df0$experiment == unname(input$experiment), "rdfType"]), sub(".*#", "", unique(df0[df0$experiment == unname(input$experiment), "rdfType"])))[2]
    )
  })
  
  output$choix_solabel<-renderUI({
    selectizeInput("study_object_label",
                 "Choose a study object:",
                 choices = unique(df0[df0$experiment == unname(input$experiment) & df0$rdfType == unname(input$study_object), "label"]),
                 selected = unique(df0[df0$experiment == unname(input$experiment) & df0$rdfType == unname(input$study_object), "label"])[7]
                )
  })
  
  output$choix_variable<-renderUI({
    selectizeInput("variable",
                  "Choose a variable:",
                  choices = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label),
                  selected = setNames(getVariablesByExperiment(unname(input$experiment))$data$uri, getVariablesByExperiment(unname(input$experiment))$data$label)[2]
                  )
  })
  
  df_sel <- reactive({
    data = data.frame(getData(paste(input$variable), pageSize = getData(variableUri = paste(input$variable))$totalCount)$data)
    df1 = left_join(data, obj, by = c("object.uri" = "uri"))
    df2 = left_join(df1, exp, by = c("experiment" = "uri"))
    df2$Year = substr(df2$date, 1, 4)
    df2$Month = substr(df2$date, 6, 7)
    df2$date = str_replace(df2$date, pattern = "T", replacement = " ")
    df2$date = as.POSIXct(df2$date)
    as.data.frame(df2[df2$object.labels == unname(input$study_object_label), ])
  })
  
  tab1 <- reactive({
    y_obs = as.numeric(as.character(na.omit(df_sel()[,"value"])))
    pelt.meancpt = cpt.var(y_obs, method = "PELT", penalty = "Manual", pen.value = 15*(log(length(y_obs))**2), class = TRUE)
    meancpt.point = cpts(pelt.meancpt)
    coeff = coef(pelt.meancpt)
    meancpt.point = append(1, meancpt.point,after = 1)
    int = lapply(1:length(meancpt.point), function(i) { list(meancpt.point[i],  ifelse(!is.na(meancpt.point[i+1]), meancpt.point[i+1], length(y_obs)))
        })
    fin = sort(sapply(1:length(meancpt.point), function(i) { unlist(int[[i]][1]) }), decreasing = TRUE)
    deb = sort(sapply(1:length(meancpt.point), function(i) { unlist(int[[i]][2]) }), decreasing = TRUE)
    moy = sapply(1:length(meancpt.point), function(i) { mean(na.omit(df_sel()[deb[i]:fin[i],"value"]))   })
    tab = tibble(coeff$variance, deb, fin, moy)
    tab = rename(tab, "var" = "coeff$variance")
    as.data.frame(tab)
  })
  
  output$distPlot <- renderPlotly({
    pelt.meancpt = cpt.var(as.numeric(as.character(na.omit(df_sel()[,"value"]))), method = "PELT", penalty= "Manual", pen.value=15*(log(length(df_sel()[,"value"]))**2), class = TRUE)
    meancpt.point = cpts(pelt.meancpt)
    
    print(head(df_sel()))
    a <- seq(0, 1, 0.1)
    sse <- numeric(length(a))
    
    for(i in 1:11){
      assign(paste0("fit", i), ses(as.numeric(as.character(df_sel()[,"value"])), alpha=a[i], initial="simple", h=1))
      sse[i] <- sum(get(paste0("fit", i))$residuals^2)
    }
    
    #construction of red rectangle
    line <- list(
      type = "rect",
      line = list(color = "red")
    )
    slist <- list()
    for (i in (which(tab1()[,"var"] >= var(na.omit(df_sel()[,"value"]))))) { 
      debi = tab1()[,"deb"][i]
      fini = tab1()[,"fin"][i]
      line[["opacity"]] <- 0.2
      line[["fillcolor"]] <- "red"
      line[["x0"]] <- sort(df_sel()[debi,"date"])
      line[["x1"]] <- sort(df_sel()[fini,"date"])
      line[["y0"]] <- min(na.omit(df_sel()[,"value"]))
      line[["y1"]] <- max(na.omit(df_sel()[,"value"]))
      slist <- c(slist, list(line))
    }
    
    #construction of plot
    if(length(meancpt.point)){
      fig <- plot_ly(x = df_sel()[,"date"], y = as.numeric(as.character(df_sel()[,"value"])), type = 'scatter', name = "Raw data", 
                     visible = ifelse(length(df_sel())>=1000, TRUE , "legendonly")) %>%
        layout(xaxis = list(rangeselector = list(buttons = list(
          list(count = 1, label = "1 day", step = "day", stepmode = "backward"),
          list(count = 1, label = "1 week", step = "week", stepmode = "backward"),
          list(count = 1, label = "1 month", step = "month", stepmode = "backward"))), 
          rangeslider = list(type = "date")))
      fig <- fig %>% add_segments(x = df_sel()[c(meancpt.point),"date"], xend = df_sel()[c(meancpt.point),"date"], y = min(as.numeric(as.character(na.omit(df_sel()[,"value"])))), 
                                  yend = max(as.numeric(as.character(na.omit(df_sel()[,"value"])))), name = "Changing point", visible = TRUE, line = list(color = 'rgb(205, 12, 24)')) %>%
        add_trace(y = ~fitted(fit2), mode='lines', line = list(color = 'rgb(120, 207, 67)'), name = "Smoothing data", visible = TRUE)%>% 
        layout(title=paste("Distribution of", df_sel()[,"variable.label"][1]), scene = list(yaxis = df_sel()[,"variable.label"][1])
               , shapes = slist
               ) 
    }else{
      fig <- plot_ly(x = (df_sel()[,"date"]), y = as.numeric(as.character(df_sel()[,"value"])),type = 'scatter', name = "Raw data") %>%
        add_trace(y =~fitted(fit2), mode='lines', line = list(color = 'rgb(120, 207, 67)'), name = "Smoothing data", visible = TRUE) %>%
        layout(xaxis = list(rangeselector = list(buttons = list(
          list(count = 1, label = "1 day", step = "day", stepmode = "backward"),
          list(count = 1, label = "1 week", step = "week", stepmode = "backward"),
          list(count = 1, label = "1 month", step = "month", stepmode = "backward"))), rangeslider = list(type = "date"))) %>%
        config(displayModeBar = FALSE, edits = list(shapePosition = TRUE))%>%
        layout(title = paste("Distribution of", df_sel()[,"variable.label"][1],"var",var(df_sel()[,"value"])), scene = list(yaxis = df_sel()[,"variable.label"][1])) 
    }
    fig
  })

  output$x6 = renderPrint({ print(
    sessionInfo()
    ) })
  
  output$box_var <- renderUI({
   fluidRow(
  lapply(1:nrow(tab1()), function(i) {
     deb = tab1()[, "deb"][i]
     fin = tab1()[, "fin"][i]
     text1 = paste("Variance",round(tab1()[, "var"][i], 3),"\n","Mean",round(tab1()[, "moy"][i], 3))
     # match(i, round(coef(pelt.meancpt)$variance,2))
     # valueBox(width = 2, res1, subtitle = cat(paste("Variance of segment ", i, "\nFrom", 
     #  sort(df_sel()[deb,"date"], decreasing = FALSE), "to", sort(df_sel()[fin, "date"], decreasing = FALSE), collapse = "\n")), 
     #  icon = icon("chart-line"), color = ifelse(tab1()[, "var"][i] > var(df_sel()[, "value"]), "red", "teal"),
     #  href = NULL)
    box( title = tagList(icon("calculator"),paste("Segment",i,"from", sort(df_sel()[deb,"date"], decreasing = FALSE), "to", sort(df_sel()[fin, "date"], decreasing = FALSE))),
         background = ifelse(tab1()[, "var"][i] > var(df_sel()[, "value"]), "red", "teal"),
             (text1) 
       
     )
     
     })
 ) 
  })
  
})
