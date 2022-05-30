#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# library(opensilexWSClientR)
# library(phisWSClientR)
library(opensilexClientToolsR)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(rhandsontable)

source(file = "~/code/ANSWER-code/NEW/helper/r6_to_dataframe.R")

connectToOpenSILEX(identifier = "admin@opensilex.org", password = "admin", 
                   url = "http://138.102.159.36:8081/rest")

#obj = data.frame(getScientificObjects()$data[1:6])
#exp = data.frame(getExperiments2()$data[c(1:3,5,7)])
#df0 = left_join(obj, exp, by = c("experiment" = "uri"))

experiments_api <- ExperimentsApi$new()
experiments <- experiments_api$search_experiments()$data
experiments_df <- EnvironmentList_to_dataframe(experiments)
experimentList <- setNames(experiments_df$uri, experiments_df$name)
## Add filter to remove experiments witouht any scientific object
index_filter <- rep(FALSE, length(experimentList))
index <- 1
scientific_objects_api <- ScientificObjectsApi$new()
for (experiment_uri in experiments_df$uri) {
  print(experiment_uri)
  tryCatch(
    {
      scientific_objects_counts <-scientific_objects_api$search_scientific_objects(experiment = experiment_uri, page_size = 1)$metadata$pagination$totalCount
      print(scientific_objects_counts)
      if (scientific_objects_counts > 0) {
        index_filter[index] <- TRUE
      }
    },
    error=function(cond) {
      message(paste(experiment_uri, " doesn't seem to have any scientific objects"))
      # Choose a return value in case of error
      return(NA)
    })
  index = index + 1
}

dashboardPage(
  dashboardHeader(title = "Outlier App"),
  dashboardSidebar(
    selectInput(inputId = "experiment",
                label = "Choose an experiment:",
                choices = experimentList[index_filter],
                selected = experimentList[index_filter][1]
                # choices = setNames(unique(getExperiments2()$data$uri), unique(getExperiments2()$data$alias)),
                # selected = setNames(unique(getExperiments2()$data$uri), unique(getExperiments2()$data$alias))[5]
    ),
    uiOutput('choix_so'), uiOutput('choix_solabel'), uiOutput('choix_variable'), #actionButton(inputId = "go", label = "Go"),
    #verbatimTextOutput('x4')%>% withSpinner(color="#0dc5c1"),
    verbatimTextOutput('x6'),
    textOutput("console")

  ),
  dashboardBody(
    plotlyOutput("distPlot") %>% withSpinner(color="#0dc5c1"),
    dataTableOutput("distTable") %>% withSpinner(color="#0dc5c1"),
    tags$script("$(document).on('click', '#distTable button', function () {
                  Shiny.onInputChange('lastClickId', this.id);
                  Shiny.onInputChange('lastClick', Math.random())
});")
  )
)

