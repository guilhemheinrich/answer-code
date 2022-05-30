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
library(opensilexWSClientR)
library(phisWSClientR)
library(dplyr)
library(plotly)
library(shinycssloaders)
library(shinydashboard)

connectToOpenSILEXWS(username = "admin@opensilex.org", password = "admin", 
                     url = "http://www.opensilex.org:8080/weisAPI/rest/")

obj = data.frame(getScientificObjects()$data[1:6])
exp = data.frame(getExperiments2()$data[c(1:3,5,7)])
df0 = left_join(obj, exp, by = c("experiment" = "uri"))

dashboardPage(
  dashboardHeader(title = "Changing Point App"),
  dashboardSidebar(
    selectInput(inputId = "experiment",
                label = "Choose an experiment:",
                choices = setNames(unique(getExperiments2()$data$uri), unique(getExperiments2()$data$alias)),
                selected = setNames(unique(getExperiments2()$data$uri), unique(getExperiments2()$data$alias))[5]
    ),
    uiOutput('choix_so'), uiOutput('choix_solabel'), uiOutput('choix_variable'),
    verbatimTextOutput('x6')
  ),
  dashboardBody(
    plotlyOutput("distPlot") %>% withSpinner(color="#0dc5c1"),
    (uiOutput('box_var'))
  )
)

