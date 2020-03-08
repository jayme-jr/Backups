#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("textInput Demo")
    ,
    sidebarPanel(
      wellPanel(
        textInput('x', "enter X value here","")
        ,
        textInput('y', "enter Y value here","")
        ,
        actionButton("submit","Submit")
      )
    )
    ,
    mainPanel(uiOutput('table'))
  ))