#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
shinyServer(
  function(input,output,session){
    
    Data = reactive({
      if (input$submit > 0) {
        df <- data.frame(x=input$x,y=input$y)
        return(list(df=df))
      }
    })
    
    output$table <- renderTable({
      if (is.null(Data())) {return()}
      print(Data()$df)
    }, 'include.rownames' = FALSE
    , 'include.colnames' = TRUE
    , 'sanitize.text.function' = function(x){x}
    )
    
  })


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