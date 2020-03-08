#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(
  function(input,output,session){
    
    Data = reactive({
      if (input$submit > 0) {
        df <- data.frame(x = as.numeric(unlist(strsplit(input$x, ',')))
                         , y = as.numeric(unlist(strsplit(input$y, ','))))
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
