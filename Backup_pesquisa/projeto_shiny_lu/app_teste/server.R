library(shinydashboard)
library(readxl)
library(tidyverse)
library(DT)
library(plotly)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

### Table 1  
  Data1 = reactive({
    if (input$submit1 != 0) {
      df <- isolate(data.frame(x = as.numeric(unlist(strsplit(input$x1, ',')))
                       , y = as.numeric(unlist(strsplit(input$y1, ',')))))
      return(list(df=df))
    }
  })
  
  output$table1 <- renderTable({
    if (is.null(Data1())) {return()}
    print(Data1()$df)
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
})
