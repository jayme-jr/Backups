# UI function

dfUI <- function(id){
  ns <- NS(id)
  sidebarPanel(
    wellPanel(
      textInput(ns('x'), "enter X value here","")
      ,
      textInput(ns('y'), "enter Y value here","")
      ,
      actionButton(ns("submit"),"Submit")
    )
  )
  mainPanel(uiOutput('table'))
}


df <- function(input,output,session){
  
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
  
}



