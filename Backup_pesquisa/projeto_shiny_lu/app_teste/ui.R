library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Nome do APP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tissue 1", tabName = "tissue1", icon = icon("line-chart")),
      menuItem("Tissue 2", tabName = "tissue2", icon = icon("list-ol")),
      menuItem("Tissue 3", tabName = "tissue3", icon = icon("th")),
      menuItem("Tissue 4", tabName = "tissue4", icon = icon("list-alt")))
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "tissue1",
              h2("Tissue 1"),
              fluidRow(
                box(title = "Inputs", height = 330, solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    
                  textInput(inputId = "ts1", label = "Enter tissue name",
                            value = "", placeholder = "Liver")
                  ,
                  textInput(inputId = "x1", label = "Enter X values here",
                            value = "", placeholder = "1,2,3,4,...")
                  ,
                  textInput(inputId = "y1", label = "Enter Y values here",
                            value = "", placeholder = "0.3,2,25,134.7,...")
                  ,
                  actionButton(inputId = "submit1", label = "Submit")
                
                ),
                
                box(title = "Table", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    
                    uiOutput('table1'))
              )
              ),
      
      tabItem(tabName = "tissue2",
              h2("Tissue 2"),
              fluidRow(
                box(title = "Inputs", height = 330, solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    
                    textInput("ts2","Enter tissue name", "", placeholder = "Kidnee")
                    ,
                    textInput("x2", "Enter X values here","", placeholder = "1,2,3,4,...")
                    ,
                    textInput("y2", "Enter Y values here","", placeholder = "0.3,2,25,134.7,...")
                    ,
                    actionButton("submit2","Submit")
                    
                ),
                
                box(title = "Table", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    
                    uiOutput('table2'))
              )
              ),
      
      tabItem(tabName = "tissue3",
              h2("Tissue 3"),
              fluidRow(
                box(title = "Inputs", height = 330, solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    
                    textInput("ts3", "Enter tissue name","", placeholder = "Heart")
                    ,
                    textInput("x3", "Enter X values here","", placeholder = "1,2,3,4,...")
                    ,
                    textInput("y3", "Enter Y values here","", placeholder = "0.3,2,25,134.7,...")
                    ,
                    actionButton("submit3","Submit")
                    
                ),
                
                box(title = "Table", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    
                    uiOutput('table3'))
              )
              ),
      
      tabItem(tabName = "tissue4",
              h2("Tissue 4"),
              fluidRow(
                box(title = "Inputs", height = 330, solidHeader = TRUE,
                    collapsible = TRUE, status = "primary",
                    
                    textInput("ts4", "Enter tissue name","", placeholder = "Stomach")
                    ,
                    textInput("x4", "Enter X values here","", placeholder = "1,2,3,4,...")
                    ,
                    textInput("y4", "Enter Y values here","", placeholder = "0.3,2,25,134.7,...")
                    ,
                    actionButton("submit4","Submit")
                    
                ),
                
                box(title = "Table", solidHeader = TRUE,
                    collapsible = TRUE, status = "warning",
                    
                    uiOutput('table4'))
              )
              )
      )
  )
)
