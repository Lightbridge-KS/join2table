library(shiny)


ui <- fluidPage(
  checkboxInput("swab","Swab"),
  actionButton("swab_a","Swab"),
  verbatimTextOutput("raw"),
  verbatimTextOutput("raw_1"),
  verbatimTextOutput("raw_2")
)

server <- function(input, output, session) {
  
  df_1 <- reactive({ iris })
  nm_1 <- reactive({ "iris" })
  
  df_2 <- reactive({ mtcars })
  nm_2 <- reactive({ "mtcars" })
  

  ls <-  reactive({
    
      l <- list(df_1(), df_2())
      l <- setNames(l, c(nm_1(), nm_2()))
    
      if( input$swab == F){
        l
      }else{
       rev(l)
      }
      
    })
  
  ls_a <- reactive({
    
    l <- list(df_1(), df_2())
    l <- setNames(l, c(nm_1(), nm_2()))
    
    if( (input$swab_a %% 2) == 0  ){ # Even number (start at 0)
      l
    }else{
      rev(l)
    }
    
  })

    

  
  output$raw <- renderPrint({ str(list(ls(), ls_a()) )})
  output$raw_1 <- renderPrint({ list(input$swab ,input$swab_a) })
  output$raw_2 <- renderPrint({ list(df_1(), df_2())})
  
}

shinyApp(ui, server)