#### Join table

library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(rlang)

# Function ----------------------------------------------------------------


table_join <- function(fun, df.lt, df.rt, key.lt = NULL , key.rt = NULL,  ...){
  
  fun  <- switch (fun,
                  "semi_join" = semi_join,
                  "anti_join" = anti_join,
                  "inner_join" = inner_join,
                  "left_join" = left_join,
                  "right_join" = right_join,
                  "full_join" =full_join,
                  stop("This function is not supported", call. = F)
  )
  
  names(key.rt) <- key.lt
  
  call2(fun, df.lt, df.rt, key.rt) %>% eval()
  
}


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Join 2 tables"),
  tags$a(href="https://dplyr.tidyverse.org/reference/join.html#grouping", "Code from dplyr"),
  hr(),
  
  helpText("Step 1 : Upload 2 files ( accept .csv or excel )"),
  fluidRow(
    column(6, 
           fileInput("file_x", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload file X",
                     placeholder = "choose file .csv or .xlsx",multiple = F)
           ),
    column(6,
           fileInput("file_y", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload file Y",
                     placeholder = "choose file .csv or .xlsx",multiple = F)
           )
  ),
  
  hr(),
  helpText("Step 2 : Choose each column from file X and file Y as a key column to join 2 tables together"),
  fluidRow(
    column(6, 
           selectInput("cols_x", "Column from table X to match table Y", choices = NULL)
    ),
    column(6,
           selectInput("cols_y", "Column from table Y to match table X", choices = NULL)
    )
  ),
  
  hr(),
  helpText("Step 3 : Choose joining method...  ",
           tags$a(href="https://dplyr.tidyverse.org/reference/join.html#grouping", "See Code")),
      selectInput("join_fun", "Joining method:", choices = list("semi_join" = "semi_join",
                                                                "anti_join" = "anti_join",
                                                                "inner_join"= "inner_join",
                                                                "left_join" = "left_join", 
                                                                "right_join"= "right_join",
                                                                "full_join" = "full_join")),
      
      
      

    
      tabsetPanel(
        tabPanel("Joined",
                 dataTableOutput("table_joined"),
                 verbatimTextOutput("raw"),
                 verbatimTextOutput("raw_2"),
                 verbatimTextOutput("raw_3"),
                 verbatimTextOutput("raw_4")
                 ),
        tabPanel("Table X",
                 dataTableOutput("table_x")
                 ),
        tabPanel("Table Y",
                 dataTableOutput("table_y")
                 ),
        tabPanel("Guide : Joining method",
                 img(src='joining_method.png', align = "left", height = 900 ,width = 1199)
          
        )
        
        
      )
      
    
  
  
  

)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  

# Upload file X -----------------------------------------------------------

  df_x <- reactive({
    
    req(input$file_x) # Require - code wait until file uploaded
    
    ext <- tools::file_ext(input$file_x$name)
    df <- switch(ext,
             csv = readr::read_csv(input$file_x$datapath),
             xls = readxl::read_excel(input$file_x$datapath),
             xlsx = readxl::read_excel(input$file_x$datapath),
             validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
             )
    df %>% map_df(as.character)
  
  })
  
  file_name_x <- reactive({
    
    req(input$file_x) # Require - code wait until file uploaded
    str_remove(input$file_x$name,"\\.[^\\.]+$") # remove .xxx (content after last dot)
  
  })
  
  output$table_x <- renderDataTable({ df_x() })
  

# Upload file Y -----------------------------------------------------------

  df_y <- reactive({
    
    req(input$file_y) # Require - code wait until file uploaded
    
    ext <- tools::file_ext(input$file_y$name)
    df <- switch(ext,
                 csv = readr::read_csv(input$file_y$datapath),
                 xls = readxl::read_excel(input$file_y$datapath),
                 xlsx = readxl::read_excel(input$file_y$datapath),
                 validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
    )
    df %>% map_df(as.character)
  })
  
  file_name_y <- reactive({
    
    req(input$file_y) # Require - code wait until file uploaded
    str_remove(input$file_y$name,"\\.[^\\.]+$") # remove .xxx (content after last dot)
    
  })
  
  output$table_y <- renderDataTable({ df_y() })
  

# Swab --------------------------------------------------------------------
  

# Join table --------------------------------------------------------------

  
  joined_df <- reactive({
    
    req(input$cols_x, input$cols_y)
    table_join(input$join_fun, df.lt = df_x(), df.rt = df_y(), 
                 key.lt = input$cols_x, key.rt = input$cols_y)
    
  })
  
  
  observeEvent(input$file_x,{
    
    updateSelectInput(session,"cols_x", choices = names(df_x()))
    
  })
  
  observeEvent(input$file_y,{
    
    updateSelectInput(session,"cols_y", choices = names(df_y()))
    
  })
  
  output$table_joined <- renderDataTable({
    
    joined_df()
    
  })
  
  output$raw <- renderPrint({ list( df_x(), df_y() ) })
  output$raw_2 <- renderPrint({ c(file_name_x(), file_name_y(), input$join_fun )})
  output$raw_3 <- renderPrint({ c(input$cols_x, input$cols_y  ) })
  
  
}

shinyApp(ui, server)