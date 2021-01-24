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
  
  exp <- call2(fun, df.lt, df.rt, key.rt, ...) 
  
  eval(exp)
  
}

remove_dup <- function(df, type = "distinct" ,vars ,all_vars = F,  ...) {
  
  fun <- switch (type,
                 "distinct" = distinct,
                 "count" = count
  )
  
  if(all_vars) return( fun(df, across(names(df))) )
  
  vars <- syms(vars)
  
  df %>% fun(!!!vars, ...)
}


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  
  titlePanel("Join 2 tables"),
  hr(),
  

  # File upload -------------------------------------------------------------

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
  
  checkboxInput("swab","Swab table ?"),
  
  fluidRow(
    column(6, 
           htmlOutput("nm_table_x")

    ),
    column(6,
           htmlOutput("nm_table_y")
    )
  ),
  

  # Choose cols -------------------------------------------------------------

  hr(),
  helpText("Step 2 : Choose each column from table X and table Y as a key column to join 2 tables together"),
  fluidRow(
    column(6, 
           selectInput("cols_x", "Column from table X to match table Y:", choices = NULL)
    ),
    column(6,
           selectInput("cols_y", "Column from table Y to match table X:", choices = NULL)
    )
  ),
  
  hr(),
  

# Join & Remove dup -------------------------------------------------------

  
  fluidRow(
    column(6, 
           helpText("Step 3 : Choose joining method...  ",
                    tags$a(href="https://dplyr.tidyverse.org/reference/join.html#grouping", "See Code")),
           selectInput("join_fun", "Joining method:", choices = list("semi_join" = "semi_join",
                                                                     "anti_join" = "anti_join",
                                                                     "inner_join"= "inner_join",
                                                                     "left_join" = "left_join", 
                                                                     "right_join"= "right_join",
                                                                     "full_join" = "full_join"))
    ),
    column(6,
           checkboxInput("check_rm_dup","Remove duplicate rows ?",value = F),
           tabsetPanel(
             id = "tab_rm_dup",
             type = "hidden",
             tabPanel("not_show"),
             tabPanel("show", 
                      selectInput("rm_dup", "Column to remove duplicate rows:", 
                                  selected = NULL, multiple = T, choices = NULL),
                      checkboxInput("rm_dup_type", "Add count to duplicate rows", value = F)
             )
           )
    )
  ),

  


  # Tabset ------------------------------------------------------------------

  
  tabsetPanel(
    tabPanel("Joined",
             br(),
             dataTableOutput("table_joined"),
             hr(),
             helpText("Step 4 : Download"),
             downloadButton("download", "Download Joined table .xlsx", class = "btn-block"),
             hr(),
             
             #verbatimTextOutput("raw"),
             #verbatimTextOutput("raw_2"),
             #verbatimTextOutput("raw_3"),
             #verbatimTextOutput("raw_4")
    ),
    tabPanel("Table X",
            h3(textOutput("x_lab")),
            br(),
             dataTableOutput("table_x")
    ),
    tabPanel("Table Y",
             h3(textOutput("y_lab")),
             br(),
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
    
    if( !isTruthy(input$file_x)){  return(band_members) } # Default x
      

    
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
    
    if( !isTruthy(input$file_x)){ return("band_members")} # Default names x
      
    req(input$file_x) # Require - code wait until file uploaded
    str_remove(input$file_x$name,"\\.[^\\.]+$") # remove .xxx (content after last dot)
    
  })

  

  
  
  # Upload file Y -----------------------------------------------------------
  
  df_y <- reactive({
    
    if( !isTruthy(input$file_x)){ return(band_instruments2) } # Default y
    
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
    
    if( !isTruthy(input$file_x)){ return("band_instruments2")} # Default names y
    
    req(input$file_y) # Require - code wait until file uploaded
    str_remove(input$file_y$name,"\\.[^\\.]+$") # remove .xxx (content after last dot)
    
  })

  
  
  # Swab --------------------------------------------------------------------
  
  
  ls <-  reactive({
    
    l <- list(df_x(), df_y())
    l <- setNames(l, c(file_name_x(), file_name_y()))
    
    if( input$swab == F){
      l
    }else{
      rev(l)
    }
    
  })
  
  output$nm_table_x <- renderText({ HTML("Table X is ", "<b>",names(ls())[[1]],"</b>" )})
  output$nm_table_y <- renderText({ HTML("Table Y is ",  "<b>",names(ls())[[2]],"</b>" )})

  # Display table X and Y ---------------------------------------------------
  
  output$x_lab <- renderText({ names(ls())[[1]]  })
  output$y_lab <- renderText({ names(ls())[[2]] })
  
  output$table_x <- renderDataTable({ 
    ls()[[1]] 
    },options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
  
  output$table_y <- renderDataTable({ 
    ls()[[2]] 
    },options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))

  
  # Join table --------------------------------------------------------------
  
  
  joined_df <- reactive({
    
    req(ls(), input$cols_x, input$cols_y)
    table_join(input$join_fun, df.lt = ls()[[1]], df.rt = ls()[[2]], 
               key.lt = input$cols_x, key.rt = input$cols_y, 
               suffix = c(paste0("_from: ", c(names(ls())[[1]], names(ls())[[2]]) )))
    
  })
  
  
  observeEvent(ls(),{
    
    updateSelectInput(session,"cols_x", choices = names(ls()[[1]]))
    
  })
  
  observeEvent(ls(),{
    
    updateSelectInput(session,"cols_y", choices = names(ls()[[2]]))
    
  })


# Remove duplicate rows ---------------------------------------------------

  check_string <- reactive({  
    
    if(input$check_rm_dup == T){"show"}else{"not_show"}
    
  })
  
  observeEvent(input$check_rm_dup,{
    updateTabsetPanel(session, "tab_rm_dup", selected = check_string())
    
  })
  
  
  observeEvent(input$check_rm_dup,{

    updateSelectInput(session, "rm_dup", choices = names(joined_df()))

  })
  
  # rm_ready <- reactive({ isTruthy(names(joined_df())) &&  input$check_rm_dup  })
  # 
  # observe({
  # 
  #     req(rm_ready())
  #     updateSelectInput(session, "rm_dup", choices = names(joined_df()))
  # 
  # 
  # 
  # })

  
  joined_df_2 <- reactive({
    
    if( !isTruthy(input$rm_dup) ){
      
    joined_df()
      
    }else{
    
    type <- if(input$rm_dup_type){"count"}else{"distinct"} 
    
    joined_df() %>% 
        remove_dup(type = type, vars = input$rm_dup)
      
    }
  })
  

# Display Joined Table -----------------------------------------------------------

  
  
  output$table_joined <- renderDataTable({
    
    joined_df_2()
    
  },options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
  
  
  
  
  output$raw <- renderPrint({ list( ls() ) })
  output$raw_2 <- renderPrint({  names(joined_df())})
  output$raw_3 <- renderPrint({  isTruthy( names(joined_df() ) )})
  
  
  
  # Download ---------------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(input$join_fun,"_", names(ls())[[1]], "_" ,names(ls())[[2]],".xlsx") #remove .xxx
      
    },
    content = function(file) {
      
      openxlsx::write.xlsx(joined_df_2(), file)
      
    }
  )
  
  
}

shinyApp(ui, server)