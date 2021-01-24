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
    
    observeEvent(input$join_fun,{
        updateTabsetPanel(session, "tab_inform", selected = input$join_fun)
        
    })
    
    
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
    
    
    
    
    # output$raw <- renderPrint({ list( ls() ) })
    # output$raw_2 <- renderPrint({  names(joined_df())})
    # output$raw_3 <- renderPrint({  isTruthy( names(joined_df() ) )})
    
    
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
