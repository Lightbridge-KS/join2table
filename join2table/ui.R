#### Join table

library(shiny)
library(dplyr)
library(purrr)
library(stringr)
library(rlang)


# UI ----------------------------------------------------------------------


ui <- fluidPage(
    #theme = shinythemes::shinytheme("sandstone"),
    titlePanel("Join 2 tables"),
    tags$a(href="https://github.com/Lightbridge-AI/join2table", "Source Code"),
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
               helpText(h4("Step 3 : Choose joining method"),
                        tags$a(href="https://dplyr.tidyverse.org/reference/join.html#grouping", "See Code"), "or",
                        tags$a(href="https://r4ds.had.co.nz/relational-data.html#relational-data", "Learn Theory !")),
               
               selectInput("join_fun", "Joining method:", choices = list("semi_join" = "semi_join",
                                                                         "anti_join" = "anti_join",
                                                                         "inner_join"= "inner_join",
                                                                         "left_join" = "left_join", 
                                                                         "right_join"= "right_join",
                                                                         "full_join" = "full_join")),
               
               tabsetPanel(
                   id = "tab_inform",
                   type = "hidden",
                   
                   tabPanel("semi_join",
                            helpText("Return all rows from table X where there are matching values in table Y, keeping just columns from table X.")),
                   
                   tabPanel("anti_join",
                            helpText("Return all rows from table X where there are", strong("not") ,"matching values in table Y, keeping just columns from table X.")),
                   tabPanel("inner_join",
                            helpText("Return all rows from table X where there are matching values in table Y, and all columns from X and Y. If there are multiple matches between X and Y, all combination of the matches are returned.")
                   ),
                   tabPanel("left_join",
                            helpText("Return all rows from table X, and all columns from X and Y. Rows in X with no match in Y will have NA values (blank) in the new columns. If there are multiple matches between X and Y, all combinations of the matches are returned.")),
                   tabPanel("right_join",
                            helpText("Return all rows from table Y, and all columns from X and Y. Rows in Y with no match in X will have NA values (blank) in the new columns. If there are multiple matches between X and Y, all combinations of the matches are returned.")),
                   tabPanel("full_join",
                            helpText("Return all rows and all columns from both x and y. Where there are not matching values, returns NA (blank) for the one missing."))
               )
               
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
    
    br(),
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
                 
                 img(src='joining_method.png', align = "left", height = 900 ,width = 1199),
                 tags$a(href="https://github.com/Lightbridge-AI/join2table/blob/main/www/joining_method.png", 
                        "Download picture"),
                 br(),
                 tags$a(href="https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf", 
                        "dplyr::cheatsheets")
                 
                 
        )
        
        
    )
    
    
    
    
    
    
)