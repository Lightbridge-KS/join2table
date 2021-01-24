
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Shiny app : join 2 table

<!-- badges: start -->
<!-- badges: end -->

[`https://lightbridge-ai.shinyapps.io/join2table/`](https://lightbridge-ai.shinyapps.io/join2table/)

This shiny app implement R-package: `dplyr::*_join()` function into
web-application.

1.  User supply 2 files (in .csv or excel), choose key column to join
    and joining method .

2.  The apps will join 2 tables into 1 table. (also, has an option to
    remove rows duplication)

3.  User can download joined table in xlsx.

Main server code was inspired from
[`dplyr::*_join`](https://dplyr.tidyverse.org/reference/join.html)

Read more basic joining concept from [R4DS :
Relational-database](https://r4ds.had.co.nz/relational-data.html#relational-data)

![](www/joining_method.png)

Adapted from
[`dplyr::cheat sheet`](https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf)
