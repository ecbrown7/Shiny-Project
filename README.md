README
================
Evan Brown
7/27/2022

-   [Brief description of the app and its
    purpose.](#brief-description-of-the-app-and-its-purpose)
-   [A list of packages needed to run the
    app.](#a-list-of-packages-needed-to-run-the-app)
-   [A line of code that would install all the packages
    used.](#a-line-of-code-that-would-install-all-the-packages-used)
-   [Run the app.](#run-the-app)

### Brief description of the app and its purpose.

This Shiny app does something.

### A list of packages needed to run the app.

``` r
data.table
tidyverse
shiny
```

### A line of code that would install all the packages used.

``` r
install.packages("data.table", "tidyverse", "shiny")
library(data.table)
library(tidyverse)
library(shiny)
```

### Run the app.

``` r
shiny::runGitHub(repo="Shiny-Project",username = "ecbrown7",ref = "main",subdir = "shinyfiles")
```
