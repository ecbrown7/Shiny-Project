# This is the user-interface definition of a Shiny web application. You can

library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)

#UI
shinyUI(navbarPage("AR2 Assay Data App", theme = shinytheme("flatly"),
                   tabPanel("Introduction",
                            titlePanel("Introduction to AR2 Assay"),
                    sidebarLayout(
                    sidebarPanel("Description"),
                    mainPanel())),
                   
                   tabPanel("Data Visualization",
                            titlePanel("AR2 Antagonist Plots"),
                     sidebarLayout(
                     sidebarPanel("Description"),
                     mainPanel())),
        
                   tabPanel("Modeling",
                            titlePanel("Chemotype Modeling for AR2 Assay Hits"),
                      sidebarLayout(
                      sidebarPanel("Description"),
                      mainPanel()))
))


