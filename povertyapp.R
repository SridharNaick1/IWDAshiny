###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.


library(shiny)

library(shinydashboard)
library(dplyr)
library(reshape2)
library(shinyWidgets)
library(ggplot2)
library(forcats)
library(labelled)
library(plotly)
library(tidyverse)
library(ordinal)
library(ICC)

library(collapsibleTree)
library(expss)
library(formattable)
library(shinyjs)
library(data.table)
library(ggcorrplot)

#dimensions <- data[,to_compare]



source('./ui.R')
source('./server.R')


shinyApp(ui, server)
