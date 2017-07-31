library(shiny)
library(shinythemes)
ui=fluidPage(theme = shinytheme("united"),DT::dataTableOutput('tbl'))