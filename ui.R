library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  includeCSS("./styles.css"),
  fluidRow(
    column(
      4,
      textOutput("cash"),
      verbatimTextOutput("crop_info"),
      actionButton("plant_crop", "Plant"),
      actionButton("harvest_crop", "Harvest"),
      actionButton("sell_crop", "Sell"),
      br(), br(),
      tabsetPanel(
       type = "tabs",
       tabPanel(
         "Hiring",
         uiOutput("hiring") 
       ),
       tabPanel(
         "Enhancements",
         uiOutput("enhancements") 
       ),
       tabPanel(
         "Research",
         uiOutput("research") 
       )
      )
    ),
    column(
      4,
      uiOutput("table_header"),
      tableOutput("crop_table"),
      h3("Workers"),
      tableOutput("worker_table")),
    column(
      4,
      h3("Potatoes sold per second"),
      highchartOutput("psps_plot"))
  )
)