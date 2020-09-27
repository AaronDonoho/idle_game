library(shiny)
library(shinythemes)
library(highcharter)

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  includeCSS("./styles.css"),
  fluidRow(
    column(
      4,
      textOutput("cash"),
      textOutput("debt"),
      actionButton("pay_debt", "Pay Debts"),
      textOutput("interest_rate"),
      textOutput("research_points"),
      verbatimTextOutput("crop_info"),
      actionButton("plant_crop", "Plant"),
      actionButton("harvest_crop", "Harvest"),
      actionButton("sell_crop", "Sell"),
      br(), br(),
      tabsetPanel(
       type = "tabs",
       tabPanel(
         "Enhancements",
         uiOutput("enhancements") 
       ),
       tabPanel(
         "Hiring",
         uiOutput("hiring") 
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