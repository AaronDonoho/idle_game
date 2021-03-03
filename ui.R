library(shiny)
library(shinythemes)
library(highcharter)

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  includeCSS("./styles.css"),
  fluidRow(
    column(
      4,
      verbatimTextOutput("vital_info"),
      actionButton("pay_debt", "Pay Debts"),
      br(),
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
      tableOutput("crop_table")),
    column(
      4,
      h3("Potatoes sold per second"),
      highchartOutput("psps_plot"))
  )
)