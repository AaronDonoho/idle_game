library(shiny)

ui <- fluidPage(
  fluidRow(
    column(3,
           textOutput("cash"),
           verbatimTextOutput("crop_info"),
           actionButton("plant_crop", "Plant"),
           br(),
           actionButton("harvest_crop", "Harvest"),
           br(),
           actionButton("sell_crop", "Sell"),
           uiOutput("power_up")
    ),
    column(3,
           uiOutput("table_header"),
           tableOutput("crop_table")),
    column(3,
           h3("Workers"),
           tableOutput("worker_table")),
    column(3,
           h3("Potatoes sold per second"),
           highchartOutput("psps_plot"))
  )
)