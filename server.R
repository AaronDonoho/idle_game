library(shiny)
library(highcharter)

# what's next?
# --replace plot with plotly or similar
# metrics: potatoes/sec into storage
# canceled because of complications with sellers: fluctuating sell prices
# canceled because fluctuating sell prices canceled:storage limit + upgrades
# --auto-planter
# --auto-seller
# improved layout
# worker wages
# more upgrades: marketing (increase sell price)


server <- function(input, output, session) {
  # cash
  cash_history = reactiveVal(c())
  cash = reactiveVal(100)
  
  # player upgrades
  plant_quantity = reactiveVal(1)
  sell_quantity = reactiveVal(1)
  price_improve_planting = reactiveVal(10)
  price_improve_selling = reactiveVal(10)
  price_improve_extra_crops = reactiveVal(10)
  extra_crops_multiplier = reactiveVal(1)
  price_improve_growth = reactiveVal(10)
  growth_multiplier = reactiveVal(1)
  
  # workers
  price_buy_planter = reactiveVal(10)
  price_buy_harvester = reactiveVal(10)
  price_buy_seller = reactiveVal(10)
  planters = reactiveVal(0)
  harvesters = reactiveVal(0)
  sellers = reactiveVal(0)
  worker_check = reactiveTimer(150, session)
  
  # crops
  price_plant_crop = 1
  price_sell_crop = 2
  crop_name = "Potato"
  crop_name_plural = "Potatoes"
  
  # storage
  planted_crops = reactiveVal(0)
  harvestable_crops = reactiveVal(0)
  harvested_crops = reactiveVal(0)
  crop_growth_check = reactiveTimer(150, session)
  
  observe({
    worker_check()
    isolate({
      if (planters() > 0 && cash() > 0) {
        count = min(planters(), cash() / price_plant_crop)
        planted_crops(planted_crops() + count)
        cash(cash() - count * price_plant_crop)
      }
      if (harvesters() > 0 && harvestable_crops() > 0) {
        count = min(harvestable_crops(), harvesters())
        harvestable_crops(harvestable_crops() - count)
        harvested_crops(harvested_crops() + count)  
      }
      if (sellers() > 0 && harvested_crops() > 0) {
        count = min(harvested_crops(), sellers())
        harvested_crops(harvested_crops() - count)
        cash(cash() + count * price_sell_crop)
      }
    })
  })
  
  observe({
    crop_growth_check()
    
    isolate({
      req(planted_crops() >= 1)
      random = runif(1, 0, 1)
      
      grown = 0
      if (random < 0.002 * growth_multiplier()) {
        # up to 1 + 60%
        grown = round(1 + planted_crops() * random * 300)
      } else if (random < 0.02 * growth_multiplier()) {
        # up to 1 + 20%
        grown = round(1 + planted_crops() * random * 10)
      } else if (random < 0.1 * growth_multiplier()) {
        # up to 1 + 10%
        grown = round(1 + (planted_crops() * random))
      }
      
      if (grown > 0 && planted_crops() > grown) {
        planted_crops(planted_crops() - grown)
        harvestable_crops(harvestable_crops() + extra_crops_multiplier() * grown)
      }
    })
  })
  
  observeEvent(crop_growth_check(), {
    cash_history(append(cash_history(), cash()))
  })
  
  observeEvent(input$plant_crop, {
    req(cash() >= price_plant_crop)
    count = min(plant_quantity(), cash() / price_plant_crop)
    cash(cash() - count * price_plant_crop)
    planted_crops(planted_crops() + count)
  })
  
  observeEvent(input$harvest_crop, {
    req(harvestable_crops() > 0)
    count = min(1, harvestable_crops())
    harvestable_crops(harvestable_crops() - count)
    harvested_crops(harvested_crops() + count)
  })
  
  observeEvent(input$sell_crop, {
    req(harvested_crops() > 0)
    count = min(harvested_crops(), sell_quantity())
    harvested_crops(harvested_crops() - count)
    cash(cash() + count * price_sell_crop)
  })
  
  observeEvent(input$planter_hire, {
    req(cash() >= price_buy_planter())
    cash(cash() - price_buy_planter())
    planters(planters() + 1)
    price_buy_planter(price_buy_planter() * 1.23)
  })
  
  observeEvent(input$harvester_hire, {
    req(cash() >= price_buy_harvester())
    cash(cash() - price_buy_harvester())
    harvesters(harvesters() + 1)
    price_buy_harvester(price_buy_harvester() * 1.23)
  })
  
  observeEvent(input$seller_hire, {
    req(cash() >= price_buy_seller())
    cash(cash() - price_buy_seller())
    sellers(sellers() + 1)
    price_buy_seller(price_buy_seller() * 1.23)
  })
  
  observeEvent(input$improve_planting, {
    req(cash() >= price_improve_planting())
    cash(cash() - price_improve_planting())
    plant_quantity(plant_quantity() + 1)
    price_improve_planting(price_improve_planting() * 1.15) 
  })
  
  observeEvent(input$improve_selling, {
    req(cash() >= price_improve_selling())
    cash(cash() - price_improve_selling())
    sell_quantity(sell_quantity() + 1)
    price_improve_selling(price_improve_selling() * 1.2) 
  })
  
  observeEvent(input$improve_extra_crops, {
    req(cash() >= price_improve_extra_crops())
    cash(cash() - price_improve_extra_crops())
    extra_crops_multiplier(extra_crops_multiplier() * 1.08)
    price_improve_extra_crops(price_improve_extra_crops() * 1.43) 
  })
  
  observeEvent(input$improve_growth, {
    req(cash() >= price_improve_growth())
    cash(cash() - price_improve_growth())
    growth_multiplier(growth_multiplier() * 1.1)
    price_improve_growth(price_improve_growth() * 1.58) 
  })
  
  output$cash <- renderText({
    paste0("Money: $", round(cash(), 2))
  })
  
  output$crop_info <- renderText({
    paste0(crop_name, " seeds cost $", price_plant_crop, "\n",
           "Mature ", crop_name_plural, " sell for $", price_sell_crop)
  })
  
  output$table_header <- renderUI({
    h3(crop_name_plural)
  })
  
  output$crop_table <- renderTable({
    data.frame(growing = planted_crops(), mature = harvestable_crops(), storage = harvested_crops())
  })
  
  output$worker_table <- renderTable({
    data.frame(planters = planters(), harvesters = harvesters(), sellers = sellers())
  })
  
  output$power_up <- renderUI({
    div(
      h5("Hire a planter"),
      actionButton("planter_hire", paste0("$", round(price_buy_planter(), 2))),
      h5("Hire a harvester"),
      actionButton("harvester_hire", paste0("$", round(price_buy_harvester(), 2))),
      h5("Hire a seller"),
      actionButton("seller_hire", paste0("$", round(price_buy_seller(), 2))),
      h5("Plant an extra potato"),
      actionButton("improve_planting", paste0("$", round(price_improve_planting(), 2))),
      h5("Sell an extra potato"),
      actionButton("improve_selling", paste0("$", round(price_improve_selling(), 2))),
      h5("Researching new seeds increases potato yields by 8%"),
      actionButton("improve_extra_crops", paste0("$", round(price_improve_extra_crops(), 2))),
      h5("Fertilizer improves chance for potatoes to mature by 10%"),
      actionButton("improve_growth", paste0("$", round(price_improve_growth(), 2)))
    )
  })
  
  output$cash_plot <- renderHighchart({
    highchart() %>%
      hc_add_series(cash_history()) %>%
      hc_add_theme(hc_theme_darkunica()) %>%
      hc_plotOptions(series = list(animation = FALSE))
  })
  
}

