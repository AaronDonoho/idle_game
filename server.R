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
# worker wages options:
#   1. a running debt you can pay off
#     what are the downsides of having debt?
#       1. could build interest over time
#       2. prevents you from performing certain actions
#   2. a constant drain on funds
#     what happens when money runs out?
#       1. workers stop working
#       2. workers quit 
#       3. workers work as quickly as your funds will pay them
# add a stock market - issuing shares, stock price, dividends, buy backs
# --seed potatoes stick around to produce more
# --planting should be free
# use exponential view of time for plots
# reduce network traffic usage
# replace one-click upgrades with researchers
# more upgrades: marketing (increase sell price), worker improvements


server <- function(input, output, session) {
  tick_rate = 150
  
  # cash
  cash = reactiveVal(100)
  cash_check = reactiveTimer(1000, session)
  cash_history = c()
  cashplot = reactiveVal(
    highchart() %>%
      hc_add_series(name = 'cash', data = cash_history) %>%
      hc_add_theme(hc_theme_darkunica()) %>%
      hc_plotOptions(series = list(animation = FALSE))
  )
  
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
  worker_check = reactiveTimer(4 * tick_rate, session)
  
  planters = Planters$new()
  harvesters = Harvesters$new()
  sellers = Sellers$new()
  
  # crops
  price_plant_crop = 0
  price_sell_crop = 2
  crop_name = "Potato"
  crop_name_plural = "Potatoes"
  
  # storage
  planted_crops = reactiveVal(0)
  harvestable_crops = reactiveVal(0)
  harvested_crops = reactiveVal(0)
  crop_growth_check = reactiveTimer(1 * tick_rate, session)
  
  observe({
    worker_check()
    isolate({
      if (planters$count() > 0 && cash() > 0) {
        count = min(planters$count(), cash() / price_plant_crop)
        planted_crops(planted_crops() + count)
        cash(cash() - count * price_plant_crop)
      }
      if (harvesters$count() > 0 && harvestable_crops() > 0) {
        count = min(harvestable_crops(), harvesters$count())
        harvestable_crops(harvestable_crops() - count)
        harvested_crops(harvested_crops() + count)  
      }
      if (sellers$count() > 0 && harvested_crops() > 0) {
        count = min(harvested_crops(), sellers$count())
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
        grown = round(1 + planted_crops() * random * 3)
      } else if (random < 0.02 * growth_multiplier()) {
        # up to 1 + 20%
        grown = round(1 + planted_crops() * random * 0.5)
      } else if (random < 0.1 * growth_multiplier()) {
        # up to 1 + 10%
        grown = round(1 + (planted_crops() * random * 0.1))
      }
      
      if (grown > 0) {
        harvestable_crops(harvestable_crops() + extra_crops_multiplier() * grown)
      }
    })
  })
  
  observeEvent(cash_check(), {
    cash_history <<- c(cash_history, cash())
    cashplot(
      cashplot() %>%
        hc_rm_series('cash') %>%
        hc_add_series(name = 'cash', data = cash_history)
    )
  })
  
  observeEvent(input$plant_crop, {
    planted_crops(planted_crops() + plant_quantity())
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
    data.frame(planters = planters$count(), harvesters = harvesters$count(), sellers = sellers$count())
  })
  
  g_purchasable(input, cash, 'planter_hire', planters)
  g_purchasable(input, cash, 'harvester_hire', harvesters)
  g_purchasable(input, cash, 'seller_hire', sellers)
  # g_purchasable(input, cash, 'improve_planting', plant_quantity, price_improve_planting, cost_mult = 1.15)
  # g_purchasable(input, cash, 'improve_selling', sell_quantity, price_improve_selling, cost_mult = 1.2)
  # g_purchasable(input, cash, 'improve_growth', growth_multiplier, price_improve_growth, 0, gain_mult = 1.1, cost_mult = 1.58)
  # g_purchasable(input, cash, 'improve_extra_crops', extra_crops_multiplier, price_improve_extra_crops, 0, gain_mult = 1.08, cost_mult = 1.43)
  
  output$power_up <- renderUI({
    div(
      h5("Hire a planter"),
      actionButton("planter_hire", paste0("$", round(planters$price$.(), 2))),
      h5("Hire a harvester"),
      actionButton("harvester_hire", paste0("$", round(harvesters$price$.(), 2))),
      h5("Hire a seller"),
      actionButton("seller_hire", paste0("$", round(sellers$price$.(), 2))),
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
    cashplot()
  })
  
}

