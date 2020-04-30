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
# random events (+ resistance to bad events such as drought, disease, insects... via mutations or research)
# replace one-click upgrades with researchers
# more upgrades: marketing (increase sell price), worker improvements
# meaningful choices:
#   expanding lands vs receiving benefits from that country
#   dna + mutations 
#   abilities + skill trees involving physics (spacetime, matter/energy transformation, electromagnetism)

server <- function(input, output, session) {
  tick_rate = 150
  
  # cash
  starting_cash = 500
  crops_sold = 0
  cash = reactiveVal(starting_cash)
  cash_check = reactiveTimer(1000, session)
  revenue_history = list(c(1, 0))
  revenue_plot = reactiveVal(
    highchart() %>%
      hc_xAxis(title = list(text = "Time")) %>%
      hc_add_series(name = 'psps', data = revenue_history) %>%
      hc_add_theme(hc_theme_darkunica()) %>%
      hc_plotOptions(series = list(animation = FALSE))
  )
  
  # player upgrades
  plant_quantity = PlantQuantity$new()
  sell_quantity = SellQuantity$new()
  crops_multiplier = CropsMultiplier$new()
  growth_multiplier = GrowthMultiplier$new()
  
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
  total_crops = reactiveVal(0)
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
        total_crops(total_crops() + count)
        crops_sold <<- crops_sold + count
      }
    })
  })
  
  observe({
    crop_growth_check()
    
    isolate({
      req(planted_crops() >= 1)
      random = runif(1, 0, 1)
      
      grown = 0
      if (random < 0.002 * growth_multiplier$count()) {
        # up to 1 + 60%
        grown = round(1 + planted_crops() * random * 3)
      } else if (random < 0.02 * growth_multiplier$count()) {
        # up to 1 + 20%
        grown = round(1 + planted_crops() * random * 0.5)
      } else if (random < 0.1 * growth_multiplier$count()) {
        # up to 1 + 10%
        grown = round(1 + (planted_crops() * random * 0.1))
      }
      
      if (grown > 0) {
        harvestable_crops(harvestable_crops() + crops_multiplier$count() * grown)
      }
    })
  })
  
  observeEvent(cash_check(), {
    n = length(revenue_history) + 1
    revenue_history[[n]] = c(n, crops_sold)
    revenue_history <<- revenue_history
    crops_sold <<- 0
    revenue_plot(
      revenue_plot() %>%
        hc_rm_series('psps') %>%
        hc_add_series(name = 'psps', data = revenue_history)
    )
  })
  
  observeEvent(input$plant_crop, {
    planted_crops(planted_crops() + plant_quantity$count())
  })
  
  observeEvent(input$harvest_crop, {
    req(harvestable_crops() > 0)
    count = min(1, harvestable_crops())
    harvestable_crops(harvestable_crops() - count)
    harvested_crops(harvested_crops() + count)
    total_crops(total_crops() + count)
  })
  
  observeEvent(input$sell_crop, {
    req(harvested_crops() > 0)
    count = min(harvested_crops(), sell_quantity$count())
    harvested_crops(harvested_crops() - count)
    cash(cash() + count * price_sell_crop)
    crops_sold <<- crops_sold + count
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
    data.frame(growing = planted_crops(), mature = harvestable_crops(), storage = harvested_crops(), sold = total_crops())
  })
  
  output$worker_table <- renderTable({
    data.frame(planters = planters$count(), harvesters = harvesters$count(), sellers = sellers$count())
  })
  
  g_purchasable(input, cash, 'planter_hire', planters)
  g_purchasable(input, cash, 'harvester_hire', harvesters)
  g_purchasable(input, cash, 'seller_hire', sellers)
  g_purchasable(input, cash, 'improve_planting', plant_quantity)
  g_purchasable(input, cash, 'improve_selling', sell_quantity)
  g_purchasable(input, cash, 'improve_growth', growth_multiplier)
  g_purchasable(input, cash, 'improve_extra_crops', crops_multiplier)

  output$power_up <- renderUI({
    div(
      h5("Hire a planter"),
      actionButton("planter_hire", paste0("$", round(planters$price$.(), 2))),
      h5("Hire a harvester"),
      actionButton("harvester_hire", paste0("$", round(harvesters$price$.(), 2))),
      h5("Hire a seller"),
      actionButton("seller_hire", paste0("$", round(sellers$price$.(), 2))),
      h5("Plant an extra potato"),
      actionButton("improve_planting", paste0("$", round(plant_quantity$price$.(), 2))),
      h5("Sell an extra potato"),
      actionButton("improve_selling", paste0("$", round(sell_quantity$price$.(), 2))),
      h5("Researching new seeds increases potato yields by 8%"),
      actionButton("improve_extra_crops", paste0("$", round(crops_multiplier$price$.(), 2))),
      h5("Fertilizer improves chance for potatoes to mature by 10%"),
      actionButton("improve_growth", paste0("$", round(growth_multiplier$price$.(), 2)))
    )
  })
  
  output$psps_plot <- renderHighchart({
    revenue_plot()
  })
  
}

