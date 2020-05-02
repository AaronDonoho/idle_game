library(shiny)
library(highcharter)
library(splines)

# what's next?
# --replace plot with plotly or similar
# --metrics: potatoes/sec sold
# canceled because of complications with sellers: fluctuating sell prices
# canceled because fluctuating sell prices canceled:storage limit + upgrades
# canceled exponential view of time for plots; replaced with pruning
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
# reduce network traffic usage of plot
# prune the data for plots
# random events (+ resistance to bad events such as drought, disease, insects... via mutations or research)
# replace one-click upgrades with researchers
# more upgrades: marketing (increase sell price), worker improvements
# meaningful choices:
#   expanding lands vs receiving benefits from that country
#   dna + mutations 
#   abilities + skill trees involving physics (spacetime, matter/energy transformation, electromagnetism)
# remove decimals
# purchasables cost only have up to 4 significant digits (e.g. 1105030.34 => 1105000)
# change numerical display: 1000 is 1k, 1000000 is 1m, 1105030 is 1.105m

server <- function(input, output, session) {
  # timers
  tick_rate = 150
  psps_check = reactiveTimer(1000, session)
  crop_growth_check = reactiveTimer(1 * tick_rate, session)
  worker_check = reactiveTimer(4 * tick_rate, session)
  
  # currencies
  cash = reactiveVal(500)
  research = reactiveVal(0)
  
  # player upgrades
  plant_quantity = PlantQuantity$new()
  sell_quantity = SellQuantity$new()
  crops_multiplier = CropsMultiplier$new()
  growth_multiplier = GrowthMultiplier$new()
  
  # workers
  planters = Planters$new()
  harvesters = Harvesters$new()
  sellers = Sellers$new()
  researchers = Researchers$new()
  
  # market
  price_plant_crop = 0
  price_sell_crop = 2
  crop_name = "Potato"
  crop_name_plural = "Potatoes"
  crops_sold = 0
  sold_history = c()
  sold_plot = reactiveVal(
    highchart() %>%
      hc_xAxis(title = list(text = "Time")) %>%
      hc_yAxis(min = 0) %>%
      hc_add_theme(hc_theme_darkunica()) %>%
      hc_plotOptions(series = list(animation = FALSE))
  )
  
  # storage
  planted_crops = reactiveVal(0)
  mature_crops = reactiveVal(0)
  stored_crops = reactiveVal(0)
  total_crops = reactiveVal(0)
  
  observe({
    worker_check()
    isolate({
      if (planters$count() > 0 && cash() > 0) {
        plant_crops(planters$count())
      }
      if (harvesters$count() > 0 && mature_crops() > 0) {
        harvest_crops(harvesters$count())
      }
      if (sellers$count() > 0 && stored_crops() > 0) {
        sell_crops(sellers$count())
      }
      research(research() + researchers$count())
    })
  })
  
  observe({
    crop_growth_check()
    
    isolate({
      req(planted_crops() >= 1)
      random = runif(1, 0, 1)
      
      grown = 0
      if (random < 0.0015 * growth_multiplier$count()) {
        grown = ceiling(planted_crops() * random * 3)
      } else if (random < 0.01 * growth_multiplier$count()) {
        grown = ceiling(planted_crops() * random * 0.5)
      } else if (random < 0.025 * growth_multiplier$count()) {
        grown = ceiling(planted_crops() * random * 0.1)
      }
      
      if (grown > 0) {
        mature_crops(mature_crops() + crops_multiplier$count() * grown)
      }
    })
  })
  
  observeEvent(psps_check(), {
    n = length(sold_history) + 1
    sold_history[n] = crops_sold
    sold_history <<- sold_history
    crops_sold <<- 0
    
    req(n > 30)
    
    # if (n > 200) {
    #   revenue_history <<- prune(revenue_history, 100)
    # }
    
    spline = smooth.spline(sold_history, NULL, df=16)$y
    
    sold_plot(
      sold_plot() %>%
        hc_rm_series('spline') %>%
        hc_add_series(name = 'spline', data = spline)
    )
  })
  
  observeEvent(input$plant_crop, {
    plant_crops(plant_quantity$count())
  })
  
  observeEvent(input$harvest_crop, {
    harvest_crops(1)
  })
  
  observeEvent(input$sell_crop, {
    sell_crops(sell_quantity$count())
  })
  
  output$cash <- renderText({
    paste0("Cash: $", round(cash(), 2))
  })
  
  output$research_points <- renderText({
    paste0("RP: ", round(research(), 2))
  })
  
  output$crop_info <- renderText({
    paste0(crop_name, " seeds cost $", price_plant_crop, "\n",
           "Mature ", crop_name_plural, " sell for $", price_sell_crop)
  })
  
  output$table_header <- renderUI({
    h3(crop_name_plural)
  })
  
  output$crop_table <- renderTable({
    data.frame(growing = planted_crops(), mature = mature_crops(), storage = stored_crops(), sold = total_crops())
  })
  
  output$worker_table <- renderTable({
    data.frame(planters = planters$count(), harvesters = harvesters$count(), sellers = sellers$count())
  })
  
  g_purchasable(input, cash, 'planter_hire', planters)
  g_purchasable(input, cash, 'harvester_hire', harvesters)
  g_purchasable(input, cash, 'seller_hire', sellers)
  g_purchasable(input, cash, 'researcher_hire', researchers)
  g_purchasable(input, cash, 'improve_planting', plant_quantity)
  g_purchasable(input, cash, 'improve_selling', sell_quantity)
  g_purchasable(input, research, 'improve_growth', growth_multiplier)
  g_purchasable(input, research, 'improve_extra_crops', crops_multiplier)

  output$hiring <- renderUI({
    div(
      h4("Hire a planter"),
      actionButton("planter_hire", paste0("$", round(planters$price$.(), 2))),
      h4("Hire a harvester"),
      actionButton("harvester_hire", paste0("$", round(harvesters$price$.(), 2))),
      h4("Hire a seller"),
      actionButton("seller_hire", paste0("$", round(sellers$price$.(), 2))),
      h4("Hire a researcher"),
      actionButton("researcher_hire", paste0("$", round(researchers$price$.(), 2)))
    )
  })
  
  output$enhancements <- renderUI({
    div(
      h4("Plant an extra potato"),
      actionButton("improve_planting", paste0("$", round(plant_quantity$price$.(), 2))),
      h4("Sell an extra potato"),
      actionButton("improve_selling", paste0("$", round(sell_quantity$price$.(), 2)))
    )
  })
  
  output$research <- renderUI({
    div(
      h4("Fertilized plants have increased yields at maturity"),
      actionButton("improve_growth", paste(round(growth_multiplier$price$.(), 2), "RP")),
      h4("Higher quality seeds improves the chance for plants to mature"),
      actionButton("improve_extra_crops", paste(round(crops_multiplier$price$.(), 2), "RP"))
    )
  })
  
  output$psps_plot <- renderHighchart({
    sold_plot()
  })
  
  plant_crops = function(quantity) {
    count = min(quantity, cash() / price_plant_crop)
    planted_crops(planted_crops() + count)
    cash(cash() - count * price_plant_crop)
  }
  
  harvest_crops = function(quantity) {
    req(mature_crops() > 0)
    count = min(quantity, mature_crops())
    mature_crops(mature_crops() - count)
    stored_crops(stored_crops() + count)
  }
  
  sell_crops = function(quantity) {
    req(stored_crops() > 0)
    count = min(quantity, stored_crops())
    stored_crops(stored_crops() - count)
    cash(cash() + count * price_sell_crop)
    total_crops(total_crops() + count)
    crops_sold <<- crops_sold + count
  }
  
}

