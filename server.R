library(shiny)
library(highcharter)
library(splines)

server <- function(input, output, session) {
  
  # timers
  tick_rate = 15
  slow_check = reactiveTimer(30 * tick_rate, session)
  very_slow_check = reactiveTimer(300 * tick_rate, session)
  psps_check_interval = 50 * tick_rate
  psps_check = reactiveTimer(psps_check_interval, session)
  crop_growth_check = reactiveTimer(1 * tick_rate, session)
  worker_check = reactiveTimer(4 * tick_rate, session)
  
  # currencies
  cash = reactiveVal(500)
  debt = reactiveVal(0)
  interest_rate = reactiveVal(0)
  research = reactiveVal(0)
  
  # player upgrades
  plant_quantity = PlantQuantity$new()
  sell_quantity = SellQuantity$new()
  crops_multiplier = CropsMultiplier$new()
  growth_multiplier = GrowthMultiplier$new()
  productivity_multiplier = ProductivityMultiplier$new()
  lab_equipment = LabEquipment$new()
  
  # workers
  planters = Planters$new()
  harvesters = Harvesters$new()
  haulers = Haulers$new()
  marketers = Marketers$new()
  researchers = Researchers$new()
  accountants = Accountants$new()
  
  # market
  price_plant_crop = 0
  price_sell_crop = reactive(2 + marketers$count())
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
    slow_check()
    isolate({
      interest_rate((log(debt() + 10000) - 9) / log(2 + accountants$count()))
    })
  })
  
  observe({
    very_slow_check()
    isolate({
      interest = round((debt() * interest_rate() / 100))
      debt(debt() + interest)
      if (interest > 0) {
        showNotification(paste0("Debt accrued $", interest," in interest"), type = 'warning')
      }
    })
  })
  
  observe({
    worker_check()
    isolate({
      debt(debt() +
             planters$count() * planters$recurring_cost +
             harvesters$count() * harvesters$recurring_cost +
             haulers$count() * haulers$recurring_cost +
             researchers$count() * researchers$recurring_cost +
             marketers$count() * marketers$recurring_cost +
             accountants$count() * accountants$recurring_cost)
      if (planters$count() > 0) {
        plant_crops(planters$count() * productivity_multiplier$count())
      }
      if (harvesters$count() > 0 && mature_crops() > 0) {
        harvest_crops(harvesters$count() * productivity_multiplier$count())
      }
      if (haulers$count() > 0 && stored_crops() > 0) {
        sell_crops(haulers$count() * productivity_multiplier$count())
      }
      research(research() + researchers$count() * (lab_equipment$count() + 1))
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
        mature_crops(mature_crops() + ceiling(crops_multiplier$count() * grown))
      }
    })
  })
  
  observeEvent(psps_check(), {
    n = length(sold_history) + 1
    sold_history[n] <<- crops_sold / psps_check_interval * 1000
    crops_sold <<- 0
    
    req(n > 5)
    
    spline = smooth.spline(sold_history, NULL, df=min(n, 16))$y
    
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
  
  observeEvent(input$pay_debt, {
    payment = min(debt(), cash())
    debt(debt() - payment)
    cash(cash() - payment)
  })
  
  output$vital_info <- renderText({
    paste0(
      "Cash: $", suffix(cash()), "\n",
      "Debt: $", suffix(debt()), "\n",
      "Interest Rate: ", formatC(interest_rate(), 2, format = 'f'), "%", "\n",
      "RP: ", suffix(research()), "\n",
      "Mature ", crop_name_plural, " can be sold for $", price_sell_crop(), "\n"
    )
  })
  
  output$table_header <- renderUI({
    h3(crop_name_plural)
  })
  
  output$crop_table <- renderTable({
    data.frame(growing = suffix(planted_crops()),
               mature = suffix(mature_crops()),
               storage = suffix(stored_crops()),
               sold = suffix(total_crops()))
  }, digits = 0)
  
  g_purchasable(input, cash, 'planter_hire', planters)
  g_purchasable(input, cash, 'harvester_hire', harvesters)
  g_purchasable(input, cash, 'hauler_hire', haulers)
  g_purchasable(input, cash, 'marketer_hire', marketers)
  g_purchasable(input, cash, 'researcher_hire', researchers)
  g_purchasable(input, cash, 'accountant_hire', accountants)
  g_purchasable(input, cash, 'improve_planting', plant_quantity)
  g_purchasable(input, cash, 'improve_selling', sell_quantity)
  g_purchasable(input, cash, 'improve_lab_equipment', lab_equipment)
  g_purchasable(input, research, 'improve_growth', growth_multiplier)
  g_purchasable(input, research, 'improve_extra_crops', crops_multiplier)
  g_purchasable(input, research, 'increased_productivity', productivity_multiplier)
  
  output$hiring <- renderUI({
    div(
      h4("Planters put potatoes in the ground"),
      h6("[", planters$count(), "]"),
      actionButton("planter_hire", paste0("$", suffix(planters$price$.()))),
      h4("Harvesters put mature potatoes into storage"),
      h6("[", harvesters$count(), "]"),
      actionButton("harvester_hire", paste0("$", suffix(harvesters$price$.()))),
      h4("Haulers sell stored potatoes"),
      h6("[", haulers$count(), "]"),
      actionButton("hauler_hire", paste0("$", suffix(haulers$price$.()))),
      h4("Marketers increase the price of potatoes"),
      h6("[", marketers$count(), "]"),
      actionButton("marketer_hire", paste0("$", suffix(marketers$price$.()))),
      h4("Researchers generate RP"),
      h6("[", researchers$count(), "]"),
      actionButton("researcher_hire", paste0("$", suffix(researchers$price$.()))),
      h4("Accountants reduce the interest rate"),
      h6("[", accountants$count(), "]"),
      actionButton("accountant_hire", paste0("$", suffix(accountants$price$.())))
    )
  })
  
  output$enhancements <- renderUI({
    div(
      h4("Plant an extra potato"),
      h6("[", plant_quantity$count(), "]"),
      actionButton("improve_planting", paste0("$", suffix(plant_quantity$price$.()))),
      h4("Sell an extra potato"),
      h6("[", sell_quantity$count(), "]"),
      actionButton("improve_selling", paste0("$", suffix(sell_quantity$price$.()))),
      h4("Improve lab equipment for more productive researchers"),
      h6("[", lab_equipment$count(), "]"),
      actionButton("improve_lab_equipment", paste0("$", suffix(lab_equipment$price$.())))
    )
  })
  
  output$research <- renderUI({
    div(
      h4("Increase yield amount at maturity"),
      h6("[x", growth_multiplier$count() %>% round(2), "]"),
      actionButton("improve_growth", paste(suffix(growth_multiplier$price$.()), "RP")),
      h4("Improve the chance for plants to mature"),
      h6("[x", crops_multiplier$count() %>% round(2), "]"),
      actionButton("improve_extra_crops", paste(suffix(crops_multiplier$price$.()), "RP")),
      h4("Increase the productivity of laborers"),
      h6("[x", productivity_multiplier$count() %>% round(2), "]"),
      actionButton("increased_productivity", paste(suffix(productivity_multiplier$price$.()), "RP"))
    )
  })
  
  output$psps_plot <- renderHighchart({
    sold_plot()
  })
  
  plant_crops = function(quantity) {
    planted_crops(planted_crops() + quantity)
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
    cash(cash() + count * price_sell_crop())
    total_crops(total_crops() + count)
    crops_sold <<- crops_sold + count
  }
  
}

