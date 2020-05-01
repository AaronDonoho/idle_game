library(tm)
library(R6)

source('./classes/Purchasable.R')

g_purchasable = FunctionGenerator(
  function(input, cash, event, purchasable) {
    observeEvent(input[[event]], {
      req(cash() >= purchasable$price$.())
      cash(cash() - purchasable$price$.())
      purchasable$count((purchasable$count() + purchasable$gain_add) * purchasable$gain_mult)
      purchasable$price$.((purchasable$price$.() + purchasable$price$add) * purchasable$price$mult)
    })
  }
)

prune = function(data, limit) {
  pruned = data[c(round(seq(1, length(data), length(data) / limit)))]
  return(pruned)
}