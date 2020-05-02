library(tm)
library(R6)

source('./classes/Purchasable.R')

g_purchasable = FunctionGenerator(
  function(input, currency, event, item) {
    observeEvent(input[[event]], {
      req(currency() >= item$price$.())
      currency(currency() - item$price$.())
      item$count((item$count() + item$gain_add) * item$gain_mult)
      item$price$.((item$price$.() + item$price$add) * item$price$mult)
    })
  }
)

prune = function(data, limit) {
  pruned = data[c(round(seq(1, length(data), length(data) / limit)))]
  return(pruned)
}