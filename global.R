library(tm)
library(R6)

source('./classes/Purchasable.R')

g_purchasable = FunctionGenerator(
  function(input, currency, event, item) {
    observeEvent(input[[event]], {
      req(currency() >= item$price$.(), 3)
      currency(currency() - item$price$.())
      item$count((item$count() + item$gain_add) * item$gain_mult)
      item$price$hidden(ceiling((item$price$hidden() + item$price$add) * item$price$mult))
    })
  }
)

suffix = function(n) {
  ifelse (n > 10 ^ 15, paste0(round(n / 10 ^ 15, 2), 'q'),
  ifelse (n > 10 ^ 12, paste0(round(n / 10 ^ 12, 2), 't'),
  ifelse (n > 10 ^ 9,  paste0(round(n / 10 ^ 9,  2), 'b'),
  ifelse (n > 10 ^ 6,  paste0(round(n / 10 ^ 6,  2), 'm'),
  ifelse (n > 10 ^ 3,  paste0(round(n / 10 ^ 3,  2), 'k'),
  n)))))
}

prune = function(data, limit) {
  pruned = data[c(round(seq(1, length(data), length(data) / limit)))]
  return(pruned)
}