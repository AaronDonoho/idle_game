library(tm)
library(R6)

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
# . refers to itself to avoid extra verbose code
Price = R6Class("Price", public = list(
  . = NULL,
  add = NULL,
  mult = NULL,
  
  initialize = function(., add, mult) {
    self$. = reactiveVal(.)
    self$add = add
    self$mult = mult
  }
))

Purchasable = R6Class("Purchasable",
  public = list(
    price = NULL,
    count = NULL,
    gain_add = NULL,
    gain_mult = NULL,
    
    initialize = function(price, count, gain_add = 1, gain_mult = 1) {
      self$price = price
      self$count = reactiveVal(count)
      self$gain_add = gain_add
      self$gain_mult = gain_mult
    }
))

Planters = R6Class("Planters", inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(300, 0, 1.14), count = 0) {
      super$initialize(price, count)
    }
  )
)

Sellers = R6Class("Sellers", inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(200, 0, 1.12), count = 0) {
      super$initialize(price, count)
    }
  )
)

Harvesters = R6Class("Harvesters", inherit = Purchasable,
  public = list(
   initialize = function(price = Price$new(100, 0, 1.08), count = 0) {
     super$initialize(price, count)
   }
  )
)