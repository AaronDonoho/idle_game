source('./classes/Price.R')

Purchasable = R6Class(
  "Purchasable",
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
  )
)

Planters = R6Class(
  "Planters",
  inherit = Purchasable,
  public = list(
   initialize = function(price = Price$new(300, 0, 1.14), count = 0) {
     super$initialize(price, count)
   }
  )
)

Sellers = R6Class(
  "Sellers",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(200, 0, 1.12), count = 0) {
      super$initialize(price, count)
    }
  )
)

Harvesters = R6Class(
  "Harvesters",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(100, 0, 1.08), count = 0) {
      super$initialize(price, count)
    }
  )
)

PlantQuantity = R6Class(
  "PlantQuantity",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(25, 0, 1.05), count = 1) {
      super$initialize(price, count)
    }
  )
)

SellQuantity = R6Class(
  "SellQuantity",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(25, 0, 1.05), count = 1) {
      super$initialize(price, count)
    }
  )
)

GrowthMultiplier = R6Class(
  "GrowthMultiplier",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(150, 0, 1.65), count = 0, gain_mult = 1.08) {
      super$initialize(price, count)
    }
  )
)

CropsMultiplier = R6Class(
  "CropsMultiplier",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(300, 0, 1.52), count = 0, gain_mult = 1.1) {
      super$initialize(price, count)
    }
  )
)