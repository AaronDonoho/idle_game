source('./classes/Price.R')

Purchasable = R6Class(
  "Purchasable",
  public = list(
    price = NULL,
    recurring_cost = NULL,
    count = NULL,
    gain_add = NULL,
    gain_mult = NULL,
    
    initialize = function(price, recurring_cost = 0, count, gain_add = 1, gain_mult = 1) {
      self$price = price
      self$recurring_cost = recurring_cost
      self$count = reactiveVal(count)
      self$gain_add = gain_add
      self$gain_mult = gain_mult
    }
  )
)

LabEquipment = R6Class(
  "Lab Equipment",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(500, 0, 2), recurring_cost = 0, count = 0) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

Planters = R6Class(
  "Planters",
  inherit = Purchasable,
  public = list(
   initialize = function(price = Price$new(150, 0, 1.14), recurring_cost = 1, count = 0) {
     super$initialize(price = price, recurring_cost = recurring_cost, count = count)
   }
  )
)

Harvesters = R6Class(
  "Harvesters",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(100, 0, 1.08), recurring_cost = 1, count = 0) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

Sellers = R6Class(
  "Sellers",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(200, 0, 1.12), recurring_cost = 1, count = 0) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

Marketers = R6Class(
  "Marketers",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(250, 0, 1.31), recurring_cost = 1, count = 0) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

Researchers = R6Class(
  "Researchers",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(300, 0, 1.28), recurring_cost = 1, count = 0) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

PlantQuantity = R6Class(
  "PlantQuantity",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(25, 0, 1.05), recurring_cost = 0, count = 1) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

SellQuantity = R6Class(
  "SellQuantity",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(25, 0, 1.05), recurring_cost = 0, count = 1) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count)
    }
  )
)

GrowthMultiplier = R6Class(
  "GrowthMultiplier",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(150, 0, 1.55), recurring_cost = 0, count = 1, gain_add = 0.02, gain_mult = 1.02) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count, gain_add = gain_add, gain_mult = gain_mult)
    }
  )
)

CropsMultiplier = R6Class(
  "CropsMultiplier",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(300, 0, 1.62), recurring_cost = 0, count = 1, gain_add = 0.01, gain_mult = 1.03) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count, gain_add = gain_add, gain_mult = gain_mult)
    }
  )
)

ProductivityMultiplier = R6Class(
  "ProductivityMultiplier",
  inherit = Purchasable,
  public = list(
    initialize = function(price = Price$new(200, 0, 2.15), recurring_cost = 0, count = 1, gain_add = 1, gain_mult = 1) {
      super$initialize(price = price, recurring_cost = recurring_cost, count = count, gain_add = gain_add, gain_mult = gain_mult)
    }
  )
)