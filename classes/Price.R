Price = R6Class("Price", public = list(
  hidden = NULL,
  add = NULL,
  mult = NULL,
  . = function(){round(self$hidden()/10) * 10},
  
  initialize = function(hiddenPrice, add, mult) {
    self$hidden = reactiveVal(hiddenPrice)
    self$add = add
    self$mult = mult
  }
  
))