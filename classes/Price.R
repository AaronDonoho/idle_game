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