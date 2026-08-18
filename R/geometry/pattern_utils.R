
# Adding pattern helping functions

new_pattern_piece <- function(id, geometry) {
      list(id = id, geometry = geometry)
  }

new_pattern <- function(id, units, pieces)(
    list(id = id, units = units, pieces = pieces)
  )