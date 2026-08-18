## Generate bag pattern ##
# This function generate_bag() will return an object in the
#format new_pattern() which includes id = "bag", units = "cm",
#pieces which is a list of the following information new_pattern_piece(),
#which includes; id = "full", geometry = geometry.


generate_bag <- function(measurements, options = list()) {

  width  <- measurements$width #Needed as input in measurements
  height <- measurements$height * 2 #Needed as input in measurements

  geometry <- sf::st_polygon(list(
    matrix(
      c(
        0, 0,
        width, 0,
        width, height,
        0, height,
        0, 0
      ),
      ncol = 2,
      byrow = TRUE
    )
  ))

  piece <- new_pattern_piece(
    id = "full",
    geometry = geometry
  )

  new_pattern(
    id = "bag",
    units = "cm",
    pieces = list(piece)
  )
}