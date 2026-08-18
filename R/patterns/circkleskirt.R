## Generate circleskirt pattern ##
# This function generate_circleskirt() will return an object in the
#format new_pattern() which includes id = "side", units = "cm",
#pieces which is a list of the following information new_pattern_piece(),
#which includes; id = "side", geometry = geometry, quantity = 2, grainline = NULL, metadata which is a list of seam_allowance = options$seam_allowance
# and name = "Side" 




generate_circleskirt <- function(measurements, options = list()) {

  waist  <- measurements$waist #Needed as input in measurements
  length <- measurements$length #Needed as input in measurements


  ## Calculate the radius of the waist opening.
  # For a semicircle, the curved edge represents half of the waist circumference, 
  # waist = pi * r
  r_waist <- waist / pi

  # The outer radius is the waist radius plus the desired skirt length.
  r_outer <- r_waist + length

  # Semicircle: angle 0 -> pi
  # Create 100 evenly spaced angles from 0 to pi radians for a smooth semicircular curve.
  angles <- seq(0, pi, length.out = 100)

  # Calculate the (x, y) coordinates of the OUTER semicircle.
  # cos() determines the horizontal (x) position.
  # sin() determines the vertical (y) position.
  outer <- cbind(
    r_outer * cos(angles),
    r_outer * sin(angles)
  )

  inner <- cbind(
    r_waist * cos(rev(angles)),
    r_waist * sin(rev(angles))
  )
  #Creating coordinates for the geometry
  # Calculate the (x, y) coordinates of the INNER semicircle (the waist opening).
  #rev(angles) reverses the order of the points so that,
  # when we join outer and inner together, the polygon boundary
  # travels around the shape without crossing itself.
  coords <- rbind(outer, inner, outer[1, ])
  #Input the coordinates into a sf polygon shape
  geometry <- sf::st_polygon(list(coords))

  #Description of the pieces
  piece <- new_pattern_piece(
    id = "side",
    geometry = geometry,
    quantity = 2,
    grainline = NULL,
    metadata = list(
      name = "Side"
    )
  )

  new_pattern(
    id = "circleskirt",
    units = "cm",
    pieces = list(piece),
    metadata = list(
      seam_allowance = options$seam_allowance
    )
  )
}