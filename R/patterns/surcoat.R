## Generate surcoat pattern ##
# This function generate_surcoat()) will return an object in the
#format new_pattern() which includes id = "surcoat", units = "cm",
#pieces which is a list of the following information pieces(),
#which includes a list containing id and geometry of each piece.
#The pieces are body1 and body2 as well as sideleft1&2 and sideright1&2.


generate_surcoat <- function(measurements, options = list()) {

  bust  <- measurements$bust #Needed as input in measurements
  length <- measurements$length #Needed as input in measurements
  shoulder_shoulder <- measurements$shoulder_shoulder #Needed as input in measurements
  neck_shoulder <- measurements$neck_shoulder #Needed as input in measurements
  armhole <- measurements$armhole #Needed as input in measurements (shoulder to armhole)
  
  # Bottom is approximately 1.8 times
  # the shoulder width
  bottom_width <- shoulder_shoulder * 1.8

  trapezoid_bottom <- bottom_width / 3
  trapezoid_top <- (bust - 2 * shoulder_shoulder) / 2
  trapezoid_length <- length - armhole
  
  # Neck dimensions
  neck_width <- shoulder_shoulder - neck_shoulder
  neck_depth <- neck_width

  half_shoulder <- shoulder_shoulder / 2
  half_bottom <- bottom_width / 2
  half_neck <- neck_width / 4

  # x coordinates across the neck opening
  neck_x <- seq(
    -half_neck,
    half_neck,
    length.out = 30
  )
  
  # Elliptical/rounded neck opening.
  #
  # At the shoulders:
  # y = 0
  #
  # At the centre:
  # y = neck_depth

  neck_y <- neck_depth *
    sqrt(
      pmax(
        0,
        1 - (neck_x / half_neck)^2
      )
  )

  geometry <- sf::st_polygon(
  list(
    rbind(
      # Left shoulder
      c(-half_shoulder, 0),

      # Left side of neck opening
      c(-half_neck, 0),

      # Neck curve, left -> right
      cbind(neck_x, neck_y),

      # Right shoulder
      c(half_shoulder, 0),

      # Bottom-right
      c(half_bottom, length),

      # Bottom-left
      c(-half_bottom, length),

      # Close polygon
      c(-half_shoulder, 0)
    )
  )
)

  geometry_side<- sf::st_polygon(
    list(
      matrix(
        c(
          -trapezoid_top / 2 , 0,
           trapezoid_top / 2, 0,

           trapezoid_bottom / 2, trapezoid_length,

          -trapezoid_bottom / 2, trapezoid_length,

          -trapezoid_top / 2, 0
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )


  piece1 <- new_pattern_piece(
    id = "body1",
    geometry = geometry
  )

  piece2 <- new_pattern_piece(
    id = "body2",
    geometry = geometry
  )

  piece3 <- new_pattern_piece(
    id = "side1",
    geometry = geometry_side
  )

  piece4 <- new_pattern_piece(
    id = "side2",
    geometry = geometry_side
  )

  piece5 <- new_pattern_piece(
    id = "side3",
    geometry = geometry_side
  )

  piece6 <- new_pattern_piece(
    id = "side4",
    geometry = gexometry_side
  )

  new_pattern(
    id = "surcoat",
    units = "cm",
    pieces = list(piece1, piece2, piece3, piece4, piece5, piece6)
  )
}
