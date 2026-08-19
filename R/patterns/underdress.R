## Generate underdress pattern ##
# This function generate_underdress() will return an object in the
#format new_pattern() which includes id = "underdress", units = "cm",
#pieces which is a list of the following information pieces(),
#which includes a list containing id and geometry of each piece.
#The pieces are front and back as well as left and right arm.


generate_underdress <- function(measurements, options = list()) {

  bust  <- measurements$bust #Needed as input in measurements
  length <- measurements$length #Needed as input in measurements
  shoulder_shoulder <- measurements$shoulder_shoulder #Needed as input in measurements
  armlength <- measurements$armlength #Needed as input in measurements (shoulder to hand)
  neck_shoulder <- measurements$neck_shoulder #Needed as input in measurements
  wrist <- measurements$wrist #Needed as input in measurements
  armhole <- measurements$armhole #Needed as input in measurements (shoulder to armhole)
  
  # Bottom is approximately 1.8 times
  # the shoulder width
  bottom_width <- shoulder_shoulder * 1.8

  #Armhole
  half_armhole <- armhole / 2
  half_wrist <- wrist /2

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
        cbind(
          neck_x,
          neck_y
        ),

        # Right shoulder
        c(half_shoulder, 0),

        c(half_bottom, length),
      
        # Bottom
        c(0, length),
        c(-half_bottom, length),

        # Close the ring back to the starting vertex - sf::st_polygon()
        # requires first == last or it errors with "polygons not (all)
        # closed" (confirmed: without this the piece can't be built at
        # all). The sleeve ring below already closes itself this way.
        c(-half_shoulder, 0)
      )
    )
  )

  geometry_arm <- sf::st_polygon(
    list(
      matrix(
        c(
          -half_armhole, 0,
           half_armhole, 0,

           half_wrist, armlength,

          -half_wrist, armlength,

          -half_armhole, 0
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
    id = "sleeve1",
    geometry = geometry_arm
  )

  piece4 <- new_pattern_piece(
    id = "sleeve2",
    geometry = geometry_arm
  )

  new_pattern(
    id = "underdress",
    units = "cm",
    pieces = list(piece1, piece2, piece3, piece4)
  )
}