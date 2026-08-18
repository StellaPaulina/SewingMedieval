## Generate underdress pattern ##
# This function generate_underdress() will return an object in the
#format new_pattern() which includes id = "underdress", units = "cm",
#pieces which is a list of the following information pieces(),
#which includes a list containing id and geometry of each piece.
#The pieces are front and back as well as left and right arm.


generate_underdress() <- function(measurements, options = list()) {

  chest  <- measurements$chest #Needed as input in measurements (width of chest)
  length <- measurements$length #Needed as input in measurements (length of garment, shoulder to heel)
  shoulder_width <- measurements$shoulder #Needed as input in measurements (shoulder to shoulder)
  armlength <- measurements$armlength #Needed as input in measurements (shoulder to hand)
  wrist <- measurements$wrist #Needed as input in measurements (circumference of wrist)
  armhole <- measurement$armhole #Needed as input in measurements (shoulder to armhole)

  #hem_drop = (neck_width / 2) * 0.5
  #could be added

  # Bottom is approximately 1.8 times
  # the shoulder width
  bottom_width <- shoulder_width * 1.8

  # Neck dimensions
  neck_width <- measurements$neck_width
  neck_depth <- measurements$neck_depth

  half_shoulder <- shoulder_width / 2
  half_bottom <- bottom_width / 2
  half_neck <- neck_width / 2

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

        # Right side / flare
        c(half_bottom, garment_length),

        # Bottom
        c(0, garment_length),

        c(-half_bottom, garment_length),

        # Back up left side
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

           half_wrist, arm_length,

          -half_wrist, arm_length,

          -half_armhole, 0
        ),
        ncol = 2,
        byrow = TRUE
      )
    )
  )

  piece1 <- new_pattern_piece(
    id = "front",
    geometry = geometry
  )

  piece2 <- new_pattern_piece(
    id = "back",
    geometry = geometry
  )

  piece3 <- new_pattern_piece(
    id = "leftarm",
    geometry = geometry_arm
  )

  piece4 <- new_pattern_piece(
    id = "rightarm",
    geometry = geometry_arm
  )

  new_pattern(
    id = "underdress",
    units = "cm",
    pieces = list(piece1, piece2, piece3, piece4)
  )
}