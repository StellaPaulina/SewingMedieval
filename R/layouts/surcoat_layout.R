#' Generate surcoat layout
#'
#' @param pattern Pattern piece of type "surcoat"
#' @param fabric_width Fabric width dimension
#'
#' @returns Layout object with plotting positions and orientation
#'
#' @export
#' @examples
surcoat_fabric_layout <- function(pattern, fabric_width){
    # find bounds of body
    index_b <- which(sapply(pattern$pieces, function(p) p$id) == "body1")
    body_bbox <- st_bbox(pattern$pieces[[index_b[1]]]$geometry)
    # find the x and y dimensions
    body_bottom  <- body_bbox[["xmax"]] - body_bbox[["xmin"]]
    body_length <- body_bbox[["ymax"]] - body_bbox[["ymin"]]
    # take width of top body piece by doubling x position of first point in geometry
    body_top <- pattern$pieces[[index_b[1]]]$geometry[[1]][[1,1]]*-2

    #side dimensions
    index_s <- which(sapply(pattern$pieces, function(p) p$id) == "side1")
    side_bbox <- st_bbox(pattern$pieces[[index_s[1]]]$geometry)
    # find the x and y dimensions
    side_width  <- side_bbox[["xmax"]] - side_bbox[["xmin"]]
    side_length <- side_bbox[["ymax"]] - side_bbox[["ymin"]]
    
    # now calculate pattern sbbox without collision to determine needed width
    x_b0 <- body_bottom/2 + side_width/2
    y_b0 <- 0
  angle_s0 <- 180
    x_s1_0 <- side_width/2
    y_s1_0 <- side_length
  x_s2_0 <- body_bottom
  y_s2_0 <- side_length
  # test one side for collisopn
    collision_test <- st_overlaps(
        move_geometry(pattern$pieces[[index_b[1]]]$geometry, x_b0 , y_b0), move_geometry(rotate_geometry(pattern$pieces[[index_s[1]]]$geometry,180), x_s1_0, y_s1_0))[[1]]
  move_by <- 0  
  while (length(collision_test) != 0) {
        move_by <- move_by + 2
        collision_test <- st_overlaps(
        move_geometry(pattern$pieces[[index_b[1]]]$geometry, x_b0 + move_by, y_b0), move_geometry(rotate_geometry(pattern$pieces[[index_s[1]]]$geometry,180), x_s1_0, y_s1_0))[[1]]
  }
  x_b0 <- body_bottom/2 + side_width/2 + move_by
    y_b0 <- 0
  angle_s0 <- 180
    x_s1_0 <- side_width/2
    y_s1_0 <- side_length
  x_s2_0 <- body_bottom + side_width/2 + move_by*2
  y_s2_0 <- side_length

  required_pattern_x <- x_s2_0 + side_width/2
  required_pattern_y <- max(body_length, side_length)
    
    # for widest fabric option
    if (fabric_width >= 2* required_pattern_y) {
        # one pair of body ans sides without rotation
        angle_b1 <- 0 # for bottom
        x_b1 <- x_b0
        y_b1 <- y_b0
      angle_s12 <- angle_s0
      x_s1 <- x_s1_0
        y_s1 <- y_s1_0
        x_s2 <- x_s2_0
      y_s2 <- y_s2_0
        # assign fabric fold above pattern
        fabric_x <- 0
        fabric_y <- required_pattern_y
        # add mirroring pieces above fold line
        angle_b2 <- 180 # for bottom
        x_b2 <- x_b1
        y_b2 <- y_b1 + required_pattern_y*2
      
      angle_s34 <- 0
      x_s3 <- x_s1
        y_s3 <- y_s1 + (body_length - side_length)*2
        x_s4 <- x_s2
      y_s4 <- y_s2 + (body_length - side_length)*2
        # assign minimum fabric amount
        minimum_fabric <- required_pattern_x
    }
    # fabric can fit one body length
    else if (fabric_width >= body_length) {
        angle_b1 <- 0 # for bottom
        x_b1 <- x_b0
        y_b1 <- y_b0
      angle_s12 <- angle_s0
      x_s1 <- x_s1_0
        y_s1 <- y_s1_0
        x_s2 <- x_s2_0
      y_s2 <- y_s2_0
        # assign fabric fold above pattern
        fabric_x <- required_pattern_x
        fabric_y <- 0
      
      angle_b2 <- 0 # for bottom
        x_b2 <- x_b1 + required_pattern_x
        y_b2 <- y_b1 
      
      angle_s34 <- 180
      x_s3 <- x_s1 + required_pattern_x
        y_s3 <- y_s1 
        x_s4 <- x_s2 + required_pattern_x
      y_s4 <- y_s2 
        # assign minimum fabric amount
        minimum_fabric <- required_pattern_x * 2
    }
    #use required pattern width to determine rotated layout
    else if (fabric_width >= required_pattern_x) {
        x_b0 <- body_bottom/2 + side_width/2 + move_by
    y_b0 <- 0
  angle_s0 <- 180
    x_s1_0 <- side_width/2
    y_s1_0 <- side_length
  x_s2_0 <- body_bottom + side_width/2 + move_by*2
  y_s2_0 <- side_length  
      
      angle_b1 <- 90 # for bottom
        x_b1 <- 0 + body_length 
        y_b1 <- body_bottom/2 + side_width/2 + move_by
      angle_s12 <- 270
      x_s1 <- 0
        y_s1 <- side_width/2
        x_s2 <- 0
      y_s2 <- side_width/2 + body_bottom + 2*move_by
        # assign fabric fold above pattern
        fabric_x <- required_pattern_y
        fabric_y <- 0
      
      angle_b2 <- 270 # for bottom
        x_b2 <- x_b1 
        y_b2 <- y_b1 
      
      angle_s34 <- 90
      x_s3 <- x_s1 + 2*required_pattern_y
        y_s3 <- y_s1 
        x_s4 <- x_s2 + 2*required_pattern_y
      y_s4 <- y_s2 
        # assign minimum fabric amount
        minimum_fabric <- required_pattern_y * 2
    }
    else {
        stop("Pattern is too large for fabric width")
    }
    layout_result <- list(
        fabric = list(
            width = fabric_width,
            length = minimum_fabric,
            fold_x = fabric_x,
            fold_y = fabric_y
        ),
        placements = list(
            list(
                piece_id = "body1",
                x = x_b1,
                y = y_b1,
                rotation = angle_b1,
                flipped = FALSE
            ),
            list(
                piece_id = "body2",
                x = x_b2,
                y = y_b2,
                rotation = angle_b2,
                flipped = FALSE
            ),
            list(
                piece_id = "side1",
                x = x_s1,
                y = y_s1,
                rotation = angle_s12,
                flipped = FALSE
            ),
            list(
                piece_id = "side2",
                x = x_s2,
                y = y_s2,
                rotation = angle_s12,
                flipped = FALSE
            ),
            list(
                piece_id = "side3",
                x = x_s3,
                y = y_s3,
                rotation = angle_s34,
                flipped = FALSE
            ),
            list(
                piece_id = "side4",
                x = x_s4,
                y = y_s4,
                rotation = angle_s34,
                flipped = FALSE
            )
        )
    )
    return(layout_result)
}