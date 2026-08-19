#' Generate underdress layout
#'
#' @param pattern Pattern piece of type "underdress"
#' @param fabric_width Fabric width dimension
#'
#' @returns Layout object with plotting positions and orientation
#'
#' @export
#' @examples
underdress_fabric_layout <- function(pattern, fabric_width){
    # find bounds of body
    index_b <- which(sapply(pattern$pieces, function(p) p$id) == "body1")
    body_bbox <- st_bbox(pattern$pieces[[index_b[1]]]$geometry)
    # find the x and y dimensions
    body_bottom  <- body_bbox[["xmax"]] - body_bbox[["xmin"]]
    body_length <- body_bbox[["ymax"]] - body_bbox[["ymin"]]
    # take width of top body piece by doubling x position of first point in geometry
    body_top <- pattern$pieces[[index_b[1]]]$geometry[[1]][[1,1]]*-2

    #sleeve dimensions
    index_s <- which(sapply(pattern$pieces, function(p) p$id) == "sleeve1")
    sleeve_bbox <- st_bbox(pattern$pieces[[index_s[1]]]$geometry)
    # find the x and y dimensions
    sleeve_width  <- sleeve_bbox[["xmax"]] - sleeve_bbox[["xmin"]]
    sleeve_length <- sleeve_bbox[["ymax"]] - sleeve_bbox[["ymin"]]
    
    # now calculate pattern sbbox without collision to determine needed width
    x_b0 <- body_bottom/2
    y_b0 <- 0
    x_s0 <- sleeve_width/2 + body_top + (body_bottom - body_top)/2
    y_s0 <- 0
    collision_test <- st_overlaps(
        move_geometry(pattern$pieces[[index_b[1]]]$geometry, x_b0, 0), move_geometry(pattern$pieces[[index_s[1]]]$geometry, x_s0, 0))[[1]]
    while (length(collision_test) != 0) {
        x_s0 <- x_s0 + 2
        collision_test <- st_overlaps(
        move_geometry(pattern$pieces[[index_b[1]]]$geometry, x_b0, 0), move_geometry(pattern$pieces[[index_s[1]]]$geometry, x_s0, 0))[[1]]
    }
    required_pattern_width <- x_s0 + sleeve_width/2
    
    # for widest fabric option
    if (fabric_width >= 2* body_length) {
        # one pair of body ans dleeve without rotation
        angle_1 <- 0 # for bottom
        # move body to origin on left bottom corner
        x_b1 <- x_b0
        y_b1 <- y_b0
        # move sleeve to the potition after top of body pattern
        x_s1 <- x_s0
        y_s1 <- y_s0
        
        # assign fabric fold above pattern
        fabric_x <- 0
        fabric_y <- body_length
        # add mirroring pieces above fold line
        angle_2 <- 180
        x_b2 <- x_b1
        y_b2 <- 2*body_length
        x_s2 <- x_s1
        y_s2 <- 2*body_length
        # assign minimum fabric amount
        minimum_fabric <- x_s1 + sleeve_width/2
    }
    # fabric can fit one body length
    else if (fabric_width >= body_length) {
        # body as is and sleeve next to it
        angle_1 <- 0 # same for both
        angle_2 <- 0
        x_b1 <- x_b0
        y_b1 <- y_b0
        x_s1 <- x_s0
        y_s1 <- y_s0
        # folding fabric after first sleeve
        fabric_x <- x_s1 + sleeve_width/2
        fabric_y <- 0
        x_s2 <- x_s1 + sleeve_width
        y_s2 <- 0
        x_b2 <- x_b1 + fabric_x + (fabric_x - body_bottom)
        y_b2 <- 0
        minimum_fabric <- fabric_x*2
    }
    #use required pattern width to determine rotated layout
    else if (fabric_width >= required_pattern_width) {
        angle_1 <- 90
        x_b1 <- body_length
        y_b1 <- body_bottom/2
        x_s1 <- body_length
        y_s1 <- x_s0
        angle_2 <- 270
        x_b2 <- x_b1
        y_b2 <- y_b1
        x_s2 <- x_s1
        y_s2 <- y_s1
        # add fabric fold
        fabric_x <- x_s1 
        fabric_y <- 0
        # required fabric
        minimum_fabric <- fabric_x * 2
    }
    #if sleeve has to be plotted after body
    else if (fabric_width >= body_bottom){
        angle_1 <- 90
        x_b1 <- body_length
        y_b1 <- body_bottom/2
        x_s1 <- sleeve_length + body_length
        y_s1 <- body_bottom/2
        # fabric fold
        fabric_x <- sleeve_length + body_length
        fabric_y <- 0
        # rotation
        angle_2 <- 270
        x_s2 <- x_s1
        y_s2 <- y_s1
        x_b2 <- x_b1+2*sleeve_length
        y_b2 <- y_b1
        minimum_fabric <- fabric_x * 2
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
                rotation = angle_1,
                flipped = FALSE
            ),
            list(
                piece_id = "body2",
                x = x_b2,
                y = y_b2,
                rotation = angle_2,
                flipped = FALSE
            ),
            list(
                piece_id = "sleeve1",
                x = x_s1,
                y = y_s1,
                rotation = angle_1,
                flipped = FALSE
            ),
            list(
                piece_id = "sleeve2",
                x = x_s2,
                y = y_s2,
                rotation = angle_2,
                flipped = FALSE
            )
        )
    )
    return(layout_result)
}