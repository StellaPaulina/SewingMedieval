#' Calculate bag layout
#'
#' @param pattern Pattern object of type "bag"
#' @param fabric_width Desired width of fabric
#'
#' @returns Layout object
#'
#' @export
#' @examples
bag_layout <- function(pattern, fabric_width){
    # find bounds of pattern piece
    full_bbox <- st_bbox(pattern$pieces[[1]]$geometry)
    # find the x and y dimensions
    dim_x  <- full_bbox["xmax"] - full_bbox["xmin"]
    dim_y <- full_bbox["ymax"] - full_bbox["ymin"]
    if (min(dim_x, dim_y) > fabric_width) { #there is no way to rotate
        stop("Pattern is too large for fabric width")
    }
    if (dim_x > dim_y & dim_y <= fabric_width) {
        # rectangle must be rotated to fit fabric length
        angle <- 90
        # rectangle must be moved to fit properly to origin after rotation
        x <- dim_y[[1]]
        minimum_fabric <- dim_y[[1]]
    }
    # optionally could add in a rotation to further minimize length for dim_x > dim_y
    #else if (dim_x > dim_y ) {
    #    # trigonometry to determin rectangle rotation
    #    d = sqrt(dim_x^2 + dim_y^2)
    #    triangle_angle <- asin(fabric_width/d) * 180 / pi
    #    h_angle <- asin(dim_y/d) * 180 / pi
    #    # set angle
    #    angle <- triangle_angle - h_angle
    #    #check that angle does produce shorter fabric length than no rotation
    #}
    else {
        angle <- 0
        x = 0
        minimum_fabric <- dim_x[[1]]
    }
    layout_result <- list(
        fabric = list(
            width = fabric_width,
            length = minimum_fabric,
            fold_x = 0,
            fold_y = 0
        ),
        placements = list(
            list(
                piece_id = "full",
                x = x,
                y = 0,
                rotation = angle,
                flipped = FALSE
            )
        )
    )
    return(layout_result)
}
