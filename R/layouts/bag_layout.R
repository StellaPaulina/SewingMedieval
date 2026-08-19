#' Calculate bag layout
#'
#' @param pattern Pattern object of type "bag"
#' @param fabric_width Desired width of fabric
#'
#' @returns Layout object
#'
#' @export
#' @examples
bag_fabric_layout <- function(pattern, fabric_width){
    if (is.null(pattern)) {
        stop("Pattern is required")
    }
    if (pattern$id != "bag") {
        stop("Invalid pattern type")
    }
    if (is.null(pattern$pieces[[1]]$id)){
        stop("Bag pattern piece full is required")
    }
    # find bounds of pattern piece
    full_bbox <- st_bbox(pattern$pieces[[1]]$geometry)
    # find the x and y dimensions
    dim_x  <- full_bbox["xmax"][[1]] - full_bbox["xmin"][[1]]
    dim_y <- full_bbox["ymax"][[1]] - full_bbox["ymin"][[1]]
    
    # if all orientations fit the fabric length
    if (max(dim_x, dim_y) <= fabric_width) {
        index_min <- which(c(dim_x, dim_y) == min(c(dim_x, dim_y)))
        # plot with smaller dimension as the length of fabric
        if (index_min == 1) {
            angle <- 0
            x <- 0
            y <- 0
            minimum_fabric <- dim_x
        }
        else {
            angle <- 90
            x <- dim_y
            y <- 0
            minimum_fabric <- dim_y
        }
    }
    else if (min(dim_x, dim_y) > fabric_width) { #there is no way to rotate
        stop("Pattern is too large for fabric width")
    }
    else {
        index_min <- which(c(dim_x, dim_y) == min(c(dim_x, dim_y)))
        if (index_min == 2) {
            angle <- 0
            x <- 0
            y <- 0
            minimum_fabric <- dim_x
        }
        else {
            angle <- 90
            x <- dim_y
            y <- 0
            minimum_fabric <- dim_y
        }
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
                y = y,
                rotation = angle,
                flipped = FALSE
            )
        )
    )
    return(layout_result)
}
