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
        # BUGFIX (Fatemeh): whichever axis stays un-rotated becomes the
        # fabric LENGTH (the piece's y-extent after any rotation) - this
        # was swapped, assigning the *width* dimension as if it were the
        # required length, so the returned fabric rectangle didn't
        # actually contain the placed piece. Confirmed against
        # tests/testthat/test-layouts.R and test-pipeline.R, which
        # already encoded the correct expected values (18x80 -> no
        # rotation, length 80; 100x20 -> rotate 90, length 100).
        if (index_min == 1) {
            angle <- 0
            x <- 0
            y <- 0
            minimum_fabric <- dim_y
        }
        else {
            angle <- 90
            x <- dim_y
            y <- 0
            minimum_fabric <- dim_x
        }
    }
    else if (min(dim_x, dim_y) > fabric_width) { #there is no way to rotate
        stop("Pattern is too large for fabric width")
    }
    else {
        # Only one orientation fits fabric_width at all - same fix as
        # above, plus the angle choice itself was inverted here (it was
        # rotating exactly when rotation wasn't needed, and vice versa),
        # which would have placed a piece wider than the fabric.
        index_min <- which(c(dim_x, dim_y) == min(c(dim_x, dim_y)))
        if (index_min == 1) {
            angle <- 0
            x <- 0
            y <- 0
            minimum_fabric <- dim_y
        }
        else {
            angle <- 90
            x <- dim_y
            y <- 0
            minimum_fabric <- dim_x
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