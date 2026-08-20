#' Generate circle skirt layout
#'
#' @param pattern circle skirt pattern object
#' @param fabric_width maximum fabric width
#'
#' @returns circle skirt layout object
#'
#' @export
#' @examples
circleskirt_fabric_layout <- function(pattern, fabric_width){
    # find bounds of pattern piece
    side_bbox <- st_bbox(pattern$pieces[[1]]$geometry)
    # find the x and y dimensions
    dim_x  <- side_bbox["xmax"] - side_bbox["xmin"]
    dim_y <- side_bbox["ymax"] - side_bbox["ymin"]
    double_x <- 2*dim_x[[1]]
    double_y <- 2*dim_y[[1]]
    # dim x should always be larger than y
    if (fabric_width >= double_y) {
        # hourglass configuration, same maximum dimensions as double D
        angle1 <- 0
        angle2 <- 180
        x1 <- dim_x[[1]]/2
        y1 <- 0
        x2 <- dim_x[[1]]/2
        y2 <- fabric_width
        minimum_fabric <- dim_x[[1]]
        fabric_x <- 0
        fabric_y <- fabric_width/2
    }
    else if (fabric_width >= dim_y){
        # two hills configuration
        angle1 <- 0
        angle2 <- 0
        x1 <- dim_x[[1]]/2
        y1 <- 0
        x2 <- dim_x[[1]]/2 + dim_x[[1]]
        y2 <- 0
        minimum_fabric <- double_x
        fabric_x <- double_x/2
        fabric_y <- 0
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
                piece_id = "side1",
                x = x1,
                y = y1,
                rotation = angle1,
                flipped = FALSE
            ),
            list(
                piece_id = "side2",
                x = x2,
                y = y2,
                rotation = angle2,
                flipped = FALSE
            )
        )
    )
    return(layout_result)
}
