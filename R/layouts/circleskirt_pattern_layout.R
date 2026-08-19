#' Generate circleskirt pattern layout
#'
#' @param pattern circleskirt pattern object
#'
#' @returns list of placements of circleskirt pattern pieces
#'
#' @export
#' @examples
circleskirt_pattern_layout <- function(pattern) {
  layout_result <- list(
    placements = list(
      list(
        piece_id = "side1",
        x = 0,
        y = 0,
        rotation = 0,
        flipped = FALSE
      ),
      list(
        piece_id = "side2",
        x = 0,
        y = 0,
        rotation = 180,
        flipped = FALSE
      )
    )
  )
  return(layout_result)
}
