#' Generate pattern layout for bag pattern
#'
#' @param pattern bag pattern object
#'
#' @returns list of placements of the piece full, which is always at origin
#'
#' @export
#' @examples
bag_pattern_layout <- function(pattern) {
  layout_result <- list(
    placements = list(
      list(
        piece_id = "full",
        x = 0,
        y = 0,
        rotation = 0,
        flipped = FALSE
      )
    )
  )
  return (layout_result)
}
