#' Generate pattern layout for underdress pattern
#'
#' @param pattern underdress pattern object
#'
#' @returns list of positions for plotting underdress pattern layout
#'
#' @export
#' @examples
underdress_pattern_layout <- function(pattern) {
  # find bounds of body
  index_b <- which(sapply(pattern$pieces, function(p) p$id) == "body1")
  body_bbox <- st_bbox(pattern$pieces[[index_b[1]]]$geometry)
  # find the x and y dimensions
  body_bottom <- body_bbox[["xmax"]] - body_bbox[["xmin"]]
  #body_length <- body_bbox[["ymax"]] - body_bbox[["ymin"]]
  # take width of top body piece by doubling x position of first point in geometry
  #body_top <- pattern$pieces[[index_b[1]]]$geometry[[1]][[1, 1]] * -2

  #sleeve dimensions
  index_s <- which(sapply(pattern$pieces, function(p) p$id) == "sleeve1")
  sleeve_bbox <- st_bbox(pattern$pieces[[index_s[1]]]$geometry)
  # find the x and y dimensions
  sleeve_width <- sleeve_bbox[["xmax"]] - sleeve_bbox[["xmin"]]
  #sleeve_length <- sleeve_bbox[["ymax"]] - sleeve_bbox[["ymin"]]

  layout_result <- list(
    placements = list(
      list(
        piece_id = "body1",
        x = -10,
        y = 10,
        rotation = 180,
        flipped = FALSE
      ),
      list(
        piece_id = "body2",
        x = 0,
        y = 0,
        rotation = 180,
        flipped = FALSE
      ),
      list(
        piece_id = "sleeve1",
        x = (-body_bottom / 2) * 0.9,
        y = -sleeve_width / 2,
        rotation = 90,
        flipped = FALSE
      ),
      list(
        piece_id = "sleeve2",
        x = (body_bottom / 2) * 0.9,
        y = -sleeve_width / 2,
        rotation = 270,
        flipped = FALSE
      )
    )
  )
  return(layout_result)
}
