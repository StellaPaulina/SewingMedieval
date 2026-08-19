#' Generate pattern layout for surcoat
#'
#' @param pattern surcoat pattern object
#'
#' @returns list of positions for pattern pieces, do not plot last piece
#'
#' @export
#' @examples
surcoat_pattern_layout <- function(pattern) {
  # find bounds of body
  index_b <- which(sapply(pattern$pieces, function(p) p$id) == "body1")
  body_bbox <- st_bbox(pattern$pieces[[index_b[1]]]$geometry)
  # find the x and y dimensions
  body_bottom <- body_bbox[["xmax"]] - body_bbox[["xmin"]]
  body_length <- body_bbox[["ymax"]] - body_bbox[["ymin"]]
  # take width of top body piece by doubling x position of first point in geometry
  #body_top <- pattern$pieces[[index_b[1]]]$geometry[[1]][[1, 1]] * -2

  #sleeve dimensions
  index_s <- which(sapply(pattern$pieces, function(p) p$id) == "side1")
  side_bbox <- st_bbox(pattern$pieces[[index_s[1]]]$geometry)
  # find the x and y dimensions
  side_width <- side_bbox[["xmax"]] - side_bbox[["xmin"]]
  side_length <- side_bbox[["ymax"]] - side_bbox[["ymin"]]

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
        piece_id = "side1",
        x = -(body_bottom / 2) - 10,
        y = -(body_length - side_length) + 10,
        rotation = 165,
        flipped = FALSE
      ),
      list(
        piece_id = "side2",
        x = -(body_bottom / 2),
        y = -(body_length - side_length),
        rotation = 165,
        flipped = FALSE
      ),
      list(
        piece_id = "side3",
        x = (body_bottom / 2) -10,
        y = -(body_length - side_length) +10,
        rotation = 195,
        flipped = FALSE
      ),
      list(
        piece_id = "side4",
        x = (body_bottom / 2) ,
        y = -(body_length - side_length),
        rotation = 195,
        flipped = FALSE
      )
    )
  )
  return(layout_result)
}
