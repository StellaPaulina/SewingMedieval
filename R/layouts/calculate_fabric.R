#' Calculate required fabric length
#'
#' @param layout A layout object
#' @param margin Desired margin for fabric recommendation
#'
#' @returns a fabric size object
#'
#' @export
#' @examples
calculate_fabric <- function(layout, margin = 10) {
  if (is.null(layout)) {
    stop("Needs a valid layout object")
  }
  if (is.null(layout$fabric$length)) {
    stop("Fabric length in layout object is missing")
  }
  required_length <- layout$fabric$length
  fabric_result <- list(
    required_length = required_length,
    recommended_length = required_length + margin,
    fabric_width = layout$fabric$width,
    margin = margin,
    units = "cm"
  )
  return(fabric_result)
}
