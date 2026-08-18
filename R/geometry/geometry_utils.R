#' Rotate geometry
#'
#' @param geometry is an sf geometry object
#' @param angle is the desired angle for rotation
#'
#' @returns an sf geometry object with modified coordinates after rotation
#'
#' @export
#' @examples
rotate_geometry <- function(geometry, angle) {
  
  coords <- st_coordinates(geometry)
  
  theta <- angle * pi / 180
  
  x_new <- coords[, 1] * cos(theta) -
           coords[, 2] * sin(theta)
  
  y_new <- coords[, 1] * sin(theta) +
           coords[, 2] * cos(theta)
  
  st_polygon(list(
    cbind(x_new, y_new)
  ))
}

#' Move geometry
#'
#' @param geometry Is an sf geometry object
#' @param dx is the x axis movement
#' @param dy is the y axis movement
#'
#' @returns geometry with new coordinates after performed movement
#'
#' @export
#' @examples
move_geometry <- function(geometry, dx, dy) {
  
  coords <- st_coordinates(geometry)
  
  coords[, 1] <- coords[, 1] + dx
  coords[, 2] <- coords[, 2] + dy
  
  st_polygon(list(coords[, 1:2]))
}
