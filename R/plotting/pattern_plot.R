# -----------------------------------------------------------------------
# pattern_plot.R
# Owner: Person C (Fatemeh)
#
# Contract (per Planning session, section 11):
#   plot_pattern(pattern, pattern_layout) -> ggplot object
#
# Reads pattern$pieces[[i]]$geometry (an sf geometry, per Contract A -> B)
# plus $id. pattern_layout is Liba's "as it should look" assembled
# arrangement (see calculate_pattern_layout() in run_pipeline.R) -
# without it, every piece would render in its own local (0,0)-based
# coordinates and stack on top of each other (e.g. underdress's two
# identical body pieces). Nothing here needs to change based on what
# Person A/B name their internal functions - it only depends on the
# *shape* of the Pattern/Layout objects they hand back (and on pieces
# being a named list, which run_pipeline.R guarantees via
# normalize_pattern_pieces()).
# -----------------------------------------------------------------------

library(ggplot2)
library(sf)

# small null-coalescing helper, kept local to this file
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Find the placement for a given piece id in a Layout object's
#' $placements list (a plain list, not named by piece_id).
.find_placement <- function(placements, piece_id) {
  if (is.null(placements)) return(NULL)
  for (pl in placements) {
    if (identical(pl$piece_id, piece_id)) return(pl)
  }
  NULL
}

#' Apply a placement's flip/rotate/translate to a single (x, y) point.
#' Mirrors the geometry transform below, so grainlines and pieces stay
#' in sync when a pattern_layout is supplied.
.transform_xy <- function(x, y, placement) {
  if (is.null(placement)) return(c(x, y))
  if (isTRUE(placement$flipped)) x <- -x
  theta <- (placement$rotation %||% 0) * pi / 180
  x2 <- x * cos(theta) - y * sin(theta)
  y2 <- x * sin(theta) + y * cos(theta)
  c(x2 + (placement$x %||% 0), y2 + (placement$y %||% 0))
}

#' Plot every piece of a pattern, arranged the way it should look
#' (per Liba's pattern_layout), in real-world units.
#'
#' @param pattern Pattern object (contract: id, units, pieces)
#' @param pattern_layout Layout object from calculate_pattern_layout()
#'   (placements only, no $fabric). Optional - if omitted, pieces are
#'   drawn in their raw, unpositioned geometry.
#' @return a ggplot object
plot_pattern <- function(pattern, pattern_layout = NULL) {

  if (is.null(pattern) || length(pattern$pieces) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No pattern to display") +
        theme_void()
    )
  }

  placements <- pattern_layout$placements

  pieces_sf <- do.call(rbind, lapply(names(pattern$pieces), function(pid) {
    piece     <- pattern$pieces[[pid]]
    placement <- .find_placement(placements, pid)
    geom      <- piece$geometry

    if (!is.null(placement)) {
      # >>> A: rotate_geometry()/move_geometry() from geometry_utils.R -
      # same transform order (flip -> rotate -> translate) as
      # plot_fabric_layout(), so the two views agree.
      if (isTRUE(placement$flipped)) {
        geom <- geom * matrix(c(-1, 0, 0, 1), 2, 2)
      }
      geom <- rotate_geometry(geom, placement$rotation %||% 0)
      geom <- move_geometry(geom, placement$x %||% 0, placement$y %||% 0)
    }

    info <- piece_display_info(pattern$id, pid)
    sf::st_sf(
      piece_id = pid,
      name     = info$name,
      quantity = info$quantity,
      geometry = sf::st_sfc(geom)
    )
  }))

  grainlines <- do.call(rbind, lapply(names(pattern$pieces), function(pid) {
    g <- pattern$pieces[[pid]]$grainline
    if (is.null(g)) return(NULL)
    placement <- .find_placement(placements, pid)
    p1 <- .transform_xy(g$x1, g$y1, placement)
    p2 <- .transform_xy(g$x2, g$y2, placement)
    data.frame(piece_id = pid, x = c(p1[1], p2[1]), y = c(p1[2], p2[2]))
  }))

  p <- ggplot() +
    geom_sf(data = pieces_sf, aes(fill = piece_id), alpha = 0.3, color = "black") +
    geom_sf_text(data = pieces_sf, aes(label = paste0(name, " x", quantity)),
                 size = 3, fun.geometry = sf::st_centroid) +
    labs(
      title = paste("Pattern:", pattern$id %||% ""),
      x = paste0("cm (", pattern$units %||% "cm", ")"),
      y = NULL
    ) +
    theme_minimal() +
    theme(legend.position = "none")

  if (!is.null(grainlines)) {
    p <- p + geom_line(
      data = grainlines,
      aes(x = x, y = y, group = piece_id),
      arrow = grid::arrow(length = grid::unit(0.15, "cm")),
      linewidth = 0.4
    )
  }

  p
}