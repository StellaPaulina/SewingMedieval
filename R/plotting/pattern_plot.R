# -----------------------------------------------------------------------
# pattern_plot.R
# Owner: Person C (Fatemeh)
#
# Contract (per Planning session, section 11):
#   plot_pattern(pattern) -> ggplot object
#
# Reads pattern$pieces[[i]]$geometry (an sf geometry, per Contract A -> B)
# plus $id and $grainline. Nothing here needs to change based on what
# Person A names their internal functions - it only depends on the
# *shape* of the Pattern object they hand back (and on pieces being a
# named list, which run_pipeline.R now guarantees via
# normalize_pattern_pieces()).
# -----------------------------------------------------------------------

library(ggplot2)
library(sf)

#' Plot every piece of a pattern on its own, in real-world units
#'
#' @param pattern Pattern object (contract: id, units, pieces)
#' @return a ggplot object
plot_pattern <- function(pattern) {

  if (is.null(pattern) || length(pattern$pieces) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No pattern to display") +
        theme_void()
    )
  }

  pieces_sf <- do.call(rbind, lapply(names(pattern$pieces), function(pid) {
    piece <- pattern$pieces[[pid]]
    sf::st_sf(
      piece_id = pid,
      name     = piece$metadata$name %||% pid,
      quantity = piece$quantity %||% 1,
      geometry = sf::st_sfc(piece$geometry)
    )
  }))

  grainlines <- do.call(rbind, lapply(names(pattern$pieces), function(pid) {
    g <- pattern$pieces[[pid]]$grainline
    if (is.null(g)) return(NULL)
    data.frame(piece_id = pid, x = c(g$x1, g$x2), y = c(g$y1, g$y2))
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

# small null-coalescing helper, kept local to this file
`%||%` <- function(a, b) if (is.null(a)) b else a
