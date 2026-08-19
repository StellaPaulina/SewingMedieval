# -----------------------------------------------------------------------
# fabric_layout_plot.R
# Owner: Fatemeh
#
# Contract (per Planning session, section 11):
#   plot_fabric_layout(pattern, layout) -> ggplot object
#
#   pattern tells it what each piece looks like (geometry),
#   layout   tells it where each piece goes (x, y, rotation, flipped).
#
# Depends only on the agreed Pattern / Layout object shapes, not on
# what A/B call their internal functions. Rotation/translation reuse
# Stella's own rotate_geometry()/move_geometry() from
# geometry/geometry_utils.R instead of re-implementing the matrix math
# here, so there is exactly one place in the codebase that knows how a
# piece gets rotated.
# -----------------------------------------------------------------------

library(ggplot2)
library(sf)

#' Plot pieces placed on the fabric, per the layout returned by
#' Person B's calculate_layout().
#'
#' @param pattern Pattern object (for piece geometry)
#' @param layout  Layout object (for placements + fabric dims)
#' @return a ggplot object
plot_fabric_layout <- function(pattern, layout) {

  if (is.null(layout) || length(layout$placements) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No layout to display") +
        theme_void()
    )
  }

  # NOTE: Liba's placements put the length axis along x and the fabric
  # width along y (fabric_width is the knob the user turns, and it maps
  # to the y-axis limit; the length required to cut everything comes
  # out along x) - the opposite of what section 14 of the planning doc
  # describes, but that's the convention her layout functions actually
  # use and we're not changing layouts/, only how this plot draws the
  # boundary box to match it.
  fabric_rect <- data.frame(
    xmin = 0, xmax = layout$fabric$length,
    ymin = 0, ymax = layout$fabric$width
  )

  # Fold line - Liba's layout functions already return fold_x/fold_y
  # (the point the fold passes through). A non-zero fold_x means a
  # vertical fold (spans the fabric width); a non-zero fold_y means a
  # horizontal fold (spans the required length). Both 0 (e.g. the bag,
  # a single piece with no fold in this layout) means no fold to draw.
  fold_x <- layout$fabric$fold_x %||% 0
  fold_y <- layout$fabric$fold_y %||% 0

  fold_line <- NULL
  if (!isTRUE(all.equal(fold_x, 0))) {
    fold_line <- data.frame(x = fold_x, xend = fold_x, y = 0, yend = layout$fabric$width)
  } else if (!isTRUE(all.equal(fold_y, 0))) {
    fold_line <- data.frame(x = 0, xend = layout$fabric$length, y = fold_y, yend = fold_y)
  }

  placed_sf <- do.call(rbind, lapply(layout$placements, function(placement) {
    piece <- pattern$pieces[[placement$piece_id]]
    if (is.null(piece)) return(NULL)

    geom <- piece$geometry

    # Mirror first if flipped - no shared utility for this yet, so it
    # stays local here (there's nothing to route through A's file).
    if (!is.null(placement$flipped) && isTRUE(placement$flipped)) {
      geom <- geom * matrix(c(-1, 0, 0, 1), 2, 2)
    }

    # >>> A: rotate_geometry()/move_geometry() from geometry_utils.R -
    # reuse Stella's utilities rather than duplicating the rotation
    # math here. Her rotation is in degrees, same as Liba's placements.
    geom <- rotate_geometry(geom, placement$rotation %||% 0)
    geom <- move_geometry(geom, placement$x %||% 0, placement$y %||% 0)

    # >>> C: piece_display_info() (constants.R) looks up the display
    # name from PATTERN_DEFS$pieces since Stella's pieces don't carry
    # $metadata yet - same helper plot_pattern() uses, so labels agree
    # between the Pattern and Fabric layout tabs.
    info <- piece_display_info(pattern$id, placement$piece_id)
    sf::st_sf(
      piece_id = placement$piece_id,
      name     = info$name,
      geometry = sf::st_sfc(geom)
    )
  }))

  p <- ggplot() +
    geom_rect(
      data = fabric_rect,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA, color = "grey40", linewidth = 0.6
    )

  if (!is.null(fold_line)) {
    p <- p +
      geom_segment(
        data = fold_line,
        aes(x = x, xend = xend, y = y, yend = yend),
        color = "#DF301C", linetype = "dashed", linewidth = 0.8
      )
  }

  p +
    geom_sf(data = placed_sf, aes(fill = piece_id), alpha = 0.4, color = "black") +
    geom_sf_text(data = placed_sf, aes(label = name), size = 3,
                 fun.geometry = sf::st_centroid) +
    coord_sf(expand = FALSE) +
    labs(
      title = "Fabric layout",
      subtitle = sprintf(
        "Fabric width %.0f cm x required length %.0f cm",
        layout$fabric$width, layout$fabric$length
      ),
      x = "Required length (cm)", y = "Fabric width (cm)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

`%||%` <- function(a, b) if (is.null(a)) b else a