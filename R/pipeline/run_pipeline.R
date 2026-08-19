# -----------------------------------------------------------------------
# run_pipeline.R
# Owner: Person C (Fatemeh) / integration
#
# Contract (per Planning session, section 10):
#   run_pipeline(inputs) -> result
#   result$inputs, result$pattern, result$layout, result$fabric
# -----------------------------------------------------------------------

#' Make sure pattern$pieces is a named list keyed by each piece's own
#' $id, e.g. pieces$full, pieces$body, ...
#'
#' Stella's generate_bag()/new_pattern() currently returns an UNNAMED
#' pieces list (pieces = list(piece)), but Liba's bag_layout.R (and our
#' own plotting code) index pieces by name (pattern$pieces$full). This
#' normalizes the shape defensively on our side of the boundary so we
#' don't have to touch patterns/ or geometry/ - flag it to Stella so
#' new_pattern() can be fixed at the source too.
normalize_pattern_pieces <- function(pattern) {
  if (is.null(pattern$pieces) || length(pattern$pieces) == 0) {
    return(pattern)
  }
  ids <- vapply(pattern$pieces, function(p) p$id, character(1))
  pattern$pieces <- setNames(pattern$pieces, ids)
  pattern
}

#' >>> B: calculate_layout() dispatcher - the fabric-cutting layout.
#'
#' The planning doc's contract has calculate_layout(pattern, fabric_width)
#' as Person B's public entry point. Liba's actual functions are named
#' <pattern>_fabric_layout() (bag_fabric_layout(), underdress_fabric_layout(),
#' circleskirt_fabric_layout()) - all three exist and pick whichever
#' rotation minimizes required fabric length, so all three are wired
#' up here. Mirrors the shape of Stella's generate_pattern() dispatcher
#' above.
calculate_layout <- function(pattern, fabric_width) {

  if (pattern$id == "bag") {
    return(bag_fabric_layout(pattern = pattern, fabric_width = fabric_width))
  }

  if (pattern$id == "underdress") {
    return(underdress_fabric_layout(pattern = pattern, fabric_width = fabric_width))
  }

  if (pattern$id == "circleskirt") {
    return(circleskirt_fabric_layout(pattern = pattern, fabric_width = fabric_width))
  }

  stop(sprintf("calculate_layout(): no layout implemented yet for pattern '%s'", pattern$id))
}

#' >>> B: calculate_pattern_layout() dispatcher - the "as it should
#' look" assembled view, for the Pattern tab.
#'
#' Liba also wrote <pattern>_pattern_layout() functions
#' (bag_pattern_layout(), underdress_pattern_layout(),
#' circleskirt_pattern_layout()) that place every piece so the pattern
#' displays sensibly (no fabric-width constraint, no length
#' minimization - just a readable arrangement). These were not wired
#' into the pipeline before, so plot_pattern() was drawing every piece
#' in its own local (0,0)-based coordinates, stacking pieces like
#' underdress's two identical body pieces directly on top of each
#' other. This dispatcher feeds plot_pattern() the same kind of Layout
#' object (placements only, no $fabric) that calculate_layout() feeds
#' plot_fabric_layout().
calculate_pattern_layout <- function(pattern) {

  if (pattern$id == "bag") {
    return(bag_pattern_layout(pattern = pattern))
  }

  if (pattern$id == "underdress") {
    return(underdress_pattern_layout(pattern = pattern))
  }

  if (pattern$id == "circleskirt") {
    return(circleskirt_pattern_layout(pattern = pattern))
  }

  stop(sprintf("calculate_pattern_layout(): no pattern layout implemented yet for pattern '%s'", pattern$id))
}

run_pipeline <- function(inputs) {

  # Step 1: our own function - no adjustment ever needed here
  validated_inputs <- validate_inputs(inputs)

  # Step 2: >>> A: generate_pattern() is Stella's dispatcher (below).
  pattern <- generate_pattern(validated_inputs)
  pattern <- normalize_pattern_pieces(pattern)

  # Step 3a: >>> B: calculate_pattern_layout() / *_pattern_layout() is
  # Liba's - the assembled "how the pattern should look" arrangement,
  # displayed on the Pattern tab.
  pattern_layout <- calculate_pattern_layout(pattern = pattern)

  # Step 3b: >>> B: calculate_layout() / *_fabric_layout() is Liba's -
  # the fabric-cutting layout (rotation chosen to minimize required
  # fabric length), displayed on the Fabric layout tab.
  layout <- calculate_layout(
    pattern      = pattern,
    fabric_width = validated_inputs$fabric$width
  )

  # Step 4: >>> B: calculate_fabric() is Liba's.
  fabric_result <- calculate_fabric(
    layout = layout,
    margin = 10
  )

  list(
    inputs         = validated_inputs,
    pattern        = pattern,
    pattern_layout = pattern_layout,
    layout         = layout,
    fabric         = fabric_result
  )
}

#' Safe wrapper for use inside Shiny - never throws, always returns a
#' list with either $result or $error so app_server.R can branch cleanly.
run_pipeline_safe <- function(inputs) {
  tryCatch(
    list(result = run_pipeline(inputs), error = NULL),
    error = function(e) list(result = NULL, error = conditionMessage(e))
  )
}

generate_pattern <- function(inputs) {

  # inputs already validated (hopefully)

  if (inputs$pattern == "bag") {
    return(generate_bag(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  if (inputs$pattern == "underdress") {
    return(generate_underdress(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  if (inputs$pattern == "circleskirt") {
    return(generate_circleskirt(
      measurements = inputs$measurements,
      options = inputs$options
    ))
  }

  stop("Unknown pattern")
}