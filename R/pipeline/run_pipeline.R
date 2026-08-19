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

#' >>> B: calculate_layout() dispatcher.
#'
#' The planning doc's contract has calculate_layout(pattern, fabric_width)
#' as Person B's public entry point, but Liba's code only defines the
#' per-pattern functions (bag_layout(), and presumably
#' underdress_layout()/circleskirt_layout() once those land). Mirrors
#' the shape of Stella's generate_pattern() dispatcher above. Only
#' "bag" is wired up for now - add the other branches here once their
#' layout files are filled in.
calculate_layout <- function(pattern, fabric_width) {

  if (pattern$id == "bag") {
    return(bag_layout(pattern = pattern, fabric_width = fabric_width))
  }

  stop(sprintf("calculate_layout(): no layout implemented yet for pattern '%s'", pattern$id))
}

run_pipeline <- function(inputs) {

  # Step 1: our own function - no adjustment ever needed here
  validated_inputs <- validate_inputs(inputs)

  # Step 2: >>> A: generate_pattern() is Stella's dispatcher (below).
  pattern <- generate_pattern(validated_inputs)
  pattern <- normalize_pattern_pieces(pattern)

  # Step 3: >>> B: calculate_layout() / bag_layout() is Liba's.
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
    inputs  = validated_inputs,
    pattern = pattern,
    layout  = layout,
    fabric  = fabric_result
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