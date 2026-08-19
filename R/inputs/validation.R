# -----------------------------------------------------------------------
# validation.R
# Owner: Fatemeh
#
# Contract (per Planning session, section 3):
#   validate_inputs(inputs) -> validated_inputs
#
#   If validate_inputs() succeeds, Person A may assume the measurements
#   are valid and in the agreed units (centimetres).
#
# This is OUR function - nothing here needs to change when A/B rename
# their own functions. It only needs to change if the *shape* of
# `inputs` changes, or when PATTERN_DEFS grows past "bag".
# -----------------------------------------------------------------------

#' Validate raw Shiny inputs into the standardized inputs object
#'
#' @param inputs list with pattern, measurements, fabric, options
#' @return validated_inputs (same shape as inputs) or throws an error
#'   with a user-readable message via stop()
validate_inputs <- function(inputs) {

  errors <- character(0)

  # --- pattern ------------------------------------------------------
  pattern <- inputs$pattern
  if (is.null(pattern) || !(pattern %in% names(PATTERN_DEFS))) {
    stop(sprintf(
      "Unknown pattern '%s'. Must be one of: %s",
      as.character(pattern), paste(names(PATTERN_DEFS), collapse = ", ")
    ))
  }
  pattern_def <- PATTERN_DEFS[[pattern]]

  # --- measurements ---------------------------------------------------
  measurements <- inputs$measurements
  for (field in names(pattern_def$measurements)) {
    spec  <- pattern_def$measurements[[field]]
    value <- measurements[[field]]

    if (is.null(value) || is.na(value)) {
      errors <- c(errors, sprintf("%s is required.", spec$label))
      next
    }
    if (!is.numeric(value)) {
      errors <- c(errors, sprintf("%s must be numeric.", spec$label))
      next
    }
    if (value <= 0) {
      errors <- c(errors, sprintf("%s must be > 0.", spec$label))
    }
    if (!is.null(spec$max) && value > spec$max) {
      errors <- c(errors, sprintf("%s looks too large (max %s cm).", spec$label, spec$max))
    }
  }

  # --- fabric ---------------------------------------------------------
  fabric_width <- inputs$fabric$width
  if (is.null(fabric_width) || is.na(fabric_width) || !is.numeric(fabric_width)) {
    errors <- c(errors, "Fabric width is required and must be numeric.")
  } else if (fabric_width <= 0) {
    errors <- c(errors, "Fabric width must be > 0.")
  }

  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"))
  }

  # --- build the standardized object -----------------------------------
  # `options` was being dropped here even though generate_pattern() (via
  # run_pipeline()) reads validated_inputs$options - pass it through
  # (defaulting to an empty list) in case it's needed later. Currently
  # always empty: seam allowance was removed from the app entirely (not
  # something we're accounting for).
  list(
    pattern      = pattern,
    measurements = measurements[names(pattern_def$measurements)],
    fabric       = list(width = fabric_width),
    options      = if (is.null(inputs$options)) list() else inputs$options
  )
}