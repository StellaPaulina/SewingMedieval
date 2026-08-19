# -----------------------------------------------------------------------
# constants.R
# Owner: Person C (Fatemeh)
#
# Single source of truth for what each pattern needs. The Shiny UI,
# validate_inputs(), and the plotting functions all read from this list
# instead of hard-coding field names in three different places.
# -----------------------------------------------------------------------

PATTERN_DEFS <- list(

  bag = list(
    label = "Bag (drawstring pouch)",
    measurements = list(
      width  = list(label = "Width (cm)",  min = 1, max = 200, default = 18),
      height = list(label = "Height (cm)", min = 1, max = 200, default = 40)
    ),
    # Display-only metadata for pieces (Stella's Pattern object doesn't
    # yet carry piece$metadata$name / quantity). Keyed by piece id.
    pieces = list(
      full = list(name = "Full (cut on fold)", quantity = 1)
    )
  ),

  underdress = list(
    label = "Underdress / Undershirt",
    measurements = list(
      wrist             = list(label = "Wrist (cm)",             min = 1, max = 60,  default = 16),
      armhole           = list(label = "Armhole (cm)",           min = 1, max = 80,  default = 42),
      neck_shoulder     = list(label = "Neck to Shoulder (cm)",  min = 1, max = 40,  default = 12),
      shoulder_shoulder = list(label = "Shoulder to Shoulder (cm)", min = 1, max = 80, default = 40),
      length            = list(label = "Length (cm)",            min = 1, max = 200, default = 100),
      bust              = list(label = "Bust (cm)",               min = 1, max = 200, default = 90),
      # Stella's generate_underdress() also reads measurements$armlength
      # (shoulder to wrist) to build the sleeve piece, but it was left
      # out of the agreed measurement list - added here so the UI
      # collects it and validate_inputs() lets it through.
      armlength         = list(label = "Arm length, shoulder to wrist (cm)", min = 1, max = 100, default = 58)
    ),
    pieces = list(
      body   = list(name = "Body",   quantity = 2),
      sleeve = list(name = "Sleeve", quantity = 2)
    )
  ),
  
  circleskirt = list(
    label = "Full circle skirt",
    measurements = list(
      waist  = list(label = "Waist (cm)",  min = 1, max = 200, default = 72),
      length = list(label = "Length (cm)", min = 1, max = 200, default = 60)
    ),
    pieces = list(
      side = list(name = "Side (C.Back/Side + C.Front/Side)", quantity = 2)
    )
  ),

  surcoat = list(
    label = "Surcoat / Sideless surcote (pellote)",
    measurements = list(
      bust              = list(label = "Bust (cm)",                  min = 1, max = 200, default = 90),
      length            = list(label = "Length (cm)",                min = 1, max = 200, default = 100),
      shoulder_shoulder = list(label = "Shoulder to Shoulder (cm)",  min = 1, max = 80,  default = 40),
      neck_shoulder     = list(label = "Neck to Shoulder (cm)",      min = 1, max = 40,  default = 12),
      armhole           = list(label = "Shoulder to Armhole (cm)",   min = 1, max = 80,  default = 42)
    ),
    pieces = list(
      body = list(name = "Body (front/back)", quantity = 2),
      side = list(name = "Side panel",        quantity = 4)
    )
  )
)

PATTERN_CHOICES <- setNames(
  names(PATTERN_DEFS),
  vapply(PATTERN_DEFS, function(p) p$label, character(1))
)

DEFAULT_FABRIC_WIDTH <- 140

#' Look up display name/quantity for a pattern piece from PATTERN_DEFS.
#'
#' Stella's pieces only carry $id and $geometry (no $metadata/$quantity
#' yet), and multi-instance piece ids are suffixed with a number (e.g.
#' "body1", "body2", "sleeve1"), while PATTERN_DEFS$pieces is keyed by
#' the un-suffixed base id ("body", "sleeve"). This bridges the two so
#' the plots can show "Body x2" instead of the raw id "body1".
#'
#' @param pattern_id character, e.g. "underdress"
#' @param piece_id character, e.g. "body1"
#' @return list(name, quantity)
piece_display_info <- function(pattern_id, piece_id) {
  base_id <- sub("[0-9]+$", "", piece_id)
  spec <- PATTERN_DEFS[[pattern_id]]$pieces[[base_id]]
  list(
    name     = if (!is.null(spec)) spec$name else piece_id,
    quantity = if (!is.null(spec)) spec$quantity else 1
  )
}