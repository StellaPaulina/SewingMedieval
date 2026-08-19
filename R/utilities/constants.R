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
  )

  underdress = list(
    label = "Underdress / Undershirt",
    measurements = list(
      wrist             = list(label = "Wrist (cm)",             min = 1, max = 60,  default = 16),
      armhole           = list(label = "Armhole (cm)",           min = 1, max = 80,  default = 42),
      neck_shoulder     = list(label = "Neck to Shoulder (cm)",  min = 1, max = 40,  default = 12),
      shoulder_shoulder = list(label = "Shoulder to Shoulder (cm)", min = 1, max = 80, default = 40),
      length            = list(label = "Length (cm)",            min = 1, max = 200, default = 100),
      bust              = list(label = "Bust (cm)",               min = 1, max = 200, default = 90)
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
  )
)

PATTERN_CHOICES <- setNames(
  names(PATTERN_DEFS),
  vapply(PATTERN_DEFS, function(p) p$label, character(1))
)

DEFAULT_FABRIC_WIDTH <- 140
