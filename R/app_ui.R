# -----------------------------------------------------------------------
# app_ui.R
# Owner: Fatemeh
#
# Only depends on PATTERN_DEFS (constants.R).
# so nothing here needs adjusting for naming choices.
# -----------------------------------------------------------------------

app_ui <- function() {
  fluidPage(

    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Cinzel:wght@500;700&family=Cinzel+Decorative:wght@700&family=EB+Garamond:ital,wght@0,400;0,600;1,400&display=swap"
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    div(
  class = "app-title",

  div(
    class = "title-text",
    h1("Welcome"),
    h3("to Sewing Medieval")
  ),

  tags$img(
    src = "logo.svg",
    class = "app-logo",
    alt = "Sewing Pattern Generator crest"
  )
),

    div(
      class = "history-banner",
      h4("Clothing in medieval Sweden"),
      p(
        "Sweden's medieval period runs roughly from 1050 to 1520 AD. It opens with the ",
        "spread of Christianity through the 11th century and the gradual unification of ",
        "small kingdoms into a single realm, and it closes on the eve of the Vasa era."
      ),
      p(
        "Every garment was cut and hand-sewn to fit one specific wearer - there was no ",
        "ready-made clothing. Wool and linen were the everyday fabrics, silk and fine ",
        "dyed wool were reserved for the wealthy, and fur lined and trimmed cold-weather ",
        "pieces like cloaks. Clothes were expensive to make, so they were worn until they ",
        "wore out, then patched, handed down, or cut up for mending other garments - which ",
        "is part of why fitted, minimal-waste shapes (rectangles, trapezoids, circle ",
        "panels) show up again and again in the patterns in this app."
      )
    ),

    sidebarLayout(
      sidebarPanel(
        selectInput(
          "pattern", "Pattern",
          choices  = PATTERN_CHOICES,
          selected = PATTERN_CHOICES[1]
        ),

        # Dynamic measurement inputs, built per selected pattern
        uiOutput("measurement_inputs"),

        sliderInput(
          "size_scale", "Size adjustment (%)",
          min = 80, max = 120, value = 100, step = 1
        ),

        numericInput(
          "fabric_width", "Fabric width (cm)",
          value = DEFAULT_FABRIC_WIDTH, min = 1, max = 300
        ),

        numericInput(
          "seam_allowance", "Seam allowance (cm)",
          value = DEFAULT_SEAM_ALLOWANCE, min = 0, max = 10, step = 0.5
        ),

        hr(),
        p(class = "live-update-note",
          HTML("&#10022; Pattern updates automatically as you adjust any value.")),
        uiOutput("validation_errors")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Pattern",       plotOutput("pattern_plot", height = "600px")),
          tabPanel("Fabric layout", plotOutput("fabric_layout_plot", height = "600px")),
          tabPanel("Results",       tableOutput("fabric_summary_table"))
        )
      )
    )
  )
}
