# -----------------------------------------------------------------------
# app_server.R
# Owner: Fatemeh
#
# Server-side logic for the Medieval sewing pattern generator.
# -----------------------------------------------------------------------

app_server <- function(input, output, session) {


  # ---------------------------------------------------------------------
  # Helper functions for measurement input IDs
  # ---------------------------------------------------------------------

  num_id <- function(field) {
    paste0("meas_num_", field)
  }

  slider_id <- function(field) {
    paste0("meas_slider_", field)
  }


  # ---------------------------------------------------------------------
  # Holds active numeric <-> slider observers
  # ---------------------------------------------------------------------

  field_sync <- reactiveValues(
    observers = list()
  )


  # ---------------------------------------------------------------------
  # DYNAMIC MEASUREMENT INPUTS
  # ---------------------------------------------------------------------

  output$measurement_inputs <- renderUI({

    req(input$pattern)

    fields <- PATTERN_DEFS[[input$pattern]]$measurements

    tagList(
      lapply(
        names(fields),
        function(field) {

          spec <- fields[[field]]

          tagList(

            tags$label(
              class = "control-label",
              spec$label
            ),

            fluidRow(

              column(
                4,

                numericInput(
                  num_id(field),
                  label = NULL,
                  value = spec$default,
                  min = spec$min,
                  max = spec$max
                )
              ),

              column(
                8,

                sliderInput(
                  slider_id(field),
                  label = NULL,
                  value = spec$default,
                  min = spec$min,
                  max = spec$max,
                  step = 1
                )
              )
            )
          )
        }
      )
    )
  })


  # ---------------------------------------------------------------------
  # NUMERIC <-> SLIDER SYNCHRONIZATION
  # ---------------------------------------------------------------------

  observeEvent(
    input$pattern,
    {

      for (obs in field_sync$observers) {
        obs$destroy()
      }

      fields <- names(
        PATTERN_DEFS[[input$pattern]]$measurements
      )

      new_observers <- list()

      for (field in fields) {

        local({

          f <- field

          n_id <- num_id(f)

          s_id <- slider_id(f)


          # Numeric -> slider

          new_observers[[paste0(f, "_num")]] <<- observeEvent(

            input[[n_id]],

            {

              val <- input[[n_id]]

              if (
                !is.null(val) &&
                !is.null(input[[s_id]]) &&
                !isTRUE(
                  all.equal(
                    val,
                    input[[s_id]]
                  )
                )
              ) {

                updateSliderInput(
                  session,
                  s_id,
                  value = val
                )
              }

            },

            ignoreInit = TRUE
          )


          # Slider -> numeric

          new_observers[[paste0(f, "_slider")]] <<- observeEvent(

            input[[s_id]],

            {

              val <- input[[s_id]]

              if (
                !is.null(val) &&
                !is.null(input[[n_id]]) &&
                !isTRUE(
                  all.equal(
                    val,
                    input[[n_id]]
                  )
                )
              ) {

                updateNumericInput(
                  session,
                  n_id,
                  value = val
                )
              }

            },

            ignoreInit = TRUE
          )
        })
      }

      field_sync$observers <- new_observers

    },

    ignoreNULL = FALSE
  )


  # ---------------------------------------------------------------------
  # PATTERN DESCRIPTIONS
  # ---------------------------------------------------------------------

  pattern_descriptions <- list(

    "bag" = list(

      title = "The Bag – Medieval Alms Purse (aumônières)",

      text = c(

        "Go get that bag, girl! Without any pockets in Medieval clothes, people needed somewhere to keep their money or personal belongings – and a small pouch was the fashionable way to go.",

        "The wealthy would give small amounts of money to beggars as “alms,” which they kept in richly decorated pouches. Over-the-top decoration, tassels, and embroidery have been found in surviving examples. More is more! There are many pouches pictured in the Codex Manesse."
      )
    ),


    "underdress" = list(

      title = "The Underdress – Undergarment or Undershirt (Chemise)",

      text = c(

        "Before modern underwear was invented, this garment was what women wore underneath their dresses or surcoats. The underdress also worked as a nightgown, so it served a dual purpose.",

        "Wearing a dress underneath also helped protect finer outer garments from sweat and wear. The dress was typically made of linen and could reach the knees or ankles, depending on the fashion of the time.",

        "Around the neck, there could be a simple collar or a drawstring. This can be created by making a wide seam through which you can pull a ribbon."
      )
    ),


    "circleskirt" = list(

      title = "The Circle Skirt – or Medieval Cloak?",

      text = c(

        "The circle skirt pattern is not Medieval, but it is a great way to start sewing. It could also be used to model a Medieval cloak pattern if you remove ¼ of the circle, which would become the opening of the cloak.",

        "The full-length, floor-length cloak was usually made from wool and was multipurpose: it could be used as protection from the weather, a blanket, and a sleeping bag all in one. It could be fastened with tassels, metal clasps (fibulae), or chains, and lined with linen or a lighter wool fabric if needed.",

        "Wealthier people had their cloaks decorated with embroidery along the edges, used tablet-woven borders, and sometimes lined them with fur."
      )
    ),


    "surcoat" = list(

      title = "The Surcoat – Sideless Surcote (Pellote)",

      text = c(

        "The sideless surcoat was a popular and striking court garment. It first started out as a unisex garment in the 12th century. Later, the armholes grew in size in women’s fashion during the Late Middle Ages, becoming known as the “Gates of Hell”.",

        "The construction is quite simple, and the garment was usually made of wool, linen, cotton velveteen, or silk.",

        "When it came to color they did not hold back – remember, more is more!"
      )
    )
  )


  # ---------------------------------------------------------------------
  # DISPLAY SELECTED PATTERN DESCRIPTION
  # ---------------------------------------------------------------------

  output$pattern_description <- renderUI({

    req(input$pattern)

    current <- pattern_descriptions[[input$pattern]]

    if (is.null(current)) {
      return(NULL)
    }

    div(

      class = "pattern-description",

      h2(current$title),

      lapply(
        current$text,
        function(paragraph) {
          p(paragraph)
        }
      )
    )
  })


  # ---------------------------------------------------------------------
  # ASSEMBLE STANDARDIZED INPUT OBJECT
  # ---------------------------------------------------------------------

  raw_inputs <- reactive({

    req(input$pattern)

    fields <- PATTERN_DEFS[[input$pattern]]$measurements

    scale <- (
      input$size_scale %||% 100
    ) / 100


    measurements <- lapply(

      names(fields),

      function(field) {

        val <- input[[num_id(field)]]

        if (is.null(val)) {

          NA_real_

        } else {

          val * scale
        }
      }
    )


    names(measurements) <- names(fields)


    list(

      pattern = input$pattern,

      measurements = measurements,

      fabric = list(
        width = input$fabric_width
      ),

      options = list()
    )
  })


  # ---------------------------------------------------------------------
  # DEBOUNCE
  # ---------------------------------------------------------------------

  raw_inputs_debounced <- debounce(
    raw_inputs,
    millis = 400
  )


  # ---------------------------------------------------------------------
  # RUN FULL PIPELINE
  # ---------------------------------------------------------------------

  pipeline_output <- reactive({

    run_pipeline_safe(
      raw_inputs_debounced()
    )
  })


  # ---------------------------------------------------------------------
  # VALIDATION ERRORS
  # ---------------------------------------------------------------------

  output$validation_errors <- renderUI({

    out <- pipeline_output()

    if (!is.null(out$error)) {

      div(

        class = "alert alert-danger",

        tags$strong(
          "Please fix the following:"
        ),

        tags$pre(
          out$error
        )
      )
    }
  })


  # ---------------------------------------------------------------------
  # PATTERN PLOT
  # ---------------------------------------------------------------------

  output$pattern_plot <- renderPlot({

    out <- pipeline_output()

    req(out$result)

    plot_pattern(
      out$result$pattern,
      out$result$pattern_layout
    )
  })


  # ---------------------------------------------------------------------
  # FABRIC LAYOUT PLOT
  # ---------------------------------------------------------------------

  output$fabric_layout_plot <- renderPlot({

    out <- pipeline_output()

    req(out$result)

    plot_fabric_layout(
      out$result$pattern,
      out$result$layout
    )
  })


  # ---------------------------------------------------------------------
  # RESULTS TABLE
  # ---------------------------------------------------------------------

  output$fabric_summary_table <- renderTable({

    out <- pipeline_output()

    req(out$result)

    fr <- out$result$fabric

    data.frame(

      Metric = c(
        "Required length",
        "Recommended length",
        "Fabric width",
        "Margin",
        "Units"
      ),

      Value = c(
        fr$required_length,
        fr$recommended_length,
        fr$fabric_width,
        fr$margin,
        fr$units
      )
    )
  })
}


# -----------------------------------------------------------------------
# NULL-COALESCING OPERATOR
# -----------------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}