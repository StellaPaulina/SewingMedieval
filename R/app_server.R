# -----------------------------------------------------------------------
# app_server.R
# Owner: fatemeh
#
# this file never needs to change.
#
# Each measurement now gets a synced numericInput + sliderInput pair
# (either can be used, they always agree), and the whole pipeline
# re-runs live (debounced ~400ms) as soon as any input changes - no
# "Generate pattern" button needed any more.
# -----------------------------------------------------------------------

app_server <- function(input, output, session) {

  num_id    <- function(field) paste0("meas_num_", field)
  slider_id <- function(field) paste0("meas_slider_", field)

  # Holds the currently-active numeric<->slider sync observers so they
  # can be torn down and rebuilt whenever the selected pattern (and
  # therefore the set of measurement fields) changes.
  field_sync <- reactiveValues(observers = list())

  # --- dynamic measurement inputs: one numeric+slider pair per field ----
  output$measurement_inputs <- renderUI({
    req(input$pattern)
    fields <- PATTERN_DEFS[[input$pattern]]$measurements

    tagList(lapply(names(fields), function(field) {
      spec <- fields[[field]]
      tagList(
        tags$label(class = "control-label", spec$label),
        fluidRow(
          column(
            4,
            numericInput(num_id(field), label = NULL, value = spec$default,
                         min = spec$min, max = spec$max)
          ),
          column(
            8,
            sliderInput(slider_id(field), label = NULL, value = spec$default,
                        min = spec$min, max = spec$max, step = 1)
          )
        )
      )
    }))
  })

  # --- (re)build the numeric<->slider sync observers whenever the ------
  # --- pattern changes, since the field set (and therefore the input --
  # --- ids) changes too.
  observeEvent(input$pattern, {
    for (obs in field_sync$observers) obs$destroy()

    fields <- names(PATTERN_DEFS[[input$pattern]]$measurements)
    new_observers <- list()

    for (field in fields) {
      local({
        f      <- field
        n_id   <- num_id(f)
        s_id   <- slider_id(f)

        new_observers[[paste0(f, "_num")]] <<- observeEvent(input[[n_id]], {
          val <- input[[n_id]]
          if (!is.null(val) && !is.null(input[[s_id]]) &&
              !isTRUE(all.equal(val, input[[s_id]]))) {
            updateSliderInput(session, s_id, value = val)
          }
        }, ignoreInit = TRUE)

        new_observers[[paste0(f, "_slider")]] <<- observeEvent(input[[s_id]], {
          val <- input[[s_id]]
          if (!is.null(val) && !is.null(input[[n_id]]) &&
              !isTRUE(all.equal(val, input[[n_id]]))) {
            updateNumericInput(session, n_id, value = val)
          }
        }, ignoreInit = TRUE)
      })
    }

    field_sync$observers <- new_observers
  }, ignoreNULL = FALSE)

  # --- assemble the standardized `inputs` object from raw UI values ----
  raw_inputs <- reactive({
    req(input$pattern)
    fields <- PATTERN_DEFS[[input$pattern]]$measurements
    scale  <- (input$size_scale %||% 100) / 100

    measurements <- lapply(names(fields), function(field) {
      val <- input[[num_id(field)]]
      if (is.null(val)) NA_real_ else val * scale
    })
    names(measurements) <- names(fields)

    list(
      pattern      = input$pattern,
      measurements = measurements,
      fabric       = list(width = input$fabric_width),
      options      = list(seam_allowance = input$seam_allowance)
    )
  })

  # Debounce so dragging a slider doesn't re-run the pipeline on every
  # pixel of movement - it settles ~400ms after the last change.
  raw_inputs_debounced <- debounce(raw_inputs, millis = 400)

  # --- run the full A -> B -> C pipeline live, no button required -------
  pipeline_output <- reactive({
    run_pipeline_safe(raw_inputs_debounced())
  })

  # --- validation errors --------------------------------------------------
  output$validation_errors <- renderUI({
    out <- pipeline_output()
    if (!is.null(out$error)) {
      div(
        class = "alert alert-danger",
        tags$strong("Please fix the following:"),
        tags$pre(out$error)
      )
    }
  })

  # --- pattern plot --------------------------------------------------------
  output$pattern_plot <- renderPlot({
    out <- pipeline_output()
    req(out$result)
    plot_pattern(out$result$pattern)
  })

  # --- fabric layout plot ---------------------------------------------------
  output$fabric_layout_plot <- renderPlot({
    out <- pipeline_output()
    req(out$result)
    plot_fabric_layout(out$result$pattern, out$result$layout)
  })

  # --- results table -------------------------------------------------------
  output$fabric_summary_table <- renderTable({
    out <- pipeline_output()
    req(out$result)
    fr <- out$result$fabric
    data.frame(
      Metric = c("Required length", "Recommended length", "Fabric width", "Margin", "Units"),
      Value  = c(fr$required_length, fr$recommended_length, fr$fabric_width, fr$margin, fr$units)
    )
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
