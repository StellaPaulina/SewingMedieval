# -----------------------------------------------------------------------
# app_ui.R
# Owner: Fatemeh
#
# Only depends on PATTERN_DEFS (constants.R).
#
# Includes:
#   - Medieval Sewing title + logo
#   - Welcome text
#   - Dynamic pattern selection
#   - Dynamic measurement controls
#   - Live pattern visualization
#   - Pattern description
#   - Background audio with manual sound control
# -----------------------------------------------------------------------

app_ui <- function() {

  fluidPage(

    # ===================================================================
    # HEAD
    # ===================================================================

    tags$head(

      # ---------------------------------------------------------------
      # Google Fonts
      # ---------------------------------------------------------------

      tags$link(
        rel = "preconnect",
        href = "https://fonts.googleapis.com"
      ),

      tags$link(
        rel = "preconnect",
        href = "https://fonts.gstatic.com",
        crossorigin = ""
      ),

      tags$link(
        rel = "stylesheet",
        href = paste0(
          "https://fonts.googleapis.com/css2?",
          "family=Cinzel:wght@500;700&",
          "family=Cinzel+Decorative:wght@700&",
          "family=EB+Garamond:ital,wght@0,400;0,600;1,400&",
          "display=swap"
        )
      ),

      # ---------------------------------------------------------------
      # Main CSS
      # ---------------------------------------------------------------

      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      ),


      # ===============================================================
      # AUDIO JAVASCRIPT
      # ===============================================================

      tags$script(HTML("

        $(document).on('shiny:connected', function() {

          // -----------------------------------------------------------
          // Find audio and button
          // -----------------------------------------------------------

          const audio =
            document.getElementById('background-audio');

          const button =
            document.getElementById('sound_toggle');


          // -----------------------------------------------------------
          // Safety check
          // -----------------------------------------------------------

          if (!audio || !button) {

            console.log(
              'Audio element or sound button was not found.'
            );

            return;
          }


          // -----------------------------------------------------------
          // Initial audio settings
          // -----------------------------------------------------------

          audio.volume = 0.35;

          let soundEnabled = false;


          // -----------------------------------------------------------
          // Update button appearance
          // -----------------------------------------------------------

          function updateSoundButton() {

            if (soundEnabled) {

              button.innerHTML = '🔊 Sound On';

            } else {

              button.innerHTML = '🔇 Sound Off';
            }
          }


          // -----------------------------------------------------------
          // Start audio
          // -----------------------------------------------------------

          function startAudio() {

            audio.volume = 0.35;

            const playPromise = audio.play();


            // ---------------------------------------------------------
            // Modern browsers return a Promise from audio.play()
            // ---------------------------------------------------------

            if (playPromise !== undefined) {

              playPromise

                .then(function() {

                  soundEnabled = true;

                  updateSoundButton();

                })

                .catch(function(error) {

                  console.log(
                    'Browser prevented audio playback:',
                    error
                  );

                  soundEnabled = false;

                  updateSoundButton();
                });

            } else {

              soundEnabled = true;

              updateSoundButton();
            }
          }


          // -----------------------------------------------------------
          // Stop audio
          // -----------------------------------------------------------

          function stopAudio() {

            audio.pause();

            soundEnabled = false;

            updateSoundButton();
          }


          // -----------------------------------------------------------
          // SOUND BUTTON
          // -----------------------------------------------------------

          button.addEventListener(
            'click',
            function(event) {

              // Prevent Shiny from doing anything unnecessary
              event.preventDefault();

              // -------------------------------------------------------
              // If audio is currently playing -> stop it
              // -------------------------------------------------------

              if (!audio.paused) {

                stopAudio();

              }

              // -------------------------------------------------------
              // Otherwise -> start it
              // -------------------------------------------------------

              else {

                startAudio();
              }

            }
          );


          // -----------------------------------------------------------
          // Initial button state
          // -----------------------------------------------------------

          updateSoundButton();

        });

      "))
    ),


    # ===================================================================
    # TITLE + LOGO
    # ===================================================================

    div(
      class = "app-title",

      # ---------------------------------------------------------------
      # Title
      # ---------------------------------------------------------------

      div(
        class = "title-text",

        h1("Welcome"),

        h3("to Sewing Medieval")
      ),

      # ---------------------------------------------------------------
      # Logo
      # ---------------------------------------------------------------

      tags$img(
        src = "logo.svg",
        class = "app-logo",
        alt = "Sewing Pattern Generator crest"
      )
    ),


    # ===================================================================
    # SOUND CONTROL
    # ===================================================================

    div(
      class = "sound-control",

      # ---------------------------------------------------------------
      # Audio file
      # ---------------------------------------------------------------

      tags$audio(
        id = "background-audio",
        src = "freesound.mp3",
        loop = "loop",
        preload = "auto"
      ),

      # ---------------------------------------------------------------
      # Sound button
      # ---------------------------------------------------------------

      actionButton(
        inputId = "sound_toggle",
        label = "🔇 Sound Off",
        class = "sound-button"
      )
    ),


    # ===================================================================
    # WELCOME MESSAGE
    # ===================================================================

    div(
      class = "history-banner",

      p(
        "Hello, we are so glad that you have found this Medieval sewing pattern page!"
      ),

      p(
        "You can have any level of previous sewing experience and use our ",
        "visualization tool to learn about Medieval fashion. Surprisingly, ",
        "sewing medieval is simple since it consists of patterns that use ",
        "basic geometric shapes instantly recognizable. Let’s get to it!"
      )
    ),


    # ===================================================================
    # MAIN APPLICATION
    # ===================================================================

    sidebarLayout(

      # =================================================================
      # SIDEBAR
      # =================================================================

      sidebarPanel(

        # ---------------------------------------------------------------
        # Pattern selection
        # ---------------------------------------------------------------

        selectInput(
          "pattern",
          "Pattern",
          choices = PATTERN_CHOICES,
          selected = PATTERN_CHOICES[1]
        ),


        # ---------------------------------------------------------------
        # Dynamic measurement inputs
        # ---------------------------------------------------------------

        uiOutput(
          "measurement_inputs"
        ),


        # ---------------------------------------------------------------
        # Size adjustment
        # ---------------------------------------------------------------

        sliderInput(
          "size_scale",
          "Size adjustment (%)",
          min = 80,
          max = 120,
          value = 100,
          step = 1
        ),


        # ---------------------------------------------------------------
        # Fabric width
        # ---------------------------------------------------------------

        numericInput(
          "fabric_width",
          "Fabric width (cm)",
          value = DEFAULT_FABRIC_WIDTH,
          min = 1,
          max = 300
        ),


        # ---------------------------------------------------------------
        # Separator
        # ---------------------------------------------------------------

        hr(),


        # ---------------------------------------------------------------
        # Live update message
        # ---------------------------------------------------------------

        p(
          class = "live-update-note",

          HTML(
            "&#10022; Pattern updates automatically as you adjust any value."
          )
        ),


        # ---------------------------------------------------------------
        # Validation messages
        # ---------------------------------------------------------------

        uiOutput(
          "validation_errors"
        ),


        # ---------------------------------------------------------------
        # Selected pattern description
        # ---------------------------------------------------------------

        uiOutput(
          "pattern_description"
        )
      ),


      # =================================================================
      # MAIN PANEL
      # =================================================================

      mainPanel(

        tabsetPanel(

          # -------------------------------------------------------------
          # Pattern
          # -------------------------------------------------------------

          tabPanel(

            "Pattern",

            plotOutput(
              "pattern_plot",
              height = "600px"
            )
          ),


          # -------------------------------------------------------------
          # Fabric layout
          # -------------------------------------------------------------

          tabPanel(

            "Fabric layout",

            plotOutput(
              "fabric_layout_plot",
              height = "600px"
            )
          ),


          # -------------------------------------------------------------
          # Results
          # -------------------------------------------------------------

          tabPanel(

            "Results",

            tableOutput(
              "fabric_summary_table"
            )
          )
        )
      )
    )
  )
}