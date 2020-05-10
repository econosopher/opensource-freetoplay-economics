tab_progression_curves <-
  tabItem(tabName = "progression_curves",
          fluidRow(
            #column(4,
                   box(title = "General Inputs",
                       status = "warning",
                       solidHeader = TRUE,
                       sliderInput("levels", "Levels", min = 1, max = 1000, value = 100),
                       sliderInput("hours", "Hours to Complete", min = 1, max = 1000, value = 50),
                       ),
            #),
            #column(4,
                   box(title = "Exponential Curve Inputs",
                       status = "warning",
                       solidHeader = TRUE,
                       sliderInput("levels", "Levels", min = 1, max = 1000, value = 100),
                       sliderInput("hours", "Hours to Complete", min = 1, max = 1000, value = 50),
                       ),
            #),
            #column(4,
                   box(title = "S-Curve Curve Inputs",
                       status = "warning",
                       solidHeader = TRUE,
                       sliderInput("sequence_length", "Sequence Level Length", min = 1, max = 100, value = 10),
                       sliderInput("sequences", "# of Repeating Sequences", min = 1, max = 100, value = 5),
                       sliderInput("ymin", "ymin", min = 0, max = 100, value = 0),
                       sliderInput("ymax", "ymax", min = 0, max = 100, value = 10),
                       sliderInput("x50L", "x50L", min = 0, max = 100, value = 4),
                       sliderInput("x50U", "x50U", min = 0, max = 100, value = 6)
                       )
            #)
          ),
          h4("Curves"),
          plotlyOutput("progression_plot")
          )
