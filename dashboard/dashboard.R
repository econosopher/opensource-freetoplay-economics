if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny,
  shinydashboard,
  shinyWidgets,
  ggthemes,
  tidyverse,
  DT,
  plotly,
  scales,
  tsibble,
  stringr,
  lubridate,
  readxl,
  tidyxl,
  unpivotr,
  tidyquant,
  gt,
  glue,
  viridis
)

# --- Source All Components ---

# 1. Load All Modules
# Sourcing modules first ensures their functions are available for the UI definitions.
source("../R/mod_finance.R")
source("../R/mod_hard_currency.R")
source("../R/mod_metric_picker.R")

# 2. Load All Tab UI Definitions
# These files call the UI functions from the modules.
source("ui_elements/tabs/tab_finance.R")
source("ui_elements/tabs/tab_hard_currency.R")
source("ui_elements/tabs/tab_metric_picker.R")

# 3. Load Main UI Layout
source("ui_elements/sidebar.R")
source("ui_elements/body.R")

# 4. Load Data Helper Functions
source("../R/simfin_data.R")


# --- App Setup ---

# Set the SimFin API key and cache directory
# Make sure to set your SIMFIN_API_KEY environment variable!
set_simfin_api_key()
set_simfin_cache()

# Define the list of target company tickers
target_tickers <- c(
    "EA"
)

# --- Reactive Values ---
# Encapsulate the tickers in a reactive expression for the modules
reactive_tickers <- reactive(target_tickers)


# --- UI Definition ---
ui <-
  dashboardPage(
    dashboardHeader(title = "F2P Economics"),
    sidebar,
    body
  )

# --- Server Logic ---
server <- function(input, output) {

  # --- Module Servers ---
  # Call the server logic for our new hard currency module
  hard_currency_server("hard_currency_module")
  # Call the server logic for our new Finance module
  finance_server("finance_module", tickers = reactive_tickers)
  # Call the server logic for our new Metric Picker module
  metric_picker_server("metric_picker_module", tickers = reactive_tickers)

}

shinyApp(ui = ui, server = server)

