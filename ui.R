
# Libraries ---------------------------------------------------------------

library(shinyWidgets)
library(bslib)
library(DT)
library(plotly)


# Sidebar pages -----------------------------------------------------------
# Rates overview -----------------------------------------------------

page_rates_overview <- layout_sidebar(
  sidebar = sidebar(
    selectInput(
      "base_cur", 
      "Base Currency", 
      choices = c("NZD", "AUD", "USD")
    ),
    selectInput("conv_cur", "Conversion Currency", choices = character(0)),
    radioGroupButtons(
      "conv_cur_rate_lag_gt", 
      label = "Lag Direction", 
      choices = list("*" = NA, ">7" = "rate_lag_7", ">30" = "rate_lag_30", ">100" = "rate_lag_100"),
      selected = "rate_lag_7",
      status = "primary"
    ),
    card(full_screen = TRUE, DTOutput("rates_look")),
    padding = 9
  ),
  card(full_screen = TRUE, plotlyOutput("rate_trend")),
  fillable = TRUE,
)


# Rates Observe -------------------------------------------------------

page_rates_observe <- layout_sidebar(
  sidebar = sidebar(),
  # card(full_screen = TRUE, DTOutput("schedule_table")),
  fillable = TRUE
)


# Main UI -----------------------------------------------------------------

ui <- page_navbar(
  title = "Shaggy Camel Forex",
  nav_spacer(),
  nav_panel("Rates Overview", page_rates_overview),
  nav_panel("Rates Observe", page_rates_observe),
  theme = bs_theme(
    version = 5,
    preset = "litera",
    primary = "#133DEF"
  )
)

