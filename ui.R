
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
      "Base / Conversion", 
      choices = c("NZD", "AUD", "USD")
    ),
    selectInput("conv_cur", NULL, choices = character(0)),
    checkboxInput("only_increasing", label = "Increasing Rates Only", value = TRUE),
    radioButtons(
      "conv_cur_lag_rate", 
      label = NULL, 
      choices = list(">1" = "1", ">7" = "7", ">30" = "30", ">100" = "100"),
      selected = "7",
      inline = TRUE
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

