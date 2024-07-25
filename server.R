

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(DT)
library(nba.dataRub)
library(here)
# library(shinycssloaders)


# Server code -------------------------------------------------------------

server <- function(input, output, session) {

  
# Variables ---------------------------------------------------------------
  
  db_con <<- dh_createCon("cockroach") 
  
# Load datasets -----------------------------------------------------------

  if(Sys.info()["user"] == "shiny") load(".RData") else source(here("data", "base_frames.R"))
  
# Reactivity --------------------------------------------------------------

  df_look <- reactive({
    df_rates |>
      filter(base_cur == input$base_cur) |>
      filter(date == max(date), if_any(contains("rank"), \(x) x <= 5)) |>
      # select(conversion_cur, starts_with("perc_diff_rank")) |>
      arrange(perc_diff_rank_1) |> 
      select(conversion_cur)
  })
 
  bindEvent(observe({
    updateSelectInput(
      session,
      "conv_cur",
      choices = unique(df_rates$conversion_cur),
      selected = (df_look())$conversion_cur[1]
    ) 
  }), input$base_cur)

  
# Rates Overview -------------------------------------------------

  output$rates_look <- renderDT({
    
    # remove headers and make cell tooltips which show rank, rate, perc diff
    datatable(
      df_look(),
      rownames = FALSE,
      escape = FALSE,
      style = "default",
      options = lst(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        initComplete = JS(
          "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
          "}"
        )
      )
    )
  })
  
  output$rate_trend <- renderPlotly({
    # Add border to plot
    filter(df_rates, base_cur == input$base_cur, conversion_cur == input$conv_cur) |> 
      plot_ly(x = ~date, y = ~rate, name = "daily", type = "scatter", mode = "lines+markers") |> 
      add_trace(y = ~rate_mean_7, name = "7 day ma", mode = "lines") |> 
      add_trace(y = ~rate_mean_30, name = "30 day ma", mode = "lines") |> 
      add_trace(y = ~rate_mean_100, name = "100 day ma", mode = "lines") |> 
      layout(
        title = list(text = paste(input$base_cur, input$conv_cur, sep = "/"), x = 0.08, y = 1.1),
        yaxis = list(title = "Rate"),
        xaxis = list(title = NA, rangeslider = list(type = "date")),
        hovermode="x unified"
      )

  })

# Rates Observe ------------------------------------------------------
 
} 