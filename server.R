

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(plotly)
library(slider)
library(DT)
library(nba.dataRub)
# library(shinycssloaders)


# Server code -------------------------------------------------------------

server <- function(input, output, session) {
  
# Variables ---------------------------------------------------------------

  db_con <- dh_createCon("cockroach") 
  
# Load datasets -----------------------------------------------------------

  mean_diff_rank_calc <- function(df, interval){
    df |> 
      mutate(
        !!paste0("rate_mean_", interval) := slide_mean(rate, before = interval, complete = TRUE),
        !!paste0("rate_lag_", interval) := lag(rate, interval),
        .by = c(base_cur, conversion_cur)
      ) |> 
      mutate(
        !!paste0("perc_rate_diff_", interval) :=
          (rate - !!sym(paste0("rate_lag_", interval))) /
            ((rate + !!sym(paste0("rate_lag_", interval))) / 2)
      ) |> 
      mutate(
        !!paste0("perc_diff_rank_", interval) := dense_rank(!!sym(paste0("perc_rate_diff_", interval)) * -1),
        .by = c(base_cur, date)
      )
  }
  
  df_rates <- dh_getQuery(db_con, "anl_query.sql") |> 
    mean_diff_rank_calc(1) |> 
    mean_diff_rank_calc(7) |> 
    mean_diff_rank_calc(30) |> 
    mean_diff_rank_calc(100)
  
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