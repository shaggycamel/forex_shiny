

# Libraries ---------------------------------------------------------------

library(dplyr)
library(plotly)
library(DT)
library(nba.dataRub)
library(here)

library(magrittr)


# Server code -------------------------------------------------------------

server <- function(input, output, session) {

  
# Variables ---------------------------------------------------------------
  
  db_con <<- dh_createCon("cockroach") 
  
# Load datasets -----------------------------------------------------------

  if(Sys.info()["user"] == "shiny") load(".RData") else source(here("data", "base_frames.R"))
  
# Reactivity --------------------------------------------------------------

  df_look <- reactive({
    
    rt_rank <- paste0("perc_diff_rank_", input$conv_cur_lag_rate)
    rt_perc_diff <- paste0("perc_rate_diff_", input$conv_cur_lag_rate)

    df_rt <- df_rates
    if(input$only_increasing)
      df_rt <- filter(df_rt, !!sym(rt_perc_diff) > 0)
    
    df_rt |>
      filter(
        base_cur == input$base_cur,
        date == max(date),
        if_any(contains("rank"), \(x) x <= 5)
      ) |>
      arrange(desc(abs(!!sym(rt_perc_diff)))) |>
      select(conversion_cur, !!sym(rt_perc_diff))
    
  })
 
  bindEvent(observe({
    updateSelectInput(
      session,
      "conv_cur",
      choices = unique(df_rates$conversion_cur),
      selected = (df_look())$conversion_cur[1]
    ) 
  }), input$base_cur, input$only_increasing, input$conv_cur_lag_rate)
  
  bindEvent(observe({
    updateSelectInput(
      session,
      "conv_cur",
      choices = unique(df_rates$conversion_cur),
      selected = (df_look())$conversion_cur[input$rates_look_rows_selected]
    ) 
  }), input$rates_look_rows_selected)

# Rates Overview -------------------------------------------------

  output$rates_look <- renderDT({
    
    # make cell tooltips which show rank, rate, perc diff
    # add clickable filtering
    datatable(
      df_look(),
      rownames = FALSE,
      escape = FALSE,
      style = "default",
      selection = "single",
      options = lst(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  $('th', thead).css('display', 'none');",
          "}"
        )
      )
    ) |> 
      formatPercentage(columns = 2, digits = 1)
  })
  
  output$rate_trend <- renderPlotly({
    
    vline <- max(df_rates$date) - as.integer(input$conv_cur_lag_rate)
    
    # Add border to plot
    filter(df_rates, base_cur == input$base_cur, conversion_cur == input$conv_cur) |> 
      plot_ly(x = ~date, y = ~rate, name = "daily", type = "scatter", mode = "lines+markers") |> 
      add_trace(y = ~rate_mean_7, name = "7 day ma", mode = "lines") |> 
      add_trace(y = ~rate_mean_30, name = "30 day ma", mode = "lines") |> 
      add_trace(y = ~rate_mean_100, name = "100 day ma", mode = "lines") |> 
      config(displayModeBar = FALSE) |> 
      layout(
        title = list(text = paste(input$base_cur, input$conv_cur, sep = "/"), x = 0.08, y = 1.1),
        yaxis = list(title = "Rate"),
        xaxis = list(title = NA, rangeslider = list(type = "date")),
        legend = list(orientation="h", yanchor = "bottom", y = 1.05, xanchor = "right", x = 1),
        hovermode="x unified",
        shapes = list(
          type = "line", 
          y0 = 0, y1 = 1, 
          yref = "paper", 
          x0 = vline, x1 = vline,
          line = list(color = "grey", width = 1, dash = "dash")
        )
      )
  })

# Rates Observe ------------------------------------------------------
 
} 