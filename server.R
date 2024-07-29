

# Libraries ---------------------------------------------------------------

library(dplyr)
library(plotly)
library(DT)
library(nba.dataRub)
library(here)


# Server code -------------------------------------------------------------

server <- function(input, output, session) {

  
# Variables ---------------------------------------------------------------
  
  db_con <<- dh_createCon("cockroach") 
  
# Load datasets -----------------------------------------------------------

  if(Sys.info()["user"] == "shiny") load(".RData") else source(here("data", "base_frames.R"))
  
# Reactivity --------------------------------------------------------------

  df_look <- reactive({

    df_rt <- df_rates
    if(input$conv_cur_rate_lag_gt != "")
      df_rt <- filter(df_rt, rate > !!sym(input$conv_cur_rate_lag_gt))
    
    df_rt |>
      filter(
        base_cur == input$base_cur,
        date == max(date),
        if_any(contains("rank"), \(x) x <= 5)
      ) |>
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
    
    # make cell tooltips which show rank, rate, perc diff
    # add widget to filter on rate > rate_lag7/30/100
    # add clickable filtering
    datatable(
      df_look(),
      rownames = FALSE,
      escape = FALSE,
      style = "default",
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
    )
  })
  
  output$rate_trend <- renderPlotly({
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
        hovermode="x unified"
      )

  })

# Rates Observe ------------------------------------------------------
 
} 