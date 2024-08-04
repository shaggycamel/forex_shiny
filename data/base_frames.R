
# Symbols -----------------------------------------------------------------

df_symbols <- nba.dataRub::dh_getQuery(db_con, "SELECT * FROM forex.country_symbol")


# Rates -------------------------------------------------------------------

mean_diff_rank_calc <- function(df, interval){
  df |> 
    dplyr::mutate(
      !!paste0("rate_mean_", interval) := slider::slide_mean(rate, before = interval, complete = TRUE),
      !!paste0("rate_lag_", interval) := dplyr::lag(rate, interval),
      .by = c(base_cur, conversion_cur)
    ) |> 
    dplyr::mutate(
      !!paste0("perc_rate_diff_", interval) :=
        (rate - !!rlang::sym(paste0("rate_lag_", interval))) /
          ((rate + !!rlang::sym(paste0("rate_lag_", interval))) / 2)
    ) |> 
    dplyr::mutate(
      !!paste0("perc_diff_rank_", interval) := dplyr::dense_rank(!!rlang::sym(paste0("perc_rate_diff_", interval)) * -1),
      .by = c(base_cur, date)
    )
}

observed_currencies <- "'NZD', 'AUD', 'USD'"
df_rates <<- nba.dataRub::dh_getQuery(db_con, "anl_query.sql") |> 
  mean_diff_rank_calc(1) |> 
  mean_diff_rank_calc(7) |> 
  mean_diff_rank_calc(30) |> 
  mean_diff_rank_calc(100) |> 
  dplyr::left_join(df_symbols, by = dplyr::join_by(conversion_cur == symbol))

