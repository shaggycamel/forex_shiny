
# Rates -------------------------------------------------------------------

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

df_rates <<- dh_getQuery(db_con, "anl_query.sql") |> 
  mean_diff_rank_calc(1) |> 
  mean_diff_rank_calc(7) |> 
  mean_diff_rank_calc(30) |> 
  mean_diff_rank_calc(100)