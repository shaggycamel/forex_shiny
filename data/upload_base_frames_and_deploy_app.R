

# Initialise workspace ----------------------------------------------------

# renv::restore(clean = TRUE, prompt = FALSE)


# Initialise variables ----------------------------------------------------

cat("\nInitialising variables...\n")
db_con <- nba.dataRub::dh_createCon("cockroach") 


# Read datasets -----------------------------------------------------------

cat("Creating datasets...\n")
source(here::here("data", "base_frames.R"))


# Save datasets -----------------------------------------------------------

cat("Saving image...\n")
rm(list = c("db_con", "mean_diff_rank_calc"))
save.image()


# Upload to Shiny server --------------------------------------------------

cat("Deploying to Shiny Server...\n\n")
rsconnect::deployApp(
  appDir = here::here(),
  appFiles = c(
    ".RData",
    ".Rprofile",
    "renv.lock", 
    "server.R", 
    "ui.R"
  ),
  appName = "Forex",
  appTitle = "Forex",
  appId = 12362129,
  account = "shaggycamel",
  server = "shinyapps.io",
  launch.browser = FALSE,
  logLevel = "normal",
  forceUpdate = TRUE
)

cat("Successfully deployed on:", format(as.POSIXct(Sys.time(), tz="NZ"), usetz=TRUE), "\n\n")


# Delete residual files ---------------------------------------------------

purrr::walk(c(".RData"), \(x) file.remove(here::here(x)))
