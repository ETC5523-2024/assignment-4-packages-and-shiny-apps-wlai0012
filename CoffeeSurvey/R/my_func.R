#' @title The Shiny App
#' @description
#' This shiny app is for Analysis of The Great American Coffee Taste Test.
#' @returns It will open a new browser window displaying the Shiny app.
#' @examples
#' \dontrun{
#' launch_app()
#' }
#' @export

launch_app <- function() {
  app_dir <- system.file("shiny-app", package = "CoffeeSurvey")
  shiny::runApp(app_dir, display.mode = "normal")
}

#' @title Missing Data Summary
#' @description
#' This function provides a summary of missing values in the coffee survey dataset.
#' @param data A data frame containing the coffee survey data.
#' @returns A data frame with the number and percentage of missing values for each column.
#' @examples
#' \dontrun{
#' missing_data_summary(coffee_survey)
#' }
#' @export

missing_data_summary <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Calculate missing values per column
  missing_values <- sapply(data, function(col) sum(is.na(col)))
  total_values <- nrow(data)

  # Create a data frame summarizing missing data
  summary_df <- data.frame(
    Column = names(data),
    Missing_Values = missing_values,
    Percent_Missing = (missing_values / total_values) * 100
  )

  return(summary_df)
}

