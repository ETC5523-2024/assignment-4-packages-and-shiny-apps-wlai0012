
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CoffeeSurvey <img src="man/figures/CoffeeSurvey.png" align="right" height="80" width="80" alt="" />

CoffeeSurvey offers a comprehensive toolkit to analyze data from The
Great American Coffee Taste Test. This package empowers users to explore
consumer preferences, brewing methods, and price sensitivity, with
seamless integration of an interactive Shiny dashboard for deeper
insights.

The analysis is based on the following scenario: Imagine you are an
analyst working for a research company. Your team is conducting this
study to help a client develop a strategic plan for opening a new coffee
shop. Through this analysis, the goal is to uncover key trends in coffee
consumption and preferences, providing actionable insights for business
decisions.

## Installation

You can install the development version of CoffeeSurvey from
[GitHub](https://github.com/ETC5523-2024/assignment-4-packages-and-shiny-apps-wlai0012)
with:

``` r
# Install remotes if you don't have it
install.packages("remotes")

# Install the CoffeeSurvey package
remotes::install_github("ETC5523-2024/assignment-4-packages-and-shiny-apps-wlai0012", subdir = "CoffeeSurvey")
```

## Usage

After installation, load the package:

``` r
library(CoffeeSurvey)
```

You can load the coffee survey dataset using:

``` r
coffee_data <- data("coffee_survey")
```

This dataset provides insights into participants’ coffee preferences,
locations where they drink coffee, brewing methods, and the maximum
amounts they’ve paid for a cup of coffee.

## Shiny App

The CoffeeSurvey package includes a built-in Shiny app for interactive
data exploration. You can launch the app with:

``` r
CoffeeSurvey::launch_app()
```

The Shiny app provides interactive visualizations of:

- Coffee preferences

- Brewing methods and consumption locations

- Price sensitivity analysis

Explore trends and patterns easily with this engaging dashboard!

For more details on this package can be found on
[here](https://etc5523-2024.github.io/assignment-4-packages-and-shiny-apps-wlai0012/).
