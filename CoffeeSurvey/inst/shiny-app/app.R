library(shinydashboard)
library(shinycssloaders)
library(DT)
library(tigris)
library(shiny)
library(tidyverse)
library(rvest)
library(CoffeeSurvey)
library(plotly)
library(knitr)

# Data process
popularity <- coffee_survey |>
  filter(!is.na(prefer_overall)) |>
  count(prefer_overall) |>
  mutate(percentage = n / sum(n) * 100) |>
  arrange(desc(n))

age_gender_distribution <- coffee_survey |>
  filter(!is.na(gender) & !is.na(age)) |>
  count(age, gender) |>
  group_by(age) |>
  mutate(percentage = n / sum(n) * 100) |>
  ungroup()

where_drink_distribution <- coffee_survey |>
  filter(!is.na(where_drink)) |>
  filter(where_drink %in% c("At home", "At the office", "At a cafe", "On the go")) |>
  count(where_drink) |>
  mutate(percentage = n / sum(n) * 100) |>
  arrange(desc(percentage))

brew_distribution <- coffee_survey |>
  filter(!is.na(brew)) |>
  filter(brew %in% c("Pour over", "Espresso", "French press", "Other", "Coffee brewing machine", "Cold brew")) |>
  count(brew) |>
  mutate(percentage = n / sum(n) * 100) |>
  arrange(desc(percentage))

most_paid_distribution <- coffee_survey |>
  filter(!is.na(most_paid)) |>
  count(most_paid) |>
  mutate(percentage = n / sum(n) * 100) |>
  arrange(desc(percentage))

# Define colors for coffees
coffee_colors <- c(
  "Coffee A" = "#8B4513",
  "Coffee B" = "#D2B48C",
  "Coffee C" = "#C19A6B",
  "Coffee D" = "#4B3832")

# Define colors for each gender
colors <- c("Female" = "pink",
            "Male" = "#87c6e8",
            "Non-binary" = "#dda1e6",
            "Other" = "#4dd6bb",
            "Prefer not to say" = "grey")

# Define custom colors for each brew method
custom_colors <- c(
  "Pour over" = "#4CAF50",
  "Espresso" = "#2196F3",
  "French press" = "#FF8C00",
  "Other" = "#9C27B0",
  "Coffee brewing machine" = "#FFC107",
  "Cold brew" = "#00ACC1")

# UI part
ui <- function() {
  fluidPage(
    dashboardPage(
      skin = "purple",
      dashboardHeader(title = "Coffee Taste Test Analysis", titleWidth = 300),
      dashboardSidebar(
        width = 300,
        sidebarMenu(
          HTML(paste0(
            "<br>",
            "<img style='display: block; margin-left: auto; margin-right: auto;' src='coffee.jpg' width='300'>",
            "<br>"
          )),
          menuItem("Home", tabName = "home", icon = icon("home")),
          menuItem("Data Table", tabName = "table", icon = icon("table")),
          menuItem("Test Overview", tabName = "overview", icon = icon("stats", lib = "glyphicon")),
          menuItem("Coffee Preferences", tabName = "summary", icon = icon("magnifying-glass")),
          menuItem("Gender Distribution", tabName = "gender", icon = icon("person")),
          menuItem("Where and Brew", tabName = "wherebrew", icon = icon("mug-hot")),
          menuItem("Price Sensitivity", tabName = "price", icon = icon("coins")),
          menuItem("About", tabName = "tab_about", icon = icon("info"))
        )
      ),
      dashboardBody(
        tabItems(
        # Welcome Home Page
          tabItem(tabName = "home", includeMarkdown("www/home.md")),

          # Tab for "Data table"
          tabItem(tabName = "table",
                  fluidRow(
                    column(12,
                           h3("The Great American Coffee Taste Test Data"),
                           dataTableOutput("coffeeDataTable"),
                           box(
                             title = "Additional Information",
                             status = "info",
                             solidHeader = TRUE,
                             width = 12,
                             p("For further information regarding the coffee test, including the methodology and objectives, please refer to the Test Overview section.")
                           )
                    )
                  )
          ),

        # Tab for "Test overview"
          tabItem(tabName = "overview",
                  includeMarkdown("www/overview.md"),
                  fluidRow(plotlyOutput("popularityPlot"))
          ),

        # Tab for "Coffee Preferences"
        tabItem(tabName = "summary",
                fluidRow(
                  titlePanel("Summary Statistics and Preferences"),
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("brew", "Select Brew Method:", choices = c("All", unique(coffee_survey$brew))),
                      selectInput("gender_summary", "Select Gender:", choices = c("All", unique(coffee_survey$gender))),
                      selectInput("age_summary", "Select Age Group:", choices = c("All", unique(coffee_survey$age))),
                      helpText("Use the dropdowns to filter the dataset and explore the preferences as you want.")
                    ),
                    mainPanel(
                      verbatimTextOutput("summary"),
                      plotOutput("preferencePlot"),
                      tableOutput("filteredTable")
                    )
                  )
                )
        ),

        # Tab for "Gender distribution"
          tabItem(tabName = "gender",
                  fluidRow(
                    titlePanel("Coffee Age Group by Gender"),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("selected_gender", "Select Gender",
                                    choices = c("All", "Female", "Male", "Non-binary", "Other (please specify)", "Prefer not to say"),
                                    selected = "All"
                        ),
                        helpText("Use the dropdown menus to filter by gender and observe the differences.")
                      ),
                      mainPanel(plotOutput("genderPlot"))
                    )
                  )
          ),

        # Tab for "Where and Brew tab"
          tabItem(tabName = "wherebrew",
                  fluidRow(
                    titlePanel("Coffee Drinking Locations and Brewing Preferences"),
                    box(
                      title = "Where do you typically drink coffee?",
                      status = "primary",
                      width = 5,
                      tableOutput("whereBrewTable")
                    ),
                    box(
                      title = "Brewing Methods at Home",
                      status = "primary",
                      width = 7,
                      plotlyOutput("brewPlot") %>% withSpinner(color = "purple")
                    )
                  )
          ),

         # Tab for "Price Sensitivity"
          tabItem(tabName = "price",
                  fluidRow(
                    titlePanel("Price Sensitivity for Coffee"),
                    box(
                      title = "Maximum Amount Paid for a Cup of Coffee",
                      status = "primary",
                      width = 12,
                      plotlyOutput("pricePlot") %>% withSpinner(color = "purple")
                    )
                  )
          ),

        # Tab for "About"
          tabItem(
            "tab_about",
            fluidRow(
              box(
                title = "About me", status = "warning", width = "6 col-lg-4",
                tags$p(
                  class = "text-center",
                  tags$img(class = "img-responsive img-rounded center-block", src = "profile.jpg", style = "max-width: 150px;")
                ),
                tags$p(
                  class = "text-center",
                  HTML("&#128075;"),
                  tags$strong("Hi! I'm Christy.")
                ),
                tags$p(
                  "I am an early childhood teacher and a new student exploring the field of business analytics.",
                  "Currently, I’m learning and experimenting with R, this is my first R package and shiny app.",
                  "You can find similar analysis in my personal blog",
                  HTML(paste0(tags$a(href = "https://etc5523-2024.github.io/assignment-3-creating-a-blog-wlai0012/", "blog-wlai0012", target = "_blank"), "."))
                ),
                tags$p(
                  "Get in touch with me on Instagram at",
                  HTML(paste0("(", tags$a(href = "https://www.instagram.com/christy_llllllwh/profilecard/?igsh=MXhsYmRnc2UzY3hqbQ==", "@christy", target = "_blank"), "),")),
                  "or by email at",
                  HTML(paste0(tags$a(href = "mailto:wlai0012@student.monash.edu", "wlai0012@student.monash.edu"), "."))
                ),
                tags$p(
                  "This About dashboard was built using a template provided by",
                  HTML(paste0(tags$a(href = "https://garrickadenbuie.com", "garrickadenbuie.com", target = "_blank"), ".")),
                  "Thank you!"
                )
              ),
              box(
                title = "About this Dashboard",
                width = "6 col-lg-4",
                tags$p(
                  class = "text-center",
                  tags$a(
                    href = "https://www.r-project.org",
                    target = "_blank",
                    tags$img(class = "image-responsive",
                             src = "https://www.r-project.org/logo/Rlogo.svg",
                             style = "max-width: 150px;"
                    )
                  ),
                  tags$a(
                    href = "https://shiny.posit.co/",
                    target = "_blank",
                    tags$img(class = "image-responsive",
                             src = "shiny.jpeg",
                             style = "max-width: 150px; margin-left: 2em;"
                    )
                  ),
                  tags$a(
                    href = " ",
                    target = "_blank",
                    tags$img(class = "image-responsive",
                             src = "CoffeeSurvey.png",
                             style = "max-width: 150px; margin-left: 2em;"
                    )
                  )
                  ),
                tags$p(
                  "This dashboard was built in",
                  tags$a(href = "https://r-project.org", target = "_blank", "R"),
                  "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
                  tags$strong("shiny,"),
                  tags$strong("shinydashboard,"),
                  tags$strong("plotly,"),
                  "the", tags$strong("tidyverse,"),
                  "and many more packages.",
                  "You can find the code on",
                  HTML(paste0(tags$a(href = "https://github.com/ETC5523-2024/assignment-4-packages-and-shiny-apps-wlai0012", "GitHub", target = "_blank"), "."))
                )
              )
            )
          )
        )
      )
    )
  )
}


# Server part

server <- function(input, output) {

 # Data Table
  output$coffeeDataTable <- renderDataTable({
    coffee_survey[, c('prefer_overall', 'gender', 'age', 'where_drink', 'brew', 'most_paid')] %>%
      datatable(filter = "top", options = list(pageLength = 10),
                colnames = c('Preference Overall', 'Gender', 'Age', 'Where Drink', 'Brew Method', 'Most Paid'))
  })

 # Test overview plot
  output$popularityPlot <- renderPlotly({
    gg1 <- ggplot(popularity, aes(x = prefer_overall, y = percentage, fill = prefer_overall)) +
      geom_bar(stat = "identity") +
      labs(title = "Popularity of Coffee Types",
           x = "Coffee Type",
           y = "Percentage of Votes") +
      scale_fill_manual(values = coffee_colors) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )

    ggplotly(gg1)
  })

  # Coffee perferences plot
  output$preferencePlot <- renderPlot({
    filtered_data <- coffee_survey

    if (input$brew != "All") {
      filtered_data <- filtered_data %>% filter(brew == input$brew)
    }
    if (input$gender_summary != "All") {
      filtered_data <- filtered_data %>% filter(gender == input$gender_summary)
    }
    if (input$age_summary != "All") {
      filtered_data <- filtered_data %>% filter(age == input$age_summary)
    }

    ggplot(filtered_data, aes(x = prefer_overall, fill = prefer_overall)) +
      geom_bar() +
      labs(title = "Filtered Coffee Preferences",
           x = "Coffee Preference",
           y = "Count") +
      scale_fill_manual(values = coffee_colors) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
  })


 # Gender distribution plot
  output$genderPlot <- renderPlot({
    gender_data <- age_gender_distribution

    if (input$selected_gender != "All") {
      gender_data <- gender_data %>% filter(gender == input$selected_gender)
    }

 ggplot(gender_data, aes(x = percentage, y = age, fill = gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = colors) +
      labs(title = "Age Group Distribution by Gender",
           x = "Percentage (%)",
           y = "Age Group") +
      theme_minimal() +
   theme(
     plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
     axis.title.x = element_text(size = 12),
     axis.title.y = element_text(size = 12)
   )

  })

 # Where drink table
  output$whereBrewTable <- renderTable({
    summary_table <- where_drink_distribution |>
      select(
        Location = where_drink,
        `Percentage Responded` = percentage
      ) |>
      mutate(`Percentage Responded` = round(`Percentage Responded`, 1)) |>
      arrange(desc(`Percentage Responded`))

    summary_table
  }, rownames = TRUE, sanitize.text.function = identity)

 # Brew method plot
  output$brewPlot <- renderPlotly({

    gg2 <- brew_distribution %>%
      as.data.frame() %>%
      ggplot(aes(
        x = fct_reorder(brew, -percentage),
        y = percentage,
        fill = brew
      )) +
      geom_col(color = "black") +
      scale_fill_manual(values = custom_colors) +
      theme_minimal() +
      labs(
        x = "Brewing Method",
        y = "Percentage of Respondents",
        title = "How do you brew coffee at home?"
      )  +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none"
      )

    ggplotly(gg2)
  })

 # Price plot
  output$pricePlot <- renderPlotly({

    single_color <- "#edc453"

    gg3 <- ggplot(most_paid_distribution,
                 aes(x = percentage,
                     y = reorder(most_paid, percentage),
                     fill = most_paid)) +
      geom_bar(stat = "identity", color = "black", fill = single_color) +
      labs(title = "What is the Most You've Ever Paid for a Cup of Coffee?",
           x = "Percentage of Respondents",
           y = "Maximum Amount Paid") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none"
      )

    ggplotly(gg3) %>%
      layout(
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        hovermode = "y"
      )
  })

}

# Run the application
shinyApp(ui = ui, server = server)

