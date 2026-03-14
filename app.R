library(shiny)
library(dplyr)
library(ggplot2)

squirrels <- read.csv("data/squirrels.csv", stringsAsFactors = FALSE)
squirrels <- squirrels |>
  mutate(
    primary_fur_color = ifelse(primary_fur_color == "" | is.na(primary_fur_color), "Unknown", primary_fur_color),
    age = ifelse(age %in% c("", "?", NA), "Unknown", age),
    location = ifelse(location == "" | is.na(location), "Unknown", location)
  )

ui <- fluidPage(
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50"
  ),
  tags$head(
    tags$style(HTML("
      .banner { width: 100%; max-height: 140px; object-fit: cover; border-radius: 8px; margin-bottom: 1.5rem; }
      .card-panel { background: white; padding: 1.25rem; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); margin-bottom: 1rem; }
      .sidebar { background: #f8f9fa !important; }
      .summary-table { font-size: 0.85rem; max-height: 280px; overflow-y: auto; }
      .total-count { background: #2c3e50; color: white; padding: 10px 16px; border-radius: 6px; font-weight: 600; font-size: 1.1rem; text-align: center; margin: 10px 0; }
      .about-section { background: #e8f4f8; padding: 12px; border-radius: 6px; font-size: 0.85rem; color: #2c3e50; margin-bottom: 1rem; border-left: 4px solid #2c3e50; }
      .filter-group { margin-bottom: 1rem; }
      .filter-group label { font-weight: 500; color: #2c3e50; }
    "))
  ),
  titlePanel(
    div(
      img(src = "img/squirrels_image.png", class = "banner", alt = "Squirrel"),
      h1("NYC Central Park Squirrel Tracker", style = "margin-top: 0; font-weight: 600;")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      width = 4,
      h4("About", style = "margin-top: 0; color: #2c3e50;"),
      div(
        class = "about-section",
        p("This dashboard explores the 2018 Central Park squirrel census—over 3,000 observations of squirrel behavior, fur color, and activity."),
        p("Vision: Make urban wildlife data accessible and actionable for researchers, park managers, and curious New Yorkers.")
      ),
      hr(),
      h4("Filters", style = "margin-top: 0; color: #2c3e50;"),
      div(
        class = "filter-group",
        selectInput("fur_color", "Fur Color",
          choices = c("All", sort(unique(squirrels$primary_fur_color))),
          selected = "All",
          width = "100%"
        )
      ),
      div(
        class = "filter-group",
        radioButtons("shift", "Time of Day",
          choices = c("All" = "All", "AM" = "AM", "PM" = "PM"),
          selected = "All",
          inline = TRUE
        )
      ),
      div(
        class = "filter-group",
        selectInput("age", "Age",
          choices = c("All", sort(unique(squirrels$age))),
          selected = "All",
          width = "100%"
        )
      ),
      div(
        class = "filter-group",
        selectInput("location", "Location",
          choices = c("All", sort(unique(squirrels$location))),
          selected = "All",
          width = "100%"
        )
      ),
      hr(),
      h4("Summary by Color & Shift", style = "margin-top: 0; color: #2c3e50;"),
      div(class = "total-count", textOutput("total_count")),
      div(class = "summary-table", tableOutput("summary_table")),
      downloadButton("download_summary", "Download CSV", class = "btn-sm"),
      hr(),
      p("2018 Central Park squirrel census", style = "font-size: 0.85rem; color: #6c757d;")
    ),
    mainPanel(
      width = 8,
      div(
        class = "card-panel",
        h4("Activity by Behavior", style = "margin-top: 0; color: #2c3e50;"),
        plotOutput("activity_plot", height = 450)
      )
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    df <- squirrels
    if (input$fur_color != "All") df <- df |> filter(primary_fur_color == input$fur_color)
    if (input$shift != "All") df <- df |> filter(shift == input$shift)
    if (input$age != "All") df <- df |> filter(age == input$age)
    if (input$location != "All") df <- df |> filter(location == input$location)
    df
  })

  summary_data <- reactive({
    filtered() |>
      count(primary_fur_color, shift, name = "n")
  })

  output$total_count <- renderText({
    n <- nrow(filtered())
    paste0(n, " squirrel", if (n != 1) "s" else "", " total")
  })

  output$activity_plot <- renderPlot({
    df <- filtered()
    cols <- c("running", "chasing", "climbing", "eating", "foraging")
    counts <- sapply(cols, function(c) sum(df[[c]] == "True", na.rm = TRUE))
    plot_df <- data.frame(activity = factor(cols, levels = rev(cols)), count = counts)

    ggplot(plot_df, aes(x = activity, y = count, fill = activity)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("#3498db", "#2ecc71", "#9b59b6", "#e74c3c", "#f39c12"), guide = "none") +
      labs(x = NULL, y = "Count") +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "#2c3e50")
      )
  })

  output$summary_table <- renderTable({
    summary_data() |> head(15)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$download_summary <- downloadHandler(
    filename = function() paste0("squirrel_summary_", Sys.Date(), ".csv"),
    content = function(file) write.csv(summary_data(), file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)
