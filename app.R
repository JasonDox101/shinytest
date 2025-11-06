library(shiny) 
library(DT)
library(plotly)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny test -test update periodically----123123 copy?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select dataset:",
                  choices = c("mtcars", "iris", "faithful")),
      
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        selectInput("x_var", "X-axis variable:",
                    choices = names(mtcars))
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'iris'",
        selectInput("species", "Select species:",
                    choices = c("All" = "all", 
                               "Setosa" = "setosa",
                               "Versicolor" = "versicolor", 
                               "Virginica" = "virginica"))
      ),
      
      numericInput("obs", "Number of observations:", value = 10, min = 1, max = 100),
      
      actionButton("update", "Update Data", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DT::dataTableOutput("table")),
        tabPanel("Plot", plotlyOutput("plot")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data
  data <- eventReactive(input$update, {
    dataset <- switch(input$dataset,
                     "mtcars" = mtcars,
                     "iris" = iris,
                     "faithful" = faithful)
    
    if (input$dataset == "iris" && !is.null(input$species) && input$species != "all") {
      dataset <- dataset[dataset$Species == input$species, ]
    }
    
    head(dataset, input$obs)
  }, ignoreNULL = FALSE)
  
  # Data table output
  output$table <- DT::renderDataTable({
    data()
  }, options = list(scrollX = TRUE))
  
  # Plot output
  output$plot <- renderPlotly({
    df <- data()
    
    # Check if data is empty
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty())
    }
    
    # Create plots based on dataset type
    if (input$dataset == "mtcars" && !is.null(input$x_var) && input$x_var %in% names(df)) {
      p <- ggplot(df, aes_string(x = input$x_var, y = "mpg")) +
        geom_point() +
        theme_minimal() +
        labs(title = "Car Data Scatter Plot")
    } else if (input$dataset == "iris" && "Sepal.Length" %in% names(df) && "Sepal.Width" %in% names(df)) {
      p <- ggplot(df, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "Iris Data Scatter Plot",
             x = "Sepal Length (cm)",
             y = "Sepal Width (cm)",
             color = "Species")
    } else if (input$dataset == "faithful" && "waiting" %in% names(df) && "eruptions" %in% names(df)) {
      p <- ggplot(df, aes(x = waiting, y = eruptions)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Old Faithful Geyser Data Scatter Plot",
             x = "Waiting Time (minutes)",
             y = "Eruption Duration (minutes)")
    } else {
      # Return empty plot if column names don't match
      return(plotly_empty())
    }
    
    ggplotly(p)
  })
  
  # Summary output
  output$summary <- renderPrint({
    df <- data()
    if (is.null(df) || nrow(df) == 0) {
      cat("No data to display")
    } else {
      summary(df)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)