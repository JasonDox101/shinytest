library(shiny)
library(DT)
library(plotly)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny测试应用"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "选择数据集:",
                  choices = c("mtcars", "iris", "faithful")),
      
      conditionalPanel(
        condition = "input.dataset == 'mtcars'",
        selectInput("x_var", "X轴变量:",
                    choices = names(mtcars))
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'iris'",
        selectInput("species", "选择品种:",
                    choices = c("全部" = "all", 
                               "山鸢尾" = "setosa",
                               "变色鸢尾" = "versicolor", 
                               "维吉尼亚鸢尾" = "virginica"))
      ),
      
      numericInput("obs", "观测数量:", value = 10, min = 1, max = 100),
      
      actionButton("update", "更新数据", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("数据表", DT::dataTableOutput("table")),
        tabPanel("图表", plotlyOutput("plot")),
        tabPanel("摘要", verbatimTextOutput("summary"))
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
    
    # 检查数据是否为空
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty())
    }
    
    # 根据数据集类型创建图表
    if (input$dataset == "mtcars" && !is.null(input$x_var) && input$x_var %in% names(df)) {
      p <- ggplot(df, aes_string(x = input$x_var, y = "mpg")) +
        geom_point() +
        theme_minimal() +
        labs(title = "汽车数据散点图")
    } else if (input$dataset == "iris" && "Sepal.Length" %in% names(df) && "Sepal.Width" %in% names(df)) {
      p <- ggplot(df, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
        geom_point(size = 3) +
        theme_minimal() +
        labs(title = "鸢尾花数据散点图",
             x = "花萼长度 (cm)",
             y = "花萼宽度 (cm)",
             color = "品种")
    } else if (input$dataset == "faithful" && "waiting" %in% names(df) && "eruptions" %in% names(df)) {
      p <- ggplot(df, aes(x = waiting, y = eruptions)) +
        geom_point() +
        theme_minimal() +
        labs(title = "老忠实泉数据散点图",
             x = "等待时间 (分钟)",
             y = "喷发持续时间 (分钟)")
    } else {
      # 如果列名不匹配，返回空图表
      return(plotly_empty())
    }
    
    ggplotly(p)
  })
  
  # Summary output
  output$summary <- renderPrint({
    df <- data()
    if (is.null(df) || nrow(df) == 0) {
      cat("没有数据可显示")
    } else {
      summary(df)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)