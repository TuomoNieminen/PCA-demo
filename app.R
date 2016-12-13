library(shiny)
library(scatterplot3d)
ui <- fluidPage(style="background-color: #FCF7FC",
  
  h2("2D example of PCA", align = "center"),
  hr(),
  fluidRow(
    column(6,
           h3("Original data", align = "center"),
           plotOutput("scatter2d", click = "plot_click")
    ),
    column(6,
           h3("PCA projected data", align = "center"),
           plotOutput("pca2d")
    )),
  br(),br(),
  
  h2("3D example of PCA", align = "center"),
  hr(),
  fluidRow(
    column(6,
           h3("Original data", align = "center"),
           plotOutput("scatter3d"),
           div(actionButton("update", label = "Update", icon = icon("circle-o-notch")), align="right")
    ),
    column(6,
           h3("PCA projected data", align = "center"),
           plotOutput("pca3d")
    )
  ),
  br()
)
server <- function(input, output) {
  data2d <- reactiveValues(X = matrix(c(1:10, 1:10 + rnorm(10)), ncol = 2))

  x <- rep(1:3, times = 30)
  data3d <- reactiveValues(X = matrix(c(x, x + rnorm(length(x)), x + rnorm(length(x))), ncol = 3))

  # 2d interactive data
  update_2d_data <- observe({
    x <- input$plot_click$x
    i <- which.min(abs(data2d$X[,1] - x))
    data2d$X[i,1] <- x
    data2d$X[i,2] <- input$plot_click$y
  })
  
  # 3d interactive data
  update_3d_data <- observeEvent(input$update,{
    data3d$X <- matrix(c(x, x + rnorm(length(x)), x + rnorm(length(x))), ncol = 3)
  })
  
  # plot original 2d data
  output$scatter2d <- renderPlot({
    par(mar = c(4,4,3,1), cex.main = 1, font.main = 1)
    plot(data2d$X, main = "(click to change)", 
         xlab = "", ylab = "", cex = 2.0, pch = 20, col = "dodgerblue3")
  })
  
  # plot 2d pca rotation
  output$pca2d <- renderPlot({
    par(mar = c(4,4,3,1))
    pca <- prcomp(data2d$X, retx = F, center = T, scale = T)
    newdata <- data2d$X %*% pca$rotation
    plot(newdata, ylim = c(-5,5), cex = 2.0, pch = 20, col = "darkorchid3")
  })
  
  # plot original 3d data
  output$scatter3d <- renderPlot({
    scatterplot3d(data3d$X, cex.symbols = 2.0, pch = 20, color = "dodgerblue3",
                  xlab = "", ylab = "", zlab = "")
  })
  
  # plot pca rotated 3d data
  output$pca3d <- renderPlot({
    par(mar = c(4,4,3,1))
    pca <- prcomp(data3d$X, retx = T, center = T, scale = T)
    newdata <- data3d$X %*% pca$rotation
    plot(newdata, ylim = c(-5,5), cex = 2.0, pch = 20, col = "darkorchid3")
  })
}
shinyApp(ui = ui, server = server)