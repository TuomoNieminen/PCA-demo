library(shiny)
library(scatterplot3d)
source("newbiplot.R")

# Server
# -----

server <- function(input, output) {
  data2d <- reactiveValues(X = matrix(c(2:9, 2:9 + rnorm(8)), ncol = 2))
  
  x <- rep(c(1,4,8), times = 30)
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
    plot(data2d$X, main = "(click to change)", ylim = c(0,11), xlim = c(0,11),
         xlab = "", ylab = "", cex = 2.0, pch = 20, col = "dodgerblue3")
  })
  
  # plot 2d pca rotation
  output$pca2d <- renderPlot({
    par(mar = c(4,4,3,1))
    pca <- prcomp(data2d$X, retx = F, center = T, scale = T)
    newdata <- data2d$X %*% pca$rotation
    eigenv <- pca$sdev**2
    var_explained <- paste0("(",round(100*(eigenv/sum(eigenv)), 1),"%)")
    colnames(newdata) <- paste(colnames(newdata), var_explained)
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
    
    pca <- prcomp(data3d$X, retx = F, center = T, scale = T)
    newdata <- data3d$X %*% pca$rotation
    eigenv <- pca$sdev**2
    var_explained <- paste0("(",round(100*(eigenv/sum(eigenv)), 1),"%)")
    colnames(newdata) <- paste(colnames(newdata), var_explained)
    
    plot(newdata[,1:2], ylim = c(-5, 5), cex = 2.0, pch = 20, col = "darkorchid3")
  })
  
  # table of iris data
  output$iris <- renderDataTable({
    iris[sample(nrow(iris)),]
  }, options = list(pageLength = 5))
  
  # iris PCA
  output$pca4d <- renderPlot({
    M <- as.matrix(iris[-5])
    pca <- prcomp(M, retx = T, center = T, scale = T)
    if(input$color) color <- iris$Species
    else color <- 1
    newbiplot(pca, col = color, show_arrows = input$arrows)
  })
}