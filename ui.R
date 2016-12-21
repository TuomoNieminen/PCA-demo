# UI
# --

ui <- fluidPage(tags$style(type="text/css", "body {background-color: #FFFAFB; padding: 60px 0px 40px 0px;}"),
                
                navbarPage("PCA", position = "fixed-top",
                           
                           # 2d example
                           tabPanel("2 dimension",
                                    h2("2D example of PCA", align = "center"),
                                    hr(),
                                    fluidRow(
                                      column(6,
                                             h3("Original data", align = "center"),
                                             plotOutput("scatter2d", click = "plot_click")
                                      ),
                                      column(6,
                                             h3("PCA projected data", align = "center"),
                                             plotOutput("pca2d"), br()
                                      ))),
                           
                           # 3d example
                           tabPanel("3 dimensions",
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
                                    )),
                           # 4d example with Iris
                           tabPanel("4 dimensions (Iris)",
                                    h2("4D example of PCA with Iris", align = "center"),
                                    hr(),
                                    h3("Iris data", align = "center"),
                                    div(dataTableOutput("iris"), style = "padding: 0px 75px 0px 75px;"),
                                    
                                    h3("PCA projected Iris data", align = "center"),
                                    fluidRow(
                                      column(8, plotOutput("pca4d")),
                                      column(4, 
                                             br(),br(), br(),
                                             checkboxInput("color", "color by species"),
                                             checkboxInput("arrows", "arrows of original variables")
                                      ))
                           ))
)