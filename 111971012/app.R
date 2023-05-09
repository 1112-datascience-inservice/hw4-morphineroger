library(shiny)
library(plotly)
library(FactoMineR)
library(factoextra)
library(dplyr)

ui <- fluidPage(
  titlePanel("Iris PCA & CA"),
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("PCA",
                 selectInput("pc1", "Select PC1", choices = c("PC1", "PC2", "PC3", "PC4"),selected = "PC1"),
                 selectInput("pc2", "Select PC2", choices = c("PC1", "PC2", "PC3", "PC4"),selected = "PC2")
        ),
        tabPanel("CA")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PCA Plot", plotlyOutput("pcaPlot")),
        tabPanel("CA Plot", plotlyOutput("caPlot"))
      )
    )
  )
)

server <- function(input, output) {
  # PCA 圖表
  output$pcaPlot <- renderPlotly({
    pca_data <- iris[, 1:4]
    pca_data <- scale(pca_data)
    pca_res <- prcomp(pca_data, center = TRUE, scale. = TRUE)
    pca_df <- as.data.frame(pca_res$x)
    pca_df$species <- iris$Species
    
    p <- ggplot(pca_df, aes_string(x = input$pc1, y = input$pc2, color = "species")) +
      geom_point(size = 1) +
      scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
      labs(title = "PCA", x = input$pc1, y = input$pc2)
    
    ggplotly(p)
  })
  
  # CA 圖表
  output$caPlot <- renderPlotly({
    ca_data <- iris[,1:4]
    ca_data <- scale(ca_data)
    ca_res <- CA(ca_data)
    ca_df <- as.data.frame(ca_res$ind$coord)
    ca_df$species <- iris$Species
    
    p <- ggplot(ca_df, aes_string(x = "Dim.1", y = "Dim.2", color = "species")) +
      geom_point(size = 3) +
      scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
      labs(title = "Correspondence Analysis", x = "Dimension 1", y = "Dimension 2")
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)