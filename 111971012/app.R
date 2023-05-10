library(shiny)
library(shinythemes)
library(plotly)
library(FactoMineR)
library(factoextra)
library(ggplot2)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  navbarPage("PCA and CA analysis",
             
             # 第一頁：PCA
             tabPanel("PCA",
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("pc1", "x axis:", choices = c("PC1", "PC2", "PC3", "PC4"),selected = "PC1"),
                          selectInput("pc2", "y axis:", choices = c("PC1", "PC2", "PC3", "PC4"),selected = "PC2")
                        ),
                        
                        mainPanel(
                          plotlyOutput("pcaPlot")
                        )
                      )
                      
             ),
             
             # 第二頁：CA
             tabPanel("CA",
                      
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("dim1", "Select Dim.1", choices = c("Dim.1", "Dim.2", "Dim.3"), selected = "Dim.1"),
                          selectInput("dim2", "Select Dim.2", choices = c("Dim.1", "Dim.2", "Dim.3"), selected = "Dim.2")
                        ),
                        
                        mainPanel(
                          plotlyOutput("caPlot")
                        )
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
    # 將 iris dataset 中的 Species 欄位轉換成類別變數
    iris$Species <- as.factor(iris$Species)
    
    # 執行 CA
    ca <- CA(iris[, 1:4], graph = FALSE)

    # 將 CA 結果視覺化
    plot.CA(ca, col.col = "blue", col.row = iris$Species)
  })
  
}

shinyApp(ui, server)