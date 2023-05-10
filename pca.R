data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

library(FactoMineR)
data(iris)

# 將 iris dataset 中的 Species 欄位轉換成類別變數
iris$Species <- as.factor(iris$Species)

# 執行 CA
ca <- CA(iris[, 1:4], graph = FALSE)

# 將 CA 結果視覺化
plot.CA(ca, col.col = "black", col.row = ir.species)

install.packages("plotly")
library(ggplot2)
library(plotly)

# 使用iris資料集進行PCA分析
pca <- prcomp(iris[, 1:4], scale = TRUE)

# 將PCA分析結果轉換為資料框
df <- as.data.frame(pca$x)
df$Species <- iris$Species

# 繪製靜態PCA圖
p <- ggplot(df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 1) +
  xlab(paste0("PC1 (", round(pca$sdev[1] / sum(pca$sdev) * 100), "%)")) +
  ylab(paste0("PC2 (", round(pca$sdev[2] / sum(pca$sdev) * 100), "%)"))

# 轉換為具有互動性的PCA圖
ggplotly(p)

library(ca)
library(ggplot2)
library(ggbiplot)
library(shiny)
library(plotly)

# 计算CA分析
iris.ca <- ca(iris[,1:4], graph = FALSE)

# 绘制CA biplot
g <- ggbiplot(iris.ca, obs.scale = 1, var.scale = 1, 
              groups = iris$Species, var.axes = FALSE) + 
  scale_color_discrete(name = '') + theme(legend.direction = 'horizontal', 
                                          legend.position = 'top')

# 绘制CA图表
row_scores <- as.data.frame(iris.ca$row$coord)
col_scores <- as.data.frame(iris.ca$col$coord)
ir.species <- iris[, 5]
ca_data <- data.frame(row_scores, col_scores, Species = ir.species)

ca_plot <- ggplot(ca_data, aes(x = Dim.1, y = Dim.2, color = Species)) +
  geom_point(size = 3) + theme(legend.direction = 'horizontal', legend.position = 'top') +
  labs(title = 'Correspondence Analysis (CA) Biplot', x = 'Dim.1', y = 'Dim.2')

library(FactoMineR)
library(factoextra)
data(iris)

# 將 iris dataset 中的 Species 欄位轉換成類別變數
iris$Species <- as.factor(iris$Species)

# 執行 CA
ca <- CA(iris[, 1:4], graph = FALSE)

# 將 CA 結果視覺化
fviz_ca_biplot(ca, repel = TRUE, col.var = "black")

library(FactoMineR)
data(iris)

# 將 iris dataset 中的 Species 欄位轉換成類別變數
iris$Species <- as.factor(iris$Species)

# 執行 CA
ca <- CA(iris[, 1:4], graph = FALSE)

# 將 CA 結果視覺化
plot.CA(ca, col.col = "blue", col.row = iris$Species)
