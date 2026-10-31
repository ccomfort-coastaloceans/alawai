#NEW PCA PLOT
#This one uses lab OB/TR, could use field also

rm(list=ls()) 
mydata <- read.csv("~/UH/Ch2Fluorometer/pca_v3.csv")
enterodata <-read.csv("~/UH/Ch2Fluorometer/enteroscores_v3.csv") #this is groupings based on regulatory thresholds

bottledatapca<-data.frame(mydata)
alawai_pca <- prcomp(bottledatapca, scale.=TRUE)
scores <- as.data.frame(alawai_pca$x)
loadings <- as.data.frame(alawai_pca$rotation)

#add ranking to scores matrix
scores$entero <- enterodata[,c("score")]


library(ggplot2)
library(glue)

ggplot(scores, aes(x = PC1, y = PC2, color=entero)) +
  geom_point(size = 3) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings, aes(x = PC1*5, y = PC2*5, label = rownames(loadings)),
            color = "black", vjust = -0.5) +
  theme_minimal() +
  xlab(paste("PC1 (", round(alawai_pca$sdev[1]^2/sum(alawai_pca$sdev^2)*100, 1), "%)",
             sep = "")) +
  ylab(paste("PC2 (", round(alawai_pca$sdev[2]^2/sum(alawai_pca$sdev^2)*100, 1), "%)", 
             sep = "")) + 
  #stat_ellipse(geom="polygon", alpha=0.01, level=0.9)+
  ggtitle("PCA Biplot with Calibrated Axes")
#scale_fill_manual(values = colors1)


#Groupings based on heirarchical partitioning 
mydata <- read.csv("~/UH/Ch2Fluorometer/pca_v3.csv")

bottledatapca<-data.frame(mydata)

pca_result <- prcomp(bottledatapca, scale.=TRUE)
scores <- as.data.frame(pca_result$x)
loadings <- as.data.frame(pca_result$rotation)

# Calculate the distance matrix
dist_matrix <- dist(pca_result$x)

# Perform hierarchical clustering
hc_result <- hclust(dist_matrix, method="average") #transfer the results of this method to appropriate column in hp doc
# Plot the dendrogram
plot(hc_result)
rect.hclust(hc_result,h=4)

#put results of clustering in the hp file (hierarchical partitioning)
part<-read.csv("~/UH/Ch2Fluorometer/hp_withpca3.csv")
#add area to scores matrix
scores$hp <- part[,c("average")]

ggplot(scores, aes(x = PC1, y = PC2, color=hp)) +
  geom_point(size = 3) +
  scale_colour_discrete(
    palette = NULL,
    aesthetics = "colour",
    na.value = "grey50",
    type = getOption("ggplot2.discrete.colour")
  ) +
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1*5, yend = PC2*5),
               arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  geom_text(data = loadings, aes(x = PC1*5, y = PC2*5, label = rownames(loadings)),
            color = "black", vjust = -0.5) +
  
  theme_minimal() +
  xlab(paste("PC1 (", round(pca_result$sdev[1]^2/sum(pca_result$sdev^2)*100, 1), "%)",
             sep = "")) +
  ylab(paste("PC2 (", round(pca_result$sdev[2]^2/sum(pca_result$sdev^2)*100, 1), "%)", 
             sep = "")) + 
  stat_ellipse(geom="polygon", alpha=0.01, level=0.9)+
  ggtitle("PCA Biplot with Calibrated Axes")
#scale_fill_manual(values = colors1)