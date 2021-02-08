#https://rpubs.com/GMortensen/KMeans
library(ggplot2)   #(2)
library(caret)   #(3)
library(rpart)  #(4)
library(rpart.plot)  #(5)


tradingData <- read.csv("/Users/nselvarajan/Desktop/R/finals/datasets1/NYSE_DM.csv", header = TRUE)
tradingData<-tradingData[1:33333,]

str(tradingData)
head(tradingData, n=5)


#drop ID column
tradingData <- tradingData[, -c(1,3,4,5,6,7)]
colnames(tradingData) <- c("OPEN_P","CHANGE")



clusters.sum.squares <- rep(0.0, 14)
cluster.params <- 1:14
#set.seed(893247)
for (i in cluster.params) {
  kmeans.temp <- kmeans(tradingData, centers = i, iter.max = 10000)
  clusters.sum.squares[i - 1] <- sum(kmeans.temp$withinss)
}   


ggplot(NULL, aes(x = cluster.params, y = clusters.sum.squares)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "Cluster Sum of Squared Distances",
       title = "Trading Training Data Scree Plot")


kmeanstest.temp <- kmeans(tradingData, centers = 7, iter.max = 10000)
kmeanstest.temp$size # gives no. of records in each cluster
kmeanstest.temp$centers # gives value of cluster center datapoint value(3 centers for k=3)
kmeanstest.temp$cluster #gives cluster vector showing the custer where each record falls

summary(kmeanstest.temp)
kmeanstest.temp$centers
View(kmeanstest.temp$centers)


plot(tradingData, col=kmeanstest.temp$cluster)
# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters


tradingDataPlot <- tradingData
tradingDataPlot <- cbind(tradingDataPlot, cluster = as.factor(kmeanstest.temp$cluster))
tradingDataPlot$cluster=factor(tradingDataPlot$cluster)
centers=as.data.frame(kmeanstest.temp$centers)

library(ggplot2)

ggplot() +
  geom_point(data = tradingDataPlot, 
             mapping = aes(x = OPEN_P, 
                           y = CHANGE, 
                           colour = cluster)) +
  geom_point(mapping = aes_string(x = kmeanstest.temp$centers[, "OPEN_P"], 
                                  y = kmeanstest.temp$centers[, "CHANGE"]),
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = kmeanstest.temp$centers[, "OPEN_P"], 
                                 y = kmeanstest.temp$centers[, "CHANGE"],
                                 label = 1:7),
            color = "black", size = 4) +
  scale_x_continuous(limits = c(0,160))+
  scale_y_continuous(limits = c(-100, 200))

  theme_light()

