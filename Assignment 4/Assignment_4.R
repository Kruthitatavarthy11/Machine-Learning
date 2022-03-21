library(readr)
library(dplyr)
library(caret)
library(factoextra)
library(tidyverse)
library(cluster)


pharmacy_data <- read.csv('C:/Users/Kruthi Tatavarthy/Desktop/Machine-Learning/Assignment 4/Pharmaceuticals.csv')

head(pharmacy_data)

colSums(is.na(pharmacy_data)) # verify sum of the null columns in the Pharmacy data 

#Question a. Use only the numerical variables (1 to 9) to cluster the 21 firms. Justify the various choices made in
#conducting the cluster analysis, such as weights for different variables, the specific clustering algorithm(s)

pharmacy_data_numerical <- pharmacy_data[,c(3:11)] #numerical variable from 3 to 11

#Normalization 

normal_data <- as.data.frame(scale(pharmacy_data_numerical))
distance <- get_dist(normal_data)
fviz_dist(distance) #visuvalize a distance matrix

# Estimating the  number of clusters
# Elbow Method is used in scaleing the data to determine the value of k

fviz_nbclust(normal_data, FUNcluster = kmeans, method = "wss") + labs(subtitle = "Elbow Method")

# Silhouette Method is used in scaling thw data to determine the number of clusters
fviz_nbclust(normal_data,FUNcluster = kmeans,method = "silhouette")+labs(subtitle="Silhouette Method")

#data clusters

set.seed(12335)
k5 <- kmeans(normal_data, center = 5, nstart = 25) # where k = 5
k5$centers #centriods
fviz_cluster(k5, data = normal_data) #cluster plot viz

k5$size

#K-Means Cluster Analysis - Fitting the data with 5 clusters

data_fitting <- kmeans(normal_data, 5)
aggregate(normal_data, by = list(data_fitting$cluster), FUN = mean)
norm_data <- as.data.frame(normal_data, data_fitting$cluster)
norm_data

#(b)Interpret the clusters with respect to the numerical variables used in forming the clusters

#cluster 1 - Row 8, 6, 12
#cluster 2 - Row 2, 18
#cluster 3 - Row 5, 9, 14, 20
#cluster 4 - Row 3, 4, 7, 10, 16, 19, 21
#cluster 5 - Row 11, 13, 15, 17


#After executing the function::  aggregate(normal_data, by = list(data_fit$cluster), FUN = mean), 
#following are the observations:
  
#cluster 1 has highest Market_Cap, highest ROE, highest ROA, lowest Leverage and lowest Beta
#cluster 2 has lowest Beta, lowest PE_Ratio
#cluster 3 has lowest Market_Cap, highest Beta, highest Leverage, highest Rev_Growth, lowest PE_ratio
#cluster 4 has highest PE_Ratio, lowest ROE, lowest ROA, lowest Net_Profit_Margin
#cluster 5 has highest Asset_Turnover, lowest Revenue growth, highest Net_Profit_Margin


#cluster plot

clusplot(normal_data, data_fitting$cluster, color = TRUE, shade =TRUE, labels = 2, lines = 0)

#(c)Is there a pattern in the clusters with respect to the numerical variables (10 to 12)? 

#Moderate buy, hold, strong buy recommendations

#Cluster 1 has highest ROE, highest ROA, highest Market_Cap but Rev_Growth is not indicated to moderate sell 

#Cluster 2 has lowest Beta, lowest Asset_Turnover so hold Recommendation

#Cluster 3 has highest Beta, highest Leverage, highest Rev_Growth is strong to buy Recommendation

#Cluster 4 has highest PE_Ratio, lowest ROE, ROA, Net_Profit_Margin is to hold buy Recommendation

#Cluster 5 has highest Asset_Turnover, highest Net_Profit_Margin, lowest revenue growth is risky but to buy Recommendation

#Cluster 5 and Cluster 3 moderate to buy Recommendation

#Cluster 1,4 is hold Recommendation


# (d)Provide an appropriate name for each cluster using any or all of the variables in the dataset.


#Cluster 1 - highest Market_Cap,highest Leverage,highest Rev_Growth, lowest Leverage and Beta cluster -  Risky but high revenue

#Cluster 2 - lowest Rev_Growth,lowest PE_Ratio cluster - on hold 

#Cluster 3 - lowest PE_Ratio,lowest_ROE,lowest ROA, highest Leverage, highest Rev_growth, lowest Net_Profit_Margin cluster - moderate buy recomendation

#Cluster 4 - highest PE_Ratio, lowest ROA,lowest Asset_Turnover, lowest Net_Profit_Margin cluster - - hold recommendation

#Cluster 5 - highest Asset_Turnover, Net_Profit_Margin, lowest Rev_Growth cluster - strong buy 