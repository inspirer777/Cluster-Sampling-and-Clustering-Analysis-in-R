library(survey)
data(api)

# One-Stage Cluster Sampling Specification
api.onestage <- svydesign(id = ~dnum, data = apiclus1, fpc = ~fpc)
summary(api.onestage)

#Two-Stage Cluster Sampling Specification
api.twostage <- svydesign(id = ~dnum + snum, data = apiclus2, fpc = ~fpc1 + 
                            fpc2)
summary(api.twostage)
?data(api)
unique(weights(api.onestage))
?unique

## Link: https://techvidvan.com/tutorials/cluster-analysis-in-r/ 
# another example 
head(iris)
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, col=Species)) + geom_point() + ggtitle("TechVidvan iris scatter plot")


## Now, we can use the kmeans() function to form the clusters
set.seed(55)
cluster.iris <- kmeans(iris[, 3:4], 3, nstart = 20)
cluster.iris

#We can compare the results be forming a table with the species column of the original data.
table(cluster.iris$cluster, iris$Species)

## We can plot the cluster using ggplot. As the accuracy is high, we expect the plot to look very much like the original data plot.
cluster.iris$cluster <- as.factor(cluster.iris$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color=cluster.iris$cluster)) + geom_point() + ggtitle("TechVidvan iris cluster plot")





