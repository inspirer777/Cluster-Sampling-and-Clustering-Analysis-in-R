# Load the survey library for statistical analysis
library(survey)

# Load the API dataset
data(api)

# One-Stage Cluster Sampling Specification
# In this design, clusters (dnum) are used as the cluster identifier, and apiclus1 is the dataset.
# The fpc parameter is used to adjust for the finite population correction.
api.onestage <- svydesign(id = ~dnum, data = apiclus1, fpc = ~fpc)

# Summary of the One-Stage Cluster Sampling design
summary(api.onestage)

# Two-Stage Cluster Sampling Specification
# In this design, both dnum and snum are used as cluster identifiers in two stages.
# fpc1 and fpc2 are used for finite population correction at each stage.
api.twostage <- svydesign(id = ~dnum + snum, data = apiclus2, fpc = ~fpc1 + fpc2)

# Summary of the Two-Stage Cluster Sampling design
summary(api.twostage)

# Explore the api dataset
?data(api)

# Extract unique weights from the one-stage cluster sampling design
unique(weights(api.onestage))

# View documentation for the unique() function
?unique

## Link: https://techvidvan.com/tutorials/cluster-analysis-in-r/
# Another example using the iris dataset:
head(iris)

# Load the ggplot2 library for graphical plots
library(ggplot2)

# Create a scatter plot to visualize Petal Length vs. Petal Width, colored by Species
ggplot(iris, aes(Petal.Length, Petal.Width, col=Species)) + 
  geom_point() + 
  ggtitle("TechVidvan Iris Scatter Plot")

# Now we can use the kmeans() function to perform clustering
# We are clustering based on Petal.Length and Petal.Width, with 3 clusters and nstart=20 for better results
set.seed(55)
cluster.iris <- kmeans(iris[, 3:4], 3, nstart = 20)

# View the clustering results
cluster.iris

# Compare the clustering results with a table showing the cluster numbers vs. original species
table(cluster.iris$cluster, iris$Species)

# Create a clustered plot using ggplot
# The clusters are displayed using different colors, and the plot should resemble the original data plot due to high accuracy
cluster.iris$cluster <- as.factor(cluster.iris$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color=cluster.iris$cluster)) + 
  geom_point() + 
  ggtitle("TechVidvan Iris Cluster Plot")
