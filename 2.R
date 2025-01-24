######   Calculating Mean and Variance in Sampling from a Population
#### Page 231

# Total number of samples
N = 5

# Sum of data points
sigmaaz1taN = 3/2 + 2 + 1 + 2 + 3/2

# Calculate the mean
meanjamee = 1/N * (sigmaaz1taN)
meanjamee

# Recalculate the mean using a different variable
meanYIha = 1/N * (sigmaaz1taN)
meanYIha

# Calculate the variance between observations
Yijha = (4-2)^2 + (5-2)^2 + (0-2)^2 + ((1-2)^2) * 3
M = 6  # Sample size (number of elements in the cluster)
S2i = 1/(M-1) * (Yijha)  # Sample variance within a cluster
S2i

# Calculate the average of sample variances across clusters
meanS2iha = (27/10 + 16/5 + 4/5 + 4 + 3/2)
Sbar2w = 1/N * (meanS2iha)
Sbar2w

# Between-cluster variance calculation
m = meanjamee
sigmaha <- c((2-m)^2, (1-m)^2, (3/2-m)^2, (2-m)^2, (3/2-m)^2)
s2bb = 1/(N-1) * sigmaha
s2b = sum(s2bb)
s2b

# Calculate the overall variance using the squared differences from the mean
vahedha <- c(0, 1, 1, 3, 4, 0, 1, 1, 3, 5, 0, 2, 2, 1, 0, 0, 1, 2, 4, 5, 
             0, 1, 1, 1, 3, 2, 1, 2, 0, 1)
YI2 = sum(vahedha^2)
sigmaYi_YBAR = YI2 - (N * M) * (meanjamee)^2
S2 = 1/(N * M - 1) * (sigmaYi_YBAR)
S2

# Recalculate S2 (total variance) using an adjustment formula
M = 6
N = 5
new_s2 = (M * (N-1) * s2b + N * (M-1) * Sbar2w) * 1/(N * M - 1)
new_s2


## Example 4.5 from Page 243

# Calculate the mean of grouped data (Ybar)
Ybar = (1/(5*50) * (96 + 99.2 + 100.8 + 101 + 99.5))
print(Ybar)

# Calculate the between-cluster variance (s2b) manually
s2b = (1/4 * ((96/50-1.986)^2) + ((99.2/50-1.986)^2) + 
       ((100.8/50-1.986)^2) + ((101/50-1.986)^2) + ((99.5/50-1.986)^2))
print(s2b)

# A faster method for calculating s2b using the previously calculated mean
s2bb = (1/4 * ((96/50-Ybar)^2) + ((99.2/50-Ybar)^2) + 
        ((100.8/50-Ybar)^2) + ((101/50-Ybar)^2) + ((99.5/50-Ybar)^2))
print(s2bb)

# Calculate the variance of the mean (V(Ybar))
Yhadn = (1/5 - 1/200) * s2b
Yhadn

# Within-cluster variance
M = 50
s2w = M / (5*49) * (0.8 + 1 + 0.6 + 0.8 + 0.7)
s2w

# Total variance estimation (shad2)
N = 200
shad2 = (M * (N-1) * s2b + N * (M-1) * s2w) / (M * N - 1)
shad2

# Adjusted between-cluster variance
sigma2b = ((N-1)/N) * s2b
sigma2b

# Adjusted total variance
sigmahad2 = (M * N - 1) / (M * N) * (shad2)
sigmahad2

# Calculate the ratio of cluster to total variance (Roc)
m = 5
Roc = ((M * sigma2b / sigmahad2) - 1 * (1/m - 1))
Roc


## Example 7.5 from Page 253

# Define data for multiple clusters
N = 20
M0 = 200
n = 50 
M1 = 10
M2 = 8
M3 = 12
M4 = 8
M5 = 15

# Calculate mean for each cluster
YB10 = 80/10
YB10
YB20 = 64/8
YB20
YB30 = 90/12
YB30
YB40 = 50/8
YB40
YB50 = 72/15
YB50

# Calculate the overall cluster mean
MBAR = M0 / 20
MBAR

# Weighted sum of means
A <- c(M1 * YB10, M2 * YB20, M3 * YB30, M4 * YB40, M5 * YB50)
A

# Divide by the average cluster size
a = A / MBAR
a

# Calculate the total mean
YBARi = a[1] + a[2] + a[3] + a[4] + a[5]
YBARi
YHADBAR = 1/5 * YBARi
YHADBAR

# Create a table or dataframe with some example data
as.character("Total size of cluster units") <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE, FALSE, TRUE, FALSE)
d <- c(2.5, 8, 10, 7)

# Combine variables into a data frame
df <- data.frame(a, b, c, d)
df

# Example of using the `starwars` dataset (requires dplyr)
df <- starwars %>% 
  select(1:4) %>% 
  convert(fct(name),
          chr(height:mass),
          fct(hair_color)) %>% 
  print()
