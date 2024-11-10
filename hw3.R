# read data
data <- read.table("hcmv.txt", header = TRUE)
head(data)

N <- 200000
gene <- seq(1, N)
print(gene)

# QUESTION 1
num_sim = 100
N = 229354
n = 296
set.seed(10)
simulations <- replicate(num_sim, sample(1:N, n, replace = FALSE), simplify = FALSE)

average_distances <- sapply(simulations, function(simulation){
  sorted_simulation <- sort(simulation) # We sorted the simulation to get correct distances
  distances <- diff(sorted_simulation)
  mean(distances)
})

mean(average_distances)
var(average_distances)

histogram <- hist(average_distances, breaks = 10, col = 'lightblue', main = 'Differences in Simulated Palindrome Distances', xlab = 'Average Distance Between Palindromes')
image(matrix(average_distances, ncol=1), col=heat.colors(max(average_distances)), main="Heatmap of Simulated Palindrome Density")


# QUESTION 2
library(lattice)

simulations <- replicate(num_sim, sample(1:N, n, replace = FALSE), simplify = FALSE)
distances <- sapply(simulations, function(simulation){
  sorted_simulation <- sort(simulation) # We sorted the simulation to get correct distances
  distances <- diff(sorted_simulation) # s1, s2, s3, ...
})

# plotting distances distribution
for (i in 1:ncol(distances)) {
  print(stripplot(distances[,i], pch=16, cex=0.25)) # 1-D Scatter plot
}

# calculating sums of pairs
pairwise_diffs <- sapply(seq(1, ncol(distances) - 1, by = 2), function(i) rowSums(distances[, i:(i+1)]))

# plotting pair distribution
for (i in 1:ncol(pairwise_diffs)) {
  print(stripplot(pairwise_diffs[,i], pch=16, cex=0.25)) # 1-D Scatter plot
}

# calculating sums of triplets
triplet_diffs <- sapply(seq(1, ncol(distances) - 1, by = 3), function(i) rowSums(distances[, i:(i+1)]))

# plotting triplet distribution
for (i in 1:ncol(triplet_diffs)) {
  print(stripplot(triplet_diffs[,i], pch=16, cex=0.25)) # 1-D Scatter plot
}

