# read data
data <- read.table("hcmv.txt", header = TRUE)
head(data)
length(data)

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

# QUESTION 3

DNA <- 1:N
# INVESTIGATE REGIONS OF LENGTH 1000
split_DNA <- split(DNA, ceiling(seq_along(DNA) / 1000))
# function to count number of palindromes per region (observed counts)
count_per_region <- function(region, palindrome_locations){
  sum(region %in% data$location)
}
counts_1000 <- sapply(split_DNA, count_per_region, data$location)

# number of palindromes we would expect for each region
expected <- 296 / 230 # total palindromes / # chunks
expected_vec <- rep(expected, 230)
expected_prob <- expected_vec / sum(expected_vec)

# perform a chi-squared test to compare observed counts with expected values
chi_squared_test <- chisq.test(counts_1000, p = expected_prob)

split_DNA_5k <- split(DNA, ceiling(seq_along(DNA) / 5000))
counts_5000 <- sapply(split_DNA_5k, count_per_region, data$location)
expected_5k <- 296 / 46 # total palindromes / # chunks

expected_vec_5k <- rep(expected_5k, 46)
expected_prob_5k <- expected_vec_5k / sum(expected_vec_5k)
chi_squared_test_5k <- chisq.test(counts_5000, p = expected_prob_5k)

split_DNA_10k <- split(DNA, ceiling(seq_along(DNA) / 10000))
counts_10000 <- sapply(split_DNA_10k, count_per_region, data$location)
expected_10k <- 296 / 23 # All conditions for the chi-squared test have been satisfied, described previously.
expected_vec_10k <- rep(expected_10k, 23)
expected_prob_10k <- expected_vec_10k / sum(expected_vec_10k)
chi_squared_test_10k <- chisq.test(counts_10000, p = expected_prob_10k)

library(ggplot2)
library(forcats)
# 
normalized_1000 <- counts_1000 / 1000 * 1000
normalized_5000 <- counts_5000 / 5000 * 1000
normalized_10000 <- counts_10000 / 10000 * 1000

palindrome_counts <- data.frame(
  RegionLength = factor(rep(c('normalized_1000', 'normalized_5000', 'normalized_10000'),
                            times = c(length(normalized_1000), length(normalized_5000), length(normalized_10000)))),
  Count = c(normalized_1000, normalized_5000, normalized_10000)
)
# reorder DataFrame
palindrome_counts$RegionLength <- factor(palindrome_counts$RegionLength, levels = c('normalized_1000', 'normalized_5000', 'normalized_10000'))

ggplot(palindrome_counts, aes(x = RegionLength, y = Count, fill = RegionLength)) + 
  geom_boxplot() + ylab('Density of Palindromes per 1,000 Base Pairs') + 
  labs(title = 'Comparison of Palindrome Density for DNA Regions of Varying Lengths')

# QUESTION 4
library(ggplot2)


ggplot(palindrome_counts, aes(x = RegionLength, y = Count, fill = RegionLength)) + 
  geom_boxplot() + ylab('Density of Palindromes per 1,000 Base Pairs') + 
  labs(title = 'Comparison of Palindrome Density for DNA Regions of Varying Lengths')

