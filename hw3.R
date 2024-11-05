# read data
data <- read.table("hcmv.txt", header = TRUE)
head(data)


# QUESTION 1
library(lattice)
N <- 200000
n <- 300
gene <- seq(1, N)
set.seed(100)
site.random <- sample.int(N, size=n, replace=FALSE) # locations uniformly randomly generated
norm.quant <- seq(-3, 3, length.out=N)
set.seed(100)
site.norm <- sample.int(N, size=n, prob=dnorm(norm.quant), replace=FALSE) # site locations generated according to normal distribution
set.seed(100)
gene.ind <- 100000:103000
gene.weight <- rep(1, N)
gene.weight[gene.ind] <- 50
set.seed(100)
site.approxrandom <- sample.int(N, size=n, prob=gene.weight, replace=FALSE) # close to uniformly randomly