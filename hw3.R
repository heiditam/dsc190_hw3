# read data
data <- read.table("hcmv.txt", header = TRUE)
head(data)

N <- 200000
gene <- seq(1, N)
print(gene)

# QUESTION 1
library(lattice)
N <- 229354
n <- 296
gene <- seq(1, N)

#first iteration
set.seed(10)
random_1 <- as.vector(sample.int(N, size=n, replace=FALSE)) # locations uniformly randomly generated
random_1

#second iteration
set.seed(100)
random_2 <- as.vector(sample.int(N, size=n, replace=FALSE)) # locations uniformly randomly generated
random_2

#third iteration
set.seed(1000)
random_3 <- as.vector(sample.int(N, size=n, replace=FALSE)) # locations uniformly randomly generated
random_3

# erm not working
trunc = 1000
lvls = factor(c(0:(trunc-1),paste(">=",trunc,sep="")),levels=c(0:(trunc-1),paste(">=",trunc,sep="")))
random_1_trunc = c(random_1[1:trunc], sum(random_1[-(1:trunc)]))


# QUESTION 2

# for plotting randomly chosen sites on a line
stripplot(site.random, pch=16, cex=0.25) # 1-D Scatter plot
