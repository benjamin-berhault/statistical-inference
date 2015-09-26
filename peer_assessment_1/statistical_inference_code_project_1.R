## clear environment variable
rm(list=ls(all=TRUE))

# load necessary library
library(ggplot2)

### set constants ###
# rate parameter for the exponential distribution function : rexp()
# It's the inverse of the expected duration
lambda <- 0.2 # lambda for rexp
n <- 40 # number of exponetials
numberOfSimulations <- 1000 # number of tests

# set the seed for reproducability
set.seed(1982)

# rexp() here generates 1000*40 values according to the exponential 
# distribution with 0.2 as rate parameter
# matrix() put them in one table of 1000 rows (num_sim) and 
# 40 columns (sample_size)
exponentialDistriMatrix <- matrix(data=rexp(n * numberOfSimulations, lambda), nrow=numberOfSimulations)
# compute the mean for each row
exponentialDistriMeans <- data.frame(means=apply(exponentialDistriMatrix, 1, mean))

# plot the means
ggplot(data = exponentialDistriMeans, aes(x = means)) + 
  geom_histogram(binwidth=0.1) +   
  scale_x_continuous(breaks=round(seq(min(exponentialDistriMeans$means), max(exponentialDistriMeans$means), by=1)))

# Expected mean 
mu <- 1/lambda
mu

# Average sample mean of our 1000 samples
meanOfMeans <- mean(exponentialDistriMeans$means)
meanOfMeans

# Expected standard deviation
standard_deviation_theory <- 1/lambda/sqrt(n)
standard_deviation_theory

# Standard deviation of the samples means
standard_deviation_dist <- sd(exponentialDistriMeans$means)
standard_deviation_dist

# Theorical variance
variance_theory <- standard_deviation_theory^2
variance_theory

# Variance of the average samples means
variance_dist <- var(exponentialDistriMeans$means)
variance_dist

# plot the means
ggplot(data = exponentialDistriMeans, aes(x = means)) + 
  geom_histogram(binwidth=0.1, aes(y=..density..), alpha=0.2) + 
  stat_function(fun = dnorm, arg = list(mean = mu , sd = standard_deviation_theory), colour = "red", size=1) + 
  geom_vline(xintercept = mu, size=1, colour="#FF0000") + 
  geom_density(colour="blue", size=1) +
  geom_vline(xintercept = meanOfMeans, size=1, colour="#0000FF") +  
  scale_x_continuous(breaks=seq(mu-3,mu+3,1), limits=c(mu-3,mu+3)) 


# QQ-plot below suggests also the normality.
qqnorm(exponentialDistriMeans$means)
qqline(exponentialDistriMeans$means)
