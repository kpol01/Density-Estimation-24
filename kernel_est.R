rm(list = ls())
library(dplyr)
library(ggplot2)
x <- rnorm(1000)
df <- data.frame(x)
ggplot(data = df, aes(x = x)) + geom_density(kernel = "rectangular", color = "blue") + 
  geom_density(kernel = "triangular") +
  geom_density(kernel = "epanechnikov", color = "red")

density(x, kernel = "epanechnikov") %>% predict(qnorm(0.95))


kern_est <- function(sample, kernel, new_point)
{
  model <- density(sample, kernel = "epanechnikov")
  est <- approx(model$x, model$y, xout = new_point)
  return(est$y)
  
}
  
  
rel_eff <- function(n, parent, invparent, density, k, quantile)
{
  kern_est = 0
  sam_quant = 0 
  for(i in (1:1000))
  {
    sample <- parent(n)
    
    kern_est[i] <- kern_est(sample, kernel, invparent(quantile))
  }
  return(sum(abs(kern_est - density(invparent(quantile))) < 0.01) / 1000)
}

rel_eff(100000, rnorm, qnorm, dnorm, "epanechnikov", 0.5)


n <- seq(100, 50000, 500)
rel_freq <- 0

for(i in (1:length(n)))
{
  rel_freq[i] <- rel_eff(n[i], rnorm, qnorm, dnorm, "epanechnikov", 0.8)
}
n1 <- n[1:length(rel_freq)]
df <- data.frame(n1, rel_freq)
ggplot(data = df, mapping = aes(x = n1, y = rel_freq)) + geom_point()
df
