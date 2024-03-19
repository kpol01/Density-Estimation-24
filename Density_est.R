rm(list = ls())


library(ggplot2)
library(gridExtra)
library(cowplot)

#varying sample


hist_sam_size1 <- function(n, rpdf, dens, alpha)      #if the simulating distribution has single argument
{
  set.seed(seed = 1)
  s <- rpdf(n, alpha)
  x <- seq(0, max(s), 0.001)
  df <- data.frame(s)
  
  density <- dens(x, alpha)
  df1 <- data.frame(x, density)
  fig <- ggplot() + geom_histogram(data = df, aes(x = s, y = after_stat(density)))  + 
    geom_line(data = df1, aes(x = x, y = density)) +
    labs(title = paste("n = ", n))
  return (fig)
}

hist_sam_size2 <- function(n, rpdf, dens, alpha, beta)      #if the simulating distribution has two arguments
{
  set.seed(seed = 1)
  s <- rpdf(n, alpha, beta)
  x <- seq(0, max(s), 0.001)
  df <- data.frame(s)
  
  density <- dens(x, alpha, beta)
  df1 <- data.frame(x, density)
  fig <- ggplot() + geom_histogram(data = df, aes(x = s, y = after_stat(density)))  + 
    geom_line(data = df1, aes(x = x, y = density)) +
    labs(title = paste("n = ", n))
  return (fig)
}
hist_sam_size(100, rbeta, dbeta, 5, 2)


hist_comp_size1 <- function(n, rpdf, dens, alpha)
{
  plots <- list()
  for(i in (1:length(n)))
  {
    plots[[i]] <- hist_sam_size1(n[i], rpdf, dens, alpha)
  }

  
  grid.arrange(grobs = plots, ncol = round(length(n)/2))
}


hist_comp_size2 <- function(n, rpdf, dens, alpha, beta)
{
  plots <- list()
  for(i in (1:length(n)))
  {
    plots[[i]] <- hist_sam_size2(n[i], rpdf, dens, alpha, beta)
  }
  
  
  grid.arrange(grobs = plots, ncol = round(length(n)/2))
}

#function to generate obs from bimodal-normal
rbi.norm <- function(n)
{
  s <- runif(n, 0, 1)
  for(i in (1:n))
  {
    if(s[i] < 0.5)
    {
      x[i] <- qnorm(s[i] * 2 * pnorm(1), -1, 1)
    }
    else{
      x[i] <- -1 * qnorm((1 - s[i]) * 2 * pnorm(1), -1, 1)
    }
  }
  return(x)
}

dbi.norm <- function(x)
{
  f.x <- 0
  for(i in (1:length(x)))
  {
    if(x[i] < 0)
    {
      f.x[i] <- dnorm(x[i], -1, 1) / (2 * pnorm(1))
    }
    else
    {
      f.x[i] <- dnorm(x[i], 1, 1) / (2 * pnorm(1))
    }
  }
  return(f.x)
}

hist_sam_size_bimod <- function(n)
{
  set.seed(seed = 1)
  s <- rbi.norm(n)
  x <- seq(min(s), max(s), 0.001)
  df <- data.frame(s)
  
  density <- dbi.norm(x)
  df1 <- data.frame(x, density)
  fig <- ggplot() + geom_histogram(data = df, aes(x = s, y = after_stat(density)))  + 
    geom_line(data = df1, aes(x = x, y = density)) +
    labs(title = paste("n = ", n))
  return (fig)
}

hist_comp_size_bimod <- function(n)
{
  plots <- list()
  for(i in (1:length(n)))
  {
    plots[[i]] <- hist_sam_size_bimod(n[i])
  }
  
  
  grid.arrange(grobs = plots, ncol = round(length(n)/2))
}
