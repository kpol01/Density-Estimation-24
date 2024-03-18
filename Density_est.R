rm(list = ls())


library(ggplot2)
library(gridExtra)
library(cowplot)

#varying sample size

hist_sam_size <- function(n, rpdf, dens)
{
  set.seed(seed = 1)
  s <- rpdf(n)
  x <- seq(0, max(s), 0.001)
  df <- data.frame(s)
  
  density <- dens(x)
  df1 <- data.frame(x, density)
  fig <- ggplot() + geom_histogram(data = df, aes(x = s, y = after_stat(density)))  + 
    geom_line(data = df1, aes(x = x, y = density)) +
    labs(title = paste("n = ", n))
  return (fig)
}

hist_sam_size(100, rexp, dexp)

plot_grid(hist_sam_size(100, rexp, dexp), hist_sam_size(200, rexp, dexp), hist_sam_size(500, rexp, dexp), 
          hist_sam_size(1000, rexp, dexp), ncol = 2, nrow = 2)
