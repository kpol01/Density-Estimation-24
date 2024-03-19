rm(list = ls())

library(ggplot2)
library(gridExtra)
library(cowplot)


hist1 <- function(n, x0, h, rpdf, dens, alpha)      #if the simulating distribution has single argument
{
  set.seed(seed = 1)
  x <- rpdf(n, alpha)
  s<- seq(min(x)-h,max(x)+h,0.001)
  ds <- dens(s,alpha)
  fig <- ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),center=x0-h/2, binwidth=h, color="black", fill="white")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds))
  return (fig)
}

hist2 <- function(n, x0, h, rpdf, dens, alpha, beta)      #if the simulating distribution has two arguments
{
  set.seed(seed = 1)
  x <- rpdf(n, alpha, beta)
  s <- seq(min(x)-h,max(x)+h,0.001)
  ds <- dens(s,alpha,beta)
  
  fig <- ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),center=x0-h/2, binwidth=h, color="black", fill="white")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds))
  return (fig)
}


#For varying sample size

hist_comp_size1 <- function(samp, x0, h, rpdf, dens, alpha)
{
  plots <- list()
  for(i in (1:length(samp)))
  {
    plots[[i]] <- hist1(samp[i], x0, h, rpdf, dens, alpha)+labs(title = paste("n = ", samp[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(samp)/2))
}

hist_comp_size2 <- function(samp, x0, h, rpdf, dens, alpha, beta)
{
  plots <- list()
  for(i in (1:length(samp)))
  {
    plots[[i]] <- hist2(samp[i], x0, h, rpdf, dens, alpha, beta)+labs(title = paste("n = ", samp[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(samp)/2))
}


#For varying origin

hist_comp_origin1 <- function(n, origin, h, rpdf, dens, alpha)
{
  plots <- list()
  for(i in (1:length(origin)))
  {
    plots[[i]] <- hist1(n, origin[i], h, rpdf, dens, alpha)+labs(title = paste("x0 = ", origin[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(origin)/2))
}

hist_comp_origin2 <- function(n, origin, h, rpdf, dens, alpha, beta)
{
  plots <- list()
  for(i in (1:length(origin)))
  {
    plots[[i]] <- hist2(n, origin[i], h, rpdf, dens, alpha, beta)+labs(title = paste("x0 = ", origin[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(origin)/2))
}


#For varying width (binwidth are equal)

hist_comp_width1 <- function(n, x0, width, rpdf, dens, alpha)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist1(n, x0, width[i], rpdf, dens, alpha)+labs(title = paste("h = ", width[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}

hist_comp_width2 <- function(n, x0, width, rpdf, dens, alpha, beta)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist2(n, x0, width[i], rpdf, dens, alpha, beta)+labs(title = paste("h = ", width[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}

#parameters are given in order (sample size, origin, binwidth,rpdf, dens, parameters)

#Varying sample size

exp_size <- hist_comp_size1(c(100,500,1000,2000), 0, 0.5, rexp, dexp, 1)
title <- ggdraw() + draw_label("exp(1) with x0=0, h=0.5",fontface = 'bold')
plot_grid(title, exp_size,ncol = 1,rel_heights = c(0.1, 1))

norm_size <- hist_comp_size2(c(100,500,1000,2000), 0, 1, rnorm, dnorm, 0, 1)
title <- ggdraw() + draw_label("N(0,1) with x0=0, h=1",fontface = 'bold')
plot_grid(title, norm_size,ncol = 1,rel_heights = c(0.1, 1))

beta_size <- hist_comp_size2(c(100,500,1000,2000), 0, 0.1, rbeta, dbeta, 5, 2)
title <- ggdraw() + draw_label("Beta(5,2) with x0=0, h=0.1",fontface = 'bold')
plot_grid(title, beta_size,ncol = 1,rel_heights = c(0.1, 1))



#Varying origin

exp_origin <- hist_comp_origin1(500, c(0,0.3,0.6,0.8), 0.5, rexp, dexp, 1)
title <- ggdraw() + draw_label("exp(1) with n=500, h=0.5",fontface = 'bold')
plot_grid(title, exp_origin,ncol = 1,rel_heights = c(0.1, 1))

norm_origin <- hist_comp_origin2(500, c(0,0.3,-0.2,0.7), 1, rnorm, dnorm, 0, 1)
title <- ggdraw() + draw_label("N(0,1) with n=500, h=1",fontface = 'bold')
plot_grid(title, norm_origin,ncol = 1,rel_heights = c(0.1, 1))

beta_origin <- hist_comp_origin2(500, c(0,0.355,0.62,0.832), 0.1, rbeta, dbeta, 5, 2)
title <- ggdraw() + draw_label("Beta(5,2) with n=500, h=0.1",fontface = 'bold')
plot_grid(title, beta_origin,ncol = 1,rel_heights = c(0.1, 1))

#Varying binwidth

exp_width <- hist_comp_width1(500, 0, c(0.3,0.5,0.7,1), rexp, dexp, 1)
title <- ggdraw() + draw_label("exp(1) with n=500,x0=0",fontface = 'bold')
plot_grid(title, exp_width,ncol = 1,rel_heights = c(0.1, 1))

norm_width <- hist_comp_width2(500, 0, c(0.3,0.5,0.7,1), rnorm, dnorm, 0, 1)
title <- ggdraw() + draw_label("N(0,1) with n=500, x0=0",fontface = 'bold')
plot_grid(title, norm_width,ncol = 1,rel_heights = c(0.1, 1))

beta_width <- hist_comp_width2(500, 0, c(0.2,0.01,0.05,0.1), rbeta, dbeta, 5, 2)
title <- ggdraw() + draw_label("Beta(5,2) with n=500, x0=0",fontface = 'bold')
plot_grid(title, beta_width,ncol = 1,rel_heights = c(0.1, 1))

