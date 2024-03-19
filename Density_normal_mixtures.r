rm(list = ls())

library(ggplot2)
library(gridExtra)
library(cowplot)


hist_mix_norms <- function(n,x0,h,m1,s1,m2,s2,p){
  set.seed(seed=1)
  y <- rnorm(n*p,m1,s1)
  z <- rnorm(n*(1-p),m2,s2)
  x <- c(y,z[z!=y])
  s<- seq(min(x)-0.5,max(x)+0.5,0.1)
  ds <- p*dnorm(s,m1,s1)+(1-p)*dnorm(s,m2,s2)
  ggplot() + geom_histogram(data = data.frame(x), aes(x = x, y = after_stat(density)),center=x0-h/2, binwidth=h, color="black", fill="grey")  + 
    geom_line(data = data.frame(s, ds), aes(x = s, y = ds))
}


hist_comp_size3 <- function(samp, x0, h, m1,s1,m2,s2,p)
{
  plots <- list()
  for(i in (1:length(samp)))
  {
    plots[[i]] <- hist_mix_norms(samp[i], x0, h, m1,s1,m2,s2,p)+labs(title = paste("n = ", samp[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(samp)/2))
}

normmix_size <- hist_comp_size3(c(500,1000,5000,10000),0,0.5,-5,1,4,3,0.3)
title <- ggdraw() + draw_label("0.3*N(-5,1)+0.7*N(4,3) with x0=0, h=0.5",fontface = 'bold')
plot_grid(title, normmix_size,ncol = 1,rel_heights = c(0.1, 1))



hist_comp_origin3 <- function(n, origin, h, m1,s1,m2,s2,p)
{
  plots <- list()
  for(i in (1:length(origin)))
  {
    plots[[i]] <- hist_mix_norms(n, origin[i], h, m1,s1,m2,s2,p)+labs(title = paste("x0 = ", origin[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(origin)/2))
}

normmix_origin <- hist_comp_origin3(1000,c(0,0.2,-0.7,-0.4),0.5,-5,1,4,3,0.3)
title <- ggdraw() + draw_label("0.3*N(-5,1)+0.7*N(4,3) with n=1000, h=0.5",fontface = 'bold')
plot_grid(title, normmix_origin,ncol = 1,rel_heights = c(0.1, 1))



hist_comp_width3 <- function(n, x0, width, m1,s1,m2,s2,p)
{
  plots <- list()
  for(i in (1:length(width)))
  {
    plots[[i]] <- hist_mix_norms(n, x0, width[i], m1,s1,m2,s2,p)+labs(title = paste("h = ", width[i]))
  }
  grid.arrange(grobs = plots, ncol = round(length(width)/2))
}

normmix_width <- hist_comp_width3(1000,0,c(0.5,1,0.1,2),-5,1,4,3,0.3)
title <- ggdraw() + draw_label("0.3*N(-5,1)+0.7*N(4,3) with n=1000, x0=0",fontface = 'bold')
plot_grid(title, normmix_width,ncol = 1,rel_heights = c(0.1, 1))
