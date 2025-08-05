#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : amandine.vidalhosteng@gmail.com
#### File path : colonization/R/parameters_calibration.R
#### 
#### Tools for manual testing and visualization of all parameters and fitness function  
#### -----------------------------------------------------------------####

source("R/functions.R")
library(ggplot2)
library(cowplot)
library(tidyverse)

#### Parameters list and visualization ####

time <- 100           # simulation time
k <- 5000             # mainland carrying capacity
ipk <- 0.1            # proportion of the carrying capacity of the mainland that corresponds to the carrying capacity of the island
dopt <- 0.5             # difference between mainland ecological optimum (0) and island optimum (0+dopt) 
wopt <- 1             # width ecological niche (same for mainland and island optimum)
mr <- 0.5             # migration rate
msri <- 0.8           # migration survival rate isl -> main
msrm <- 0.2           # migration survival rate main -> isl
d <- 0.1              # death rate
wmax <- 1             # maximum achievable fitness
sigma <- 0.5          # niche width
mu <- 0.1             # pheno mutation rate
seed <- runif(1)      # save random seed

pop_init(k,ipk,dopt,wopt,sigma,wmax) 

pop1 <- rbind(curr_main,curr_isl)
dens_x <- ggplot(data=pop1,aes(x=x,fill=as.factor(loc)),alpha=0.3)+
  geom_density(show.legend = FALSE)+
  labs(y="Density")+
  labs(x="Ecological trait")+
  scale_fill_manual(values = c("#D55E0050","#0072B250"), labels = c("Mainland","Island"),name = "Location") +
  ggplot2::theme(text = element_text(size = 35)) 
hist_x <- ggplot(data=pop1,aes(x=x,fill=as.factor(loc)),alpha=0.3)+
  geom_histogram(show.legend = FALSE)+
  labs(x="Ecological trait")+
  scale_fill_manual(values = c("#D55E0050","#0072B250"), labels = c("Mainland","Island"),name = "Location") +
  ggplot2::theme(text = element_text(size = 35)) 
dens_f <- ggplot(data=pop1,aes(x=fit,fill=as.factor(loc)),alpha=0.3)+
  geom_density(show.legend = FALSE)+
  labs(y="Density")+
  labs(x="Fitness")+
  scale_fill_manual(values = c("#D55E0050","#0072B250"), labels = c("Mainland","Island"),name = "Location") +
  ggplot2::theme(text = element_text(size = 35)) 
hist_f <- ggplot(data=pop1,aes(x=fit,fill=as.factor(loc)),alpha=0.3)+
  geom_histogram(show.legend = FALSE)+
  labs(x="Fitness")+
  scale_fill_manual(values = c("#D55E0050","#0072B250"), labels = c("Mainland","Island"),name = "Location") +
  ggplot2::theme(text = element_text(size = 35)) 
x_f <- ggplot(data=pop1,aes(x=x,y=fit,group=as.factor(loc)))+
  geom_line(aes(color=as.factor(loc)),linewidth=1,alpha=0.5)+
  scale_color_manual(values = c("#D55E0050","#0072B250"), labels = c("Mainland","Island"),name = "Location") +
  labs(y="Fitness")+
  labs(x="Ecological trait")+
  ggplot2::theme(legend.position = "right",text = element_text(size = 35)) 


#### Population trait distribution and fitness calibration ####
k <- 10000              # Population size
mainland_opt <- 0       # Local optimum
max_dopt <- 10          # Max trait range expected
wopt <- max_dopt / 3    # Trait standard deviation (99.7% within range)

set.seed(123)
traits <- rnorm(k, mean = mainland_opt, sd = wopt)

get_fitness <- function(x, opt, wmax, sigma) {
  wmax * exp(- ((x - opt)^2 / sigma^2))
}

t_x <- max_dopt/2-1      # Trait distance at which fitness = t_f
t_f <- 0.5               # Fitness value at distance t_x
sigma <- t_x / sqrt(-log(t_f))
wmax <- 1

# Fitness values for each individual
fitness_vals <- get_fitness(traits, opt = mainland_opt, wmax = wmax, sigma = sigma)

# Histogram and density
hist(traits, breaks = 40, col = rgb(173, 216, 230, max = 255, alpha = 150),
     main = "Trait distribution and individual fitness",
     xlab = "Trait value", ylab = "Density / Fitness",
     xlim = c(-12, 12), freq = FALSE,
     ylim = c(0, max(density(traits)$y, fitness_vals) * 1.2))

# Add density curve
lines(density(traits), col = "red", lwd = 2)

# Overlay fitness values for individuals
points(traits, fitness_vals, col = rgb(0, 0, 1, 0.3), pch = 16)

# Add vertical lines for optimum and thresholds
abline(v = mainland_opt, col = "darkgreen", lty = 2)
abline(v = c(mainland_opt - t_x, mainland_opt + t_x), col = "orange", lty = 2)

# Legend
legend("topright",
       legend = c("Trait density", "Individual fitness", "Optimum", paste("±", t_x)),
       col = c("red", "blue", "darkgreen", "orange"),
       lty = c(1, NA, 2, 2), pch = c(NA, 16, NA, NA), lwd = c(2, NA, 1, 1))


