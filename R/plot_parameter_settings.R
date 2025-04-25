#### --------------------------------------------------------------------####
# Date : 2023-11-02
# Author : Amandine Vidal-Hosteng
# File : colonization/R/plot_parameters_settings.txt
# 
# Aim to test parameters and have a idea of the interpretations of each 
# one.
#### --------------------------------------------------------------------####

source("R/functions.R")
library(ggplot2)
library(cowplot)
library(tidyverse)

#### Parameters list ####

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

#### TIME ####
# 100 is enough to see changing in ecological trait and colonizations events, but not to be sure to reach a steady state
# 500 is goo to see a steady state but takes too much time to run

#### CARRYING CAPACITY ####
# 5000 not enough apparently
# 10000 works but too long to run!

#### DIFFERENCE BETWEEN ECOLOGICAL OPTIMUM ####
# dopt 0, no comments
# dopt 0.5 mainland migrants are adapted to the island so there is competition between island and 
# dopt 2 
# dopt 4

#### wopt/sigma/wmax/dopt ####

k=5000;ipk=0.1;dopt=2;wopt=0.5;sigma=0.5;wmax=1
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
title <- ggdraw() + draw_label(paste("k=",k,"ipk=",ipk,"dopt=",dopt,"wopt=",wopt,"sigma=",sigma,"wmax=",wmax),size=35)
p <- plot_grid(dens_x,hist_x,dens_f,hist_f,nrow=2,ncol=2)
p2 <- plot_grid(p,x_f,nrow=2,ncol=1,rel_heights = c(1,0.5))
pf <- plot_grid(title,p2,nrow=2,ncol=1,rel_heights=c(0.1, 1))

png(paste0("results/plot_param/trait_fitness_","k=",k,"ipk=",ipk,"dopt=",dopt,"wopt=",wopt,"sigma=",sigma,"wmax=",wmax,".png"),width=1400,height = 1500)
print(pf)
dev.off()


