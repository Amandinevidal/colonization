#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/parameters.R
#### 
#### This file contains a list of parameters to set for the simulation 
#### -----------------------------------------------------------------####

sim <- "test_birth"        # name your simulation
nsim <- 10            # number of replicates
time <- 250           # simulation time
k <- 5000             # mainland carrying capacity
ipk <- 0.1            # proportion of the carrying capacity of the mainland that corresponds to the carrying capacity of the island
dopt <- 2           # difference between mainland ecological optimum (0) and island optimum (0+dopt) 
wopt <- 0.5           # width ecological niche (same for mainland and island optimum)
mr <- 0.2             # migration rate
msri <- 0.8           # migration survival rate isl -> main
msrm <- 0.2           # migration survival rate main -> isl
d <- 0.1              # death rate
wmax <- 1             # maximum achievable fitness
sigma <- 0.5          # niche width
mu <- 1             # pheno mutation rate
seed <- runif(1)      # save random seed
