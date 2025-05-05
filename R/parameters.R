sim <- "K1"            # name your simulation
nsim <- 30            # number of replicates
time <- 1000          # simulation time
k <- 500             # mainland carrying capacity
ipk <- 1              # proportion of the carrying capacity of the mainland that corresponds to the carrying capacity of the island
dopt <- 2             # difference between mainland ecological optimum (0) and island optimum (0+dopt) 
wopt <- 10/3          # width ecological niche (same for mainland and island optimum)
mr <- 0.5             # migration rate
msri <- 0.8           # migration survival rate isl -> main
msrm <- 0.2           # migration survival rate main -> isl
d <- 0.1              # death rate
wmax <- 1             # maximum achievable fitness
sigma <- 4.8          # niche width
mu <- 1               # pheno mutation rate
ncores <- 10           #if parallel
seed <- runif(1)      # save random seed
