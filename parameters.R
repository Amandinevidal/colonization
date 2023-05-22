sim <- "test1"         # name your simulation
nsim <- 5             # number of replicates
time <- 50            # simulation time
km <- 200             # mainland carrying capacity
ki <- 0.4*km          # island carrying capacity
optm <- 10            # mainland ecological optimum
woptm <- 4            # mainland width ecological niche
opti <- 10            # island ecological optimum
wopti <- 4            # island width ecological niche
mr <- 0.2             # migration rate
msri <- 0.8           # migration survival rate isl -> main
msrm <- 0.2           # migration survival rate main -> isl
d <- 0.1              # extinction rate
wmax <- 1             # maximum achievable fitness
sigma <- 0.5          # niche width
mu <- 0.1             # pheno mutation rate
