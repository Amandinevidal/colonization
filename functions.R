#### Fitness function ####
# Calculate the fitness value of an invidual based on its ecological trait and the current local optimum of their location
get_fitness <- function(x, loc, wmax, sigma) {
  xopt <- ifelse(loc==0,optm,opti)
  f <- wmax * exp( - ((x - xopt)^2 / sigma^2))
  return(f)
} 

#### Population initialization ####
# Create one tibble that initiate the population on the mainland and on the island. The variables are: i the id of the individuals, x the ecological trait, fit the fitness value, origin their birth place (0 mainland 1 island), loc their current location (0 mainland 1 island), mother the individual their are born from, off the number of individuals they give birth to, mig did they migrate (0 no 1 yes), death did they die (0 no, 1 they died at this time), t time
pop_init <- function(km,ki,optm,opti,woptm,wopti){
  mainland <- tibble(i=seq(1,km),x=rnorm(km,optm,woptm),fit=NA,origin=0,loc=0,mother=0,off=0,mig=0,death=0,time=0)
  for (r in seq(nrow(mainland))) {mainland$fit[r] <- get_fitness(mainland$x[r],0,wmax,sigma)}
  island <- tibble(i=seq(km+1,km+ki),x=rnorm(ki,opti,wopti),fit=NA,origin=1,loc=1,mother=0,off=0,mig=0,death=0,time=0)
  for (r in seq(nrow(island))) {island$fit[r] <- get_fitness(island$x[r],1,wmax,sigma)}
  pop <- rbind(mainland,island)
  return(pop)
}

#### Birth function ####
# This function take a local population at a time t, and calculate for each individuals how many offspring it gives birth, allocate a new ecological trait after mutation, and save them in a table wich represent the pool of offspring for this time t in the location chose before, this table is the output of the function
birth_event <- function(data_pop,mu,ktot,t){ # apply it to curr_main and curr_isl
  data_off <- tibble(
    i=numeric(),
    x=numeric(),
    fit=numeric(),
    origin=numeric(),
    loc=numeric(),
    mother=numeric(),
    off=numeric(),
    mig=numeric(),
    death=numeric(),
    time=numeric()
  )
  for (ind in seq(nrow(data_pop))){ # for each individuals
    noff <- rpois(1,data_pop[ind,]$fit) # how many offspring according to fitness value
    data_pop[ind,]$off <- noff # save number of offspring
    if (!is.na(noff)&&noff!=0){ # if there is offspring
      for (j in seq(noff)){ # for each offspring
        ktot <- ktot + 1 # add new ind
        newx <- data_pop$x[ind] + rnorm(1, 0, mu) # mutation
        data_off <- data_off %>% add_row(i=ktot,x=newx,fit=NA,origin=data_pop$loc[ind],loc=data_pop$loc[ind],mother=data_pop$i[ind],off=NA,mig=NA,death=NA,time=t) # add to the offspring pool
      }
    }
  }
  return(data_off)
}

#### Migration function ####
migration_event <- function(off_data,mr,msr){
  m <- sample(seq(nrow(data_off)),nrow(data_off)*mr,replace=F)
  ms <- sample(m,length(m)*msr,replace=F)
  data_off$mig[ms] <- 1
  ndata_off <- data_off %>% filter(!row_number() %in% m[which(m!=ms)])
  return(ndata_off)
}


  