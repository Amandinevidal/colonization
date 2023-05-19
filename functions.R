#### Population initialization ####
# Create one tibble that initiate the population on the mainland and on the island. The variables are: i the id of the individuals, x the ecological trait, fit the fitness value, origin their birth place (0 mainland 1 island), loc their current location (0 mainland 1 island), mother the individual their are born from, off the number of individuals they give birth to, mig did they migrate (0 no 1 yes), death did they die (0 no, 1 they died at this time), t time
pop_init <- function(k,ktot,opt,wopt,origin){
  pop <- tibble(
    i=seq(ktot+1,ktot+k), # individuals id
    x=rnorm(k,opt,wopt),  # ecological trait
    fit=NA,               # fitness value according to local optimum
    origin=origin,        # birth location
    loc=origin,           # current location
    mother=0,             # mother id
    off=0,                # number of offspring produced
    mig=0,                # if 1 the ind migrate, if 0 not
    time=0                # current time
  )
  return(pop)
}

#### Fitness function ####
# Calculate the fitness value of an individual based on its ecological trait and the current local optimum of their location
get_fitness <- function(x, opt, wmax, sigma) {
  f <- wmax * exp( - ((x - opt)^2 / sigma^2))
  return(f)
} 

#### Mutation function ####
mutation_event <- function(off_data,mu){
  n <- rbinom(1,nrow(off_data),mu)             # how many individuals mutate
  if(n!=0){                                    # if there is mutation
    nind <- sample(nrow(off_data),n,replace=F) # who is mutating
    for(r in nind){                            # for each
      newx <- off_data$x[r] + rnorm(1,0,mu)    # new ecological trait
      off_data$x[r] <- newx                    # save it
    }
  }
  return(off_data)
}

#### Birth function ####
# This function take a local population at a time t, and calculate for each individuals how many offspring it gives birth, allocate a new ecological trait after mutation, and save them in a table wich represent the pool of offspring for this time t in the location chose before, this table is the output of the function
birth_event <- function(curr_data,mu,ktot,t){    # apply it to curr_main and curr_isl
  off_data <- tibble(
    i=numeric(),
    x=numeric(),
    fit=numeric(),
    origin=numeric(),
    loc=numeric(),
    mother=numeric(),
    off=numeric(),
    mig=numeric(),
    time=numeric()
  )
  for (ind in seq(nrow(curr_data))){             # for each individuals
    noff <- rpois(1,curr_data[ind,]$fit)         # how many offspring according to fitness value
    curr_data[ind,]$off <- noff                  # save number of offspring
    if (!is.na(noff)&&noff!=0){                  # if there is offspring
      for (j in seq(noff)){                      # for each offspring
        ktot <- ktot + 1                         # add new ind
        off_data <- off_data %>% add_row(i=ktot,x=curr_data$x[ind],fit=NA,origin=curr_data$loc[ind],loc=curr_data$loc[ind],mother=curr_data$i[ind],off=NA,mig=NA,time=t) # add to the offspring pool
      }
    }
  }
  return(off_data)
}

#### Migration function ####
# This function chose who is migrating whitin the offspring pool, who is surviving the migration, allowing the value 1 to the survivors and removing the dead ones
migration_event <- function(off_data,mr,msr){
  m <- rbinom(1,nrow(off_data),mr)                # how many individuals are migrating
  if(m!=0){ # if migrating individuals
    indm <- sample(nrow(off_data),m,replace=F)    # who is migrating
    ms <- rbinom(1,length(indm),msr)              # how many are surviving migration
    if(ms!=0){                                    # if survivors
      indms <- sample(indm,ms,replace=F)          # who is surviving
      for(ind in indms){                          # for each survivors 
        off_data$loc[ind] <- ifelse(off_data$loc[ind]==0,1,0) # change current location
      }
      off_data <- off_data[-setdiff(indm,indms),] # remove dead
    }
    }
  return(off_data)
}

#### Death function ####
# This function randomly selects individuals among the parent pool that die, according to the death rate
death_event <- function(curr_data,d,loc){
  dn <- rbinom(1,nrow(curr_data),d)                 # how many individuals are going to die
  dind <- sample(seq(nrow(curr_data)),dn,replace=F) # who is dying
  curr_data <- curr_data[-dind,]                    # remove them
  nindglob <<- length(dind)                         # get out the number of death
  return(curr_data)
}

#### Competition function ####
competition_event <- function(location,dind,off_main,off_isl){
  comp <- rbind(off_main[which(off_main$loc==location),],off_isl[which(off_isl$loc==location),]) 
  comp$prob <- NA
  for (ind in seq(nrow(comp))){
    comp$prob[ind] <- comp$fit[ind]/sum(comp$fit)
  }
  win <- comp[sample(seq(nrow(comp)),size=dind,replace=F,prob=comp$prob),-10]
  return(win)
}


 

