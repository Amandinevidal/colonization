#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/functions.R
#### 
#### This file contains a suite of functions designed to run the simulation
#### -----------------------------------------------------------------####

#### Fitness function ####
# Calculate the fitness value of an individual based on its ecological trait and the current local optimum of their location
# x is the ecological trait, opt the ecological optimum of the individual's location, wmax the maximum value of fitness function, and sigma the width on the fitness function
get_fitness <- function(x, opt, wmax, sigma) {
  f <- wmax * exp( - ((x - opt)^2 / sigma^2))
  return(f)
} 

#### Population initialization ####
# Create one tibble that initiate the population on the mainland or on the island depending on the arguments
# k is the wanted number of individuals, ktot the actual number of individuals in the system (helps to set the id of new individuals), opt the local ecological optimum, wopt the width of the ecological trait landscape, origin is 0 for mainland and 1 for island
pop_init <- function(k,ipk,dopt,wopt,sigma,wmax,seed){
  # MAINLAND POP
  set.seed(seed)
  curr_main <- tibble(
    i=seq(1,k),         # individuals id
    x=rnorm(k,0,wopt),  # ecological trait
    fit=NA,             # fitness value according to local optimum
    origin=0,           # birth location
    loc=0,              # current location
    mother=0,           # mother id
    off=0,              # number of offspring produced
    mig=0,              # if 1 the ind migrate, if 0 not
    age=0,              # generational age of individuals
    time=0              # current time
  )
  for(ind in 1:nrow(curr_main)){                                         # for each mainland individuals                    
    curr_main$fit[ind] <- get_fitness(curr_main$x[ind],0,wmax,sigma)     # get local fitness
  }
  # ISLAND POP
  set.seed(seed)
  curr_isl <- tibble(
    i=seq(k+1,k+ipk*k),     # individuals id
    x=rnorm(ipk*k,dopt,wopt),  # ecological trait
    fit=NA,                # fitness value according to local optimum
    origin=1,              # birth location
    loc=1,                 # current location
    mother=0,              # mother id
    off=0,                 # number of offspring produced
    mig=0,                 # if 1 the ind migrate, if 0 not
    age=0,                 # generational age of individuals
    time=0                 # current time
  )
  for(ind in 1:nrow(curr_isl)){                                         # for each mainland individuals                     
    curr_isl$fit[ind] <- get_fitness(curr_isl$x[ind],dopt,wmax,sigma) # get local fitness
  }
  curr_main <<- curr_main
  curr_isl <<- curr_isl
}

#### Mutation function ####
# Initiate a mutation event, for each offspring of the offspring data set it test weither the individual is going through a mutation event, if its the case it make its trait vary from the one inherited by the individual's parent
# off_data is the offspring data set, mu the mutation rate
mutation_event <- function(off_data,mu,seed){
  set.seed(seed)
  n <- rbinom(1,nrow(off_data),mu)             # how many individuals mutate
  nmut <<- n                                   # global var
  if(n!=0){                                    # if there is mutation
    set.seed(seed)
    nind <- sample(nrow(off_data),n,replace=F) # who is mutating
    for(r in nind){                            # for each
      set.seed(seed)
      newx <- off_data$x[r] + rnorm(1,0,mu)    # new ecological trait
      off_data$x[r] <- newx                    # save it
    }
  }
  return(off_data)
}

#### Birth function ####
# This function take a local population at a time t, and calculate for each individuals how many offspring it gives birth, allocate a new ecological trait (inherited by from the parent individual), and save them in a table which represent the pool of offspring for this time t in the location chose before, this table is the output of the function
# curr_data is the population data set in mainland or in island, ktot is the total number of individuals in the system and t the actual time (from time loop)
birth_event_main <- function(curr_data,ktot,t){    # apply it to curr_main and curr_isl
  off_data <- tibble(
    i=numeric(),
    x=numeric(),
    fit=numeric(),
    origin=numeric(),
    loc=numeric(),
    mother=numeric(),
    off=numeric(),
    mig=numeric(),
    age=numeric(),
    time=t
  )
  for (ind in seq(nrow(curr_data))){             # for each individuals
    set.seed(seed)
    noff <- rpois(1,curr_data[ind,]$fit)         # how many offspring according to fitness value
    curr_data[ind,]$off <- noff                  # save number of offspring
    if (!is.na(noff)&&noff!=0){                  # if there is offspring
      for (j in seq(noff)){                      # for each offspring
        ktot <- ktot + 1                         # add new ind
        off_data <- off_data %>% add_row(i=ktot,x=curr_data$x[ind],fit=NA,origin=curr_data$loc[ind],loc=curr_data$loc[ind],mother=curr_data$i[ind],off=0,mig=0,age=0,time=t) # add to the offspring pool
      }
    }
  }
  curr_main <<- curr_data
  return(off_data)
}

birth_event_isl <- function(curr_data,ktot,t,seed){    # apply it to curr_main and curr_isl
  off_data <- tibble(
    i=numeric(),
    x=numeric(),
    fit=numeric(),
    origin=numeric(),
    loc=numeric(),
    mother=numeric(),
    off=numeric(),
    mig=numeric(),
    age=numeric(),
    time=t
  )
  for (ind in seq(nrow(curr_data))){             # for each individuals
    set.seed(seed)
    noff <- rpois(1,curr_data[ind,]$fit)         # how many offspring according to fitness value
    curr_data[ind,]$off <- noff                  # save number of offspring
    if (!is.na(noff)&&noff!=0){                  # if there is offspring
      for (j in seq(noff)){                      # for each offspring
        ktot <- ktot + 1                         # add new ind
        off_data <- off_data %>% add_row(i=ktot,x=curr_data$x[ind],fit=NA,origin=curr_data$loc[ind],loc=curr_data$loc[ind],mother=curr_data$i[ind],off=0,mig=0,age=0,time=t) # add to the offspring pool
      }
    }
  }
  curr_isl <<- curr_data
  return(off_data)
}

#### Migration function ####
# This function chose who is migrating within the offspring pool, who is surviving the migration, allowing the value 1 to the survivors and removing the dead ones in the offspring dataset
# off_data is the offspring data set at a location (mainland or island), mr the migration rate and msr the migration survival rate
migration_event <- function(off_data,mr,msr,seed){
  set.seed(seed)
  m <- rbinom(1,nrow(off_data),mr)                # how many individuals are migrating
  if(m!=0){ # if migrating individuals
    set.seed(seed)
    indm <- sample(nrow(off_data),m,replace=F)    # who is migrating
    set.seed(seed)
    ms <- rbinom(1,length(indm),msr)              # how many are surviving migration
    if(ms!=0){                                    # if survivors
      set.seed(seed)
      indms <- sample(indm,ms,replace=F)          # who is surviving
      for(ind in indms){                          # for each survivors 
        off_data$loc[ind] <- ifelse(off_data$loc[ind]==0,1,0) # change current location
        off_data$mig[ind] <- 1
      }
      off_data <- off_data[-setdiff(indm,indms),] # remove dead
    }
    }
  return(off_data)
}

#### Death function ####
# This function randomly selects individuals among the parent pool that die, according to the death rate, and remove it from the population data set
# curr_data is the local population data set and d the death rate
death_event <- function(curr_data,d,seed){
  set.seed(seed)
  dn <- rbinom(1,nrow(curr_data),d)                   # how many individuals are going to die
  if(dn!=0){
    set.seed(seed)
    dind <- sample(seq(nrow(curr_data)),dn,replace=F) # who is dying
    curr_data <- curr_data[-dind,]                    # remove them
    nindglob <<- length(dind)                        # get out the number of death
  } else {
    nindglob <<- 0                                   # get out the number of death
  }
  return(curr_data)
}

#### Competition function ####
# This function combines rows from mainland and island populations. It calculates the probability of being colonizer for each ind based on the fit column and randomly selects a number of individuals (based on the number of death) based on this probability.
competition_event <- function(location,dind,off_main,off_isl,seed){
  comp <- rbind(off_main[which(off_main$loc==location),],off_isl[which(off_isl$loc==location),]) # Combine rows from off_main and off_isl based on the specified location
  if(nrow(comp)>dind){              # if there are more competitors than places available
    comp$prob <- NA                 # Add a new column named prob to the comp data frame and initialize it as NA
    for (ind in seq(nrow(comp))){   # Calculate the individual probability for each ind
      comp$prob[ind] <- comp$fit[ind]/sum(comp$fit)
    }
    set.seed(seed)
    win <- comp[sample(seq(nrow(comp)),size=dind,replace=F,prob=comp$prob),-11] # Randomly sample dind number of rows from the comp data frame without replacement and by using the probabilities in the prob column as weights for sampling, also exclude the 10th column from the resulting win data frame because not usefull after
  } else { # if there are less competitors than places available
    win <- comp
  }
  return(win)
}

