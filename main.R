#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/main.R
#### 
#### Main file: contains the R code for the individual based model and
#### run a simulation
#### -----------------------------------------------------------------####

#### Library / environment ####
rm(list = ls())          # clear
library(tidyverse)
source("functions.R")    # import functions
source("parameters.R")   # import parameters
source("test_param.R")   # check parameters consistency

#### Simulations set ####
for(run in 1:nsim){
  
  seed <- runif(1)
  set.seed(seed)
  start <- Sys.time()
  
  #### Files initialization ####
  if (!dir.exists(paste0("results"))) {dir.create("results")} 
  file.create(paste0("results/",sim,"_",run,"_description.txt"))
  write(paste(sim,"- starting",date(),sep=" "),file=paste0("results/",sim,"_",run,"_description.txt"))
  write(paste("Simulation SEED",seed),file=paste0("results/",sim,"_",run,"_description.txt"),append=T)
  file.create(paste0("results/",sim,"_",run,"_results.txt"))
  file.create(paste0("results/",sim,"_",run,"_summary.txt"))
  
  #### Simulation initialization ####
  ktot <- 0                                                                                   # init pop size
  curr_main <- pop_init(km,ktot,optm,woptm,0)                                                 # current mainland population
  ktot <- ktot+km                                                                             # add mainland individuals 
  curr_isl <- pop_init(ki,ktot,opti,wopti,1)                                                  # currant island population
  ktot <- km+ki                                                                               # total number of individuals (helps to name the new individuals after)
  pop <- as.matrix(rbind(curr_main,curr_isl))                                                 # total pop
  write.table(pop,file=paste0("results/",sim,"_",run,"_results.txt"),append = T,col.names =F) # save total pop
  
  #### Check variables in summary file ####
  # Table that save at each time step : nindm (number of ind on mainland), nindi (number of ind on island), ndm (number od death on mainland), ndi (number of death on island), nbm (number of birth on mainland), nbi (number of birth on island), nmutm (number of mutation on mainland), nmuti (number of mutation on island), nmigm (number of migration from mainland), nmigi (number of migration from island), smigm (number of successful migration from mainland), smigi (number of successful migration frim island)
  
  #### Simulation ####
  for (t in 1:time){ # TIME loop
    
    # set time
    curr_main$time <- t
    curr_isl$time <- t
    
    # Birth event 
    off_main <- birth_event(curr_main,ktot,t) 
    ktot <- ktot + nrow(off_main)
    off_isl <- birth_event(curr_isl,ktot,t)
    ktot <- ktot + nrow(off_isl)
    nbm <- nrow(off_main)
    nbi <- nrow(off_isl)

    # Mutation event
    off_main <- mutation_event(off_main,mu)
    nmutm <- nmut
    off_isl <- mutation_event(off_isl,mu)
    nmuti <- nmut
    
    # Migration event
    off_main <- migration_event(off_main,mr,msrm)
    off_isl <- migration_event(off_isl,mr,msri) 
    nmigm <- length(which(off_main[,8]==1))
    nmigi <- length(which(off_isl[,8]==1))

    # Local fitness calculation
    for(ind in 1:nrow(off_main)){
      off_main$fit[ind] <- get_fitness(off_main$x[ind],ifelse(off_main$loc[ind]==0,optm,opti),ifelse(off_main$loc[ind]==0,woptm,wopti),sigma)
    }
    for(ind in 1:nrow(off_isl)){
      off_isl$fit[ind] <- get_fitness(off_isl$x[ind],ifelse(off_isl$loc[ind]==0,optm,opti),ifelse(off_isl$loc[ind]==0,woptm,wopti),sigma)
    }
    
    # Death event
    curr_main <- death_event(curr_main,d)
    death_main <- nindglob
    curr_isl <- death_event(curr_isl,d)
    death_isl <- nindglob
    summ[t,4] <- death_main
    summ[t,5] <- death_isl
    summ[t,6] <- sum(death_main,death_isl)
    
    # Competition event = pas ok repose sur le nombre de death mais devrait reposer sur la différence de nindv avec la somme totale attendue km+ki
    comp_main <- competition_event(0,death_main,off_main,off_isl)
    comp_isl <- competition_event(1,death_isl,off_main,off_isl)
    summ[t,13] <- length(which(comp_main$mig==1))
    summ[t,14] <- length(which(comp_isl$mig==1))
    curr_main <- rbind(curr_main,comp_main)
    curr_isl <- rbind(curr_isl,comp_isl)
    pop <- rbind(curr_main,curr_isl)
    summ[t,1] <- nrow(curr_main)
    summ[t,2] <- nrow(curr_isl)
    summ[t,3] <- nrow(pop)
    
    # Save new population
    pop <- as.matrix(rbind(curr_main,curr_isl))
    write.table(pop,file=paste0("results/",sim,"_",run,"_results.txt"),append = T,col.names =F)
    
    # Remove
    rm(off_main,off_isl,comp_main,comp_isl,pop)
    
    print(paste("Time",t,"done"))
  }
  
  write(paste(sim,"- end, simulation time:",Sys.time()-start,sep=" "),file=paste0("results/",sim,"_",run,"_description.txt"),append = T)
  write(paste(readLines("parameters.R",warn=FALSE)),file=paste0("results/",sim,"_",run,"_description.txt"),append = T)
  write.table(summ,file=paste0("results/",sim,"_",run,"_summary.txt"),col.names =F)
  print(paste("RUN",run,"done"))
}
