#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/main.R
#### 
#### Main file: contains the R code for the individual based model
#### -----------------------------------------------------------------####

#### Library / environment ####
rm(list = ls())
library(tidyverse)
source("parameters.R") 
source("functions.R") 

#### Files initialization ####
if (!dir.exists(paste0("results"))) {dir.create("results")} 
if(!file.exists(paste0("results/",sim,"_results.txt"))) {stop("Simulation name already existing, results will be overwritten")}
file.create(paste0("results/",sim,"_description.txt"))
write(paste(sim,"- starting",date(),sep=" "),file=paste0("results/",sim,"_description.txt"))
write(paste(readLines("parameters.R",warn=FALSE)),file=paste0("results/",sim,"_description.txt"),append = T)
# get the seed as well
file.create(paste0("results/",sim,"_results.txt"))

#### Simulation initialization ####
ktot <- 0                                                              # init pop size
curr_main <- pop_init(km,ktot,optm,woptm,0)                            # current mainland population
ktot <- ktot+km                                                        # add mainland individuals 
curr_isl <- pop_init(ki,ktot,opti,wopti,1)                             # currant island population
ktot <- km+ki                                                          # total number of individuals (helps to name the new individuals after)
for(ind in 1:nrow(curr_main)){                                         #for each mainland individuals                                
  curr_main$fit[ind] <- get_fitness(curr_main$x[ind],optm,woptm,sigma) # get local fitness
}
for(ind in 1:nrow(curr_isl)){                                          # for each island individuals
  curr_isl$fit[ind] <- get_fitness(curr_isl$x[ind],opti,wopti,sigma)   # get local fitness
}
pop <- as.matrix(rbind(curr_main,curr_isl))
write.table(pop,file=paste0("results/",sim,"_results.txt"),append = T,col.names =F)

#### Simulation ####
for (t in 1:time){ # TIME loop
  
  # Birth event 
  off_main <- birth_event(curr_main,mu,ktot,t) 
  ktot <- ktot + nrow(off_main)
  off_isl <- birth_event(curr_isl,mu,ktot,t)
  ktot <- ktot + nrow(off_isl)
  
  # Mutation event
  off_main <- mutation_event(off_main,mu)
  off_isl <- mutation_event(off_isl,mu)
  
  # Migration event
  off_main <- migration_event(off_main,mr,msr)
  off_isl <- migration_event(off_isl,mr,msr) 
  
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
  
  # Competition event
  curr_main <- rbind(curr_main,competition_event(0,death_main,off_main,off_isl))
  curr_isl <- rbind(curr_isl,competition_event(1,death_isl,off_main,off_isl))
  pop <- rbind(curr_main,curr_isl)
  
  # Save new population
  pop <- as.matrix(rbind(curr_main,curr_isl))
  write.table(pop,file=paste0("results/",sim,"_results.txt"),append = T,col.names =F)
  
  # Remove
  rm(off_main,off_isl,pop)
  # history = "MIMMMI" tracker les historiques de colonizations dans une lignée
  # 
}

# log seed saved get the seed and put it in description file