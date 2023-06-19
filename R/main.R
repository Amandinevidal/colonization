#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/main.R
#### 
#### Main file: contains the R code for the individual based model and
#### run a simulation
#### -----------------------------------------------------------------####

#### Files initialization / Library / environment ####
rm(list = ls())                                                                               # clear environment
library(tidyverse)                                                                            # needed package
source("R/functions.R")                                                                         # import functions
source("R/parameters.R")                                                                        # import parameters
if (!dir.exists(paste0("results"))) {dir.create("results")}                                   # create results file if not existing
file.create(paste0("results/",sim,"_log.txt"))                                                # create simulation log file
source("R/check_param.R")                                                                       # check parameters consistency
start <- Sys.time()                                                                           # save simulation starting time
write(paste(sim,"- starting",date(),sep=" "),file=paste0("results/",sim,"_log.txt"))  # save starting time
set.seed(seed)                                                                                # set seed
write(paste("Simulation seed:",seed),file=paste0("results/",sim,"_log.txt"),append=T) # save simulation seed

#### Simulations set ####
for(run in 1:nsim){
  
  #### Files initialization for replicate ####
  file.create(paste0("results/",sim,"_",run,"_results.txt")) # Table that save all individuals existing in the system at each time step: i (individual id), x (ecological trait), fit (fitness value according the the individual location), origin (where the individual is born), loc (individual's location), mother (individual that gave birth to the focused one), off (number of offpsring given by the focused individual at this time step), mig (did this individual migrate at this time step, if 0 no, if 1 yes), time (current time)
  file.create(paste0("results/",sim,"_",run,"_summary.txt")) # Table that save at each time step :t (time), nm (number of ind on mainland), ni (number of ind on island), xm (ecological trait mean of all individuals on mainalnd), xi (ecological trait mean of all individuals on island), nbm (number of birth on mainland), nbi (number of birth on island), nmutm (number of mutation on mainland), nmuti (number of mutation on island), nmigm (number of migration from mainland), nmigi (number of migration from island), ndm (number od death on mainland), ndi (number of death on island), nmcompm (number of successful competitors on mainland from mainland), nmcompi (number of successful competitors on mainland from island) nicompi (number of successful competitors on island from island), nicompm (successful competitors on island from mainland)
  
  # Table that save at each time step :t (time), nm (number of ind on mainland), ni (number of ind on island), xm (ecological trait mean of all individuals on mainalnd), xi (ecological trait mean of all individuals on island), bm (number of birth on mainland), bi (number of birth on island), mutm (number of mutation on mainland), muti (number of mutation on island), migm (number of migration from mainland), migi (number of migration from island), dm (number od death on mainland), di (number of death on island), ebm (number of successful competitors on mainland from mainland), ebi (number of successful competitors on island from island), emigi (number of successful competitors on mainland from island), emigm (successful competitors on island from mainland)
  
  if(!file.exists(paste0("results/",sim,"_",run,"_results.txt"))){warning("ERROR with results.txt: not existing")}
  if(!file.exists(paste0("results/",sim,"_",run,"_summary.txt"))){warning("ERROR with summary.txt: not existing")}

    #### Simulation initialization ####
  pop_init(k,ipk,dopt,wopt,sigma)              # mainland and island population tibble initialization
  pop <- as.matrix(rbind(curr_main,curr_isl))  # total pop
  ktot <- nrow(pop)                            # total number of individuals
  write.table(pop,file=paste0("results/",sim,"_",run,"_results.txt"),append = T,col.names =F) # save total pop
  
  if(ktot!=sum(k+k*ipk)){write("Ktot different from population number of rows.", log.path, append=TRUE)}
  
  #### Simulation ####
  for (t in 1:100){ # TIME loop
    
    # set time
    curr_main$time <- t # set current time into mainland pop table
    curr_isl$time <- t  # set current time into island pop table
    pnm <- nrow(curr_main) # save the number of individuals on mainland before all events
    pni <- nrow(curr_isl)  # save the number of individuals on island before all events
    
    # Birth event 
    off_main <- birth_event(curr_main,ktot,t) # birth event on mainland
    ktot <- ktot + nrow(off_main)             # update total number of individuals
    off_isl <- birth_event(curr_isl,ktot,t)   # birth event on island
    ktot <- ktot + nrow(off_isl)              # update total number of individuals
    bm <- nrow(off_main)                     # save total number of birth events on mainland 
    bi <- nrow(off_isl)                      # save total number of birth events on island
    
    # Mutation event
    off_main <- mutation_event(off_main,mu) # mutation event on mainland
    mutm <- nmut                           # save total number of mutation events on mainland
    off_isl <- mutation_event(off_isl,mu)   # mutation event on island
    muti <- nmut                           # save total number of mutation events on island
    
    # Migration event
    off_main <- migration_event(off_main,mr,msrm) # migration event on mainland
    off_isl <- migration_event(off_isl,mr,msri)   # migration event on island
    migm <- length(which(off_main[,8]==1))       # save total number of migration events on mainland
    migi <- length(which(off_isl[,8]==1))        # save total number of migration events on island

    # Local fitness calculation
    for(ind in 1:nrow(off_main)){
      off_main$fit[ind] <- get_fitness(off_main$x[ind],ifelse(off_main$loc[ind]==0,0,dopt),wopt,sigma)
    } # allocate a fitness value to mainland offspring according to their location 
    for(ind in 1:nrow(off_isl)){
      off_isl$fit[ind] <- get_fitness(off_isl$x[ind],ifelse(off_isl$loc[ind]==0,0,dopt),wopt,sigma)
    } # allocate a fitness value to island offspring according to their location 
    
    # Death event
    curr_main <- death_event(curr_main,d) # death event on mainland
    dm <- nindglob                       # total number of death events on mainland
    curr_isl <- death_event(curr_isl,d)   # death event on island
    di <- nindglob                       # total number of death events on island
    
    # check birth rate and death rate
    if(dm>bm){write(paste("Number of death > number of birth on mainland",t),log.path,append=T)}
    if(dm>bm){write(paste("Number of death > number of birth on island",t),log.path,append=T)}
    
    # Competition event 
    comp_main <- competition_event(0,dm,off_main,off_isl) # competition event on mainland
    comp_isl <- competition_event(1,di,off_main,off_isl)  # competition event on island
    ebi <- length(which(comp_isl$mig==0))                  # successful competitors on island that come from the island (natives)
    emigm <- length(which(comp_isl$mig==1))                # successful competitors on island that come from the mainland (migrants)
    ebm <- length(which(comp_main$mig==0))                 # successful competitors on mainland that come from the mainland (natives)
    emigi <- length(which(comp_main$mig==1))               # successful competitors on mainland that come from the island (migrants)
    curr_main <- rbind(curr_main,comp_main)                # add the successful competitors to mainland current population
    curr_isl <- rbind(curr_isl,comp_isl)                   # add the successful competitors to island current population
    pop <- as.matrix(rbind(curr_main,curr_isl))            # total population
    nm <- nrow(curr_main)                                  # total number of individuals on mainland
    ni <- nrow(curr_isl)                                   # total number of individuals on island
    xm <- mean(curr_main$x)                                # average ecological individual trait on mainland
    xi <- mean(curr_isl$x)                                 # average ecological individual trait on island
    
    if(ni!=(pni-di+ebi+emigm)){write(paste("Ni",t-1,"is not coherent with Ni",t), log.path, append=TRUE)}
    if(nm!=(pnm-dm+ebm+emigi)){write(paste("Nm",t-1,"is not coherent with Nm",t), log.path, append=TRUE)}
    # cat("nm:",pnm,"dm:",dm,"bm:",ebm,"mi:",emigi,"SHOULD",pnm-dm+ebm+emigi,"IS",nm,"\t","ni:",pni,"di:",di,"bi:",ebi,"mm:",emigm,"SHOULD",pni-di+ebi+emigm,"IS",ni)
    
    # Save new population
    write.table(pop,file=paste0("results/",sim,"_",run,"_results.txt"),append = T,col.names =F)
    
    # Save metrics summary
    summ <- c(t,nm,ni,xm,xi,bm,bi,mutm,muti,migm,migi,dm,di,ebm,ebi,emigm,emigi)
    write(summ,file=paste0("results/",sim,"_",run,"_summary.txt"),append = T)
    
    # Remove
    rm(off_main,off_isl,comp_main,comp_isl,summ)
          
    print(paste("Time",t,"ok"))
  }
  
  print(paste("RUN",run,"ok"))
}

# Last info about simulation
write(paste(sim,"- end, simulation time:",Sys.time()-start,sep=" "),file=paste0("results/",sim,"_",run,"_description.txt"),append = T)
write(paste(readLines("R/parameters.R",warn=FALSE)),file=paste0("results/",sim,"_",run,"_description.txt"),append = T)
