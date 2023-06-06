#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/test_param.R
#### 
#### This file contains a suite of functions designed to test the 
#### correctness of the parameters in your models. 
#### By executing these tests, you can ensure that the simulation is 
#### properly initialized and that the parameters are set as expected.  
#### -----------------------------------------------------------------####

#### Functions ####

warning <- function(w) {
  cat("Warning!", w, '\n')
  cont <- readline('Do you wish to continue? [Y/N] ')
  if(cont != 'Y') stop('Simulation aborted by user')
}

#### PARAMETERS SET ####
if(length(list.files(path="results/",pattern=sim))!=0){w<-"Simulation name already existing, results will be overwritten";warning(w)}
if(nsim<=0) {stop("Error: Number of replicates, nsim, equal to or less than 0")}
if(time<=0) {stop("Error: Simulation time, time, equal to or less than 0")}
if(km<=0) {stop("Error: Empty mainland")}
if(ki<=0){stop("Error: Empty island")}
if(km<=ki){w<-"Mainland carrying capacity is lower or equal to that of the island";warning(w)}
if(optm<0){stop("Error: Mainland ecological optimum is negative")}
if(opti<0){stop("Error: Island ecological optimum is negative")}
if(woptm<0){stop("Error: Mainland ecological niche width is negative")}
if(wopti<0){stop("Error: Island ecological niche width is negative")}
if(mr==0){w<-"Migration rate is equal to 0";warning(w)}
if(mr<0){stop("Negative migration rate")}
if(msrm==0){w<-"Mainland-island survival migration rate is equal to 0";warning(w)}
if(msri==0){w<-"Island-mainland survival migration rate is equal to 0";warning(w)}
if(msrm<0){stop("Error: Negative mainland-island migration survival rate")}
if(msri<0){stop("Error: Negative Island-mainland migration survival rate")}
if(msrm>=msri){w<-"Mainland-island migration survival rate is equal or greater than island-mainland one";warning(w)}
if(d==0){w<-"Death rate is equal to 0";warning(w)}
if(d<0){stop("Error: Negative death rate")}
if(wmax<=0){stop("Error: Maximum achievable fitness wmax is equal of less than 0")}
if(sigma<=0){stop("Error: Fitness width is equal or less than 0")}
if(mu==0){w<-"Phenotype mutation rate is equal to 0";warning(w)}
if(mu<0){stop("Error: Negative phenotype mutation rate")}
