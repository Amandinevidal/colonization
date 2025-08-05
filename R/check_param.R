#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : amandine.vidalhosteng@gmail.com
#### File path : colonization/R/check_param.R
#### 
#### This file contains a suite of functions designed to test the 
#### correctness of the parameters in your models. 
#### By executing these tests, you can ensure that the simulation is 
#### properly initialized and that the parameters are set as expected.  
#### -----------------------------------------------------------------####

#### Functions ####

log.path <- paste0("results/",sim,"/",sim,"_log.txt")
warning <- function(w) {
  cat("Warning!", w, '\n')
  cont <- readline('Do you wish to continue? [Y/N] ')
  if(cont != 'Y') {stop('Simulation aborted by user')}else{write(w, log.path, append=TRUE)}
}

#### PARAMETERS SET ####
write("Parameter check", log.path, append=TRUE)
if(length(list.files(path="results/",pattern=paste0("results/",sim,"_results.txt")))!=0){w<-"Simulation name already existing, results will be overwritten";warning(w)}
if(nsim<=0) {stop("Error: Number of replicates, nsim, equal to or less than 0")}
if(time<=0) {stop("Error: Simulation time, time, equal to or less than 0")}
if(k<=0) {stop("Error: Empty mainland")}
if(ipk<=0){stop("Error: Empty island")}
if(dopt<0){stop("Error: Difference between mainlannd and island optimums is negative")}
if(wopt<0){stop("Error: Ecological niche width is negative")}
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
