#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/main.R
#### 
#### Main file: contains the R code for the individual based model
#### -----------------------------------------------------------------####

rm(list = ls())
library(tidyverse)
source("parameters.R") 
source("functions.R") 

#### SIMULATIONS ####

# Initliazation
data_pop <- pop_init(km,ki,optm,opti,woptm,wopti) # initalize the total population
curr_main <- data_pop[which(data_pop$loc==0),] # current mainland population
curr_isl <- data_pop[which(data_pop$loc==1),] # currant island population
ktot <- km+ki # total number of individuals (helps to name the new individuals after)

for (t in 1:time){ # TIME loop
  
  # Birth event (+ mutation)
  mainland_off <- birth_event(curr_main,mu,ktot,t) 
  ktot <- ktot + nrow(mainland_off)
  island_off <- birth_event(curr_isl,mu,ktot,t)
  ktot <- ktot + nrow(island_off)
  
  print(paste("BE:",nrow(mainland_off),"main -",nrow(island_off),"isl"))
  
  # Migration event
  mainland_off <- migration_event(mainland_off,mr,msr) # ça marche pas :)
  island_off <- migration_event(island_off,mr,msr) # là non plus :)
  
  
  print(paste("ME:",length(which(mainland_off$mig==1))))
  
  
}
