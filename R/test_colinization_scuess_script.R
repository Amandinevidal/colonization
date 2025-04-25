# We want to quantify the success of colonization such as:
#   - the number of (surviving) individuals emerging from a colonization events
#   - the number of generation emerging from a colonization event
# We want to quantify the adaptation of local individuals coming from a colonization event such as:
#   - tracking the average trait of descendant from a colonization events
#   - tracking the average time taken to reach the local optimum within a colonist lineage
# We want to quantify which continental individuals are the one colonizing the most, such as:
#   - what is the trait/fitness value of the colonist and ancestor

# Thus we need a "colonist_data" file, containing from each colonization event these information to be able to plot the average of all these variables for different replicates and to compare it between different set of parameters.
# This file is named "colonist.txt" with the corresponding simulation id, and contains a table with each line being a colonization event with columns describing each event such as:
#   - "id" the colonization name
#   - "colonist" the individual that, as an offspring, migrated from the mainland to the island (only offspring migrate)
#   - "colonist_x" the ecological trait value of the colonist
#   - "colonist_fit_island" the fitness value of the colonist on the island habitat
#   - "colonist_fit_main" the fitness value of the colonist on the mainland
#   - "ancestor" the mainland individual that gave birth to the offspring that migrated
#   - "ancestor_x" the ecological trait value of the colonist
#   - "ancestor_fit_main" the fitness value of the ancestor on the mainland
#   - "time" the time during which the colonist arrived on the island
#   - "nb_des" the number of individuals descended from the colonist individual
#   - "nb_gen" the time (expressed as generations here) of persistence of all individual descendant from this colonization event
#   - "mean_x" the average trait of the entire lineage

rm(list = ls())

#### Library ####
library(dplyr)
library(ggplot2)

#### Functions ####
search_offspring <- function(mother,data) {
  offsprings <- which(data$mother == mother)
  subset <- data[offsprings,]
  return(subset$id)
}
ind_history <- function(ind,data){
  history <- which(data$id == ind)
  subset <- data[history,]
  return(subset)
}
mother <- function(ind,data){
  mother <- data[which(data$id == ind),]$mother
  line <- data[which(data$id == mother),]
  x <- data[line,]$x
  fitness <- data[line,]$fitness
  return(c(mother,x,fitness))
}
time_of_birth <- function(ind,data){
  history <- data[which(data$id == ind),]
  birth_time <- history[which(history$age==0),]$time
  return(birth_time)
}
location_at_t <- function(ind,t,data){
  history <- data[which(data$id == ind),]
  location <- history[which(history$time==t),]$location
  return(location)
}
direct_descendant <- function(ind,data){
  subset <- data[which(data$mother == ind),]
  if(nrow(subset) == 0) {d <- 0} else {d <- subset$id}
  return(d)
}
colonist <- function(data){
  subset <- data[which(data$origin == 0 & data$location == 1 & data$migration == 1),]
  if(nrow(subset) == 0) {c <- 0} else {c <- unique(subset$id)}
}
colonization_time <- function(ind, data) {
  subset <- data[which(data$id == ind),]
  time <- subset[which(subset$age == 0),]$time
  return(time)
}
persistence <- function(ind,data){
  subset <- ind_history(ind,data)
  if(nrow(subset) == sum(subset$migration)) {  # no back migration
    p <- max(subset$age)
  } 
  return(p)
}
search_ancestor <- function(id,phylo) {
  mother <- phylo[which(phylo$species == id),]$mother
  while (mother != 0) {
    new_id <- mother
    mother <- phylo[which(phylo$species == new_id),]$mother
    cat("new_id:", new_id, "mother:",mother,"\n")
  }
  return(new_id)
}
search_ancestor_colonist <- function(id,phylo,colonist) {
  if(phylo[which(phylo$species == id),]$location == 0) { # the individual is on the mainland - no colonization event
    
  }
  col <- ifelse(id %in% colonist$species, TRUE, FALSE)   # is the individual a colonist ?
  if (col) { # the individual is the ancestor colonist of its own lineage
    return(id)
  } else { # find the colonist ancestor
    ancestors <- c()
    mother <- phylo[which(phylo$species == id),]$mother
    ancestors <- c(ancestors,mother)
    while (mother != 0) {
      new_id <- mother
      mother <- phylo[which(phylo$species == new_id),]$mother
      ancestors <- c(ancestors,mother)
    }
    if(any(ancestors %in% colonist)) {
      ancestor_colonist <- intersect(ancestors,colonist)
    } else {
      ancestor_colonist <- NA
    }
    return(ancestor_colonist) 
  }
}
search_trait <- function(ind,data,time){
  line <- data[which(data$id == ind & data$time == time),]
  return(line$x)
}
read_parameters <- function(sim_id) {
  param <- c("nsim","time","k","ipk","dopt","wopt","mr","msri","msrm","d","wmax","sigma","mu")
  lines <- readLines(paste0("results/", sim_id, "_log.txt"))
  for (p in param) {
    line <- grep(paste0("^\\s*", p, "\\s*<-"), lines, value = TRUE)
    if (length(line) == 1) {
      value_str <- sub(paste0(".*", p, "\\s*<-\\s*([0-9\\.eE+-]+).*"), "\\1", line)
      value <- as.numeric(value_str)
      assign(p, value, envir = .GlobalEnv)
    } else {
      warning(paste("Paramètre", p, "non trouvé ou multiple fois défini."))
    }
  }
}
get_fitness <- function(x, opt, wmax, sigma) {
  f <- wmax * exp( - ((x - opt)^2 / sigma^2))
  return(f)
} 

#### Data ####

sim <- 'sim1'
nsim <- 1

# parameters
read_parameters(sim)

# metapopulation data
test <- read.table(paste0("results/",sim,"_",nsim,"_results.txt")) # read data
data <- test[,-1] # remove first column
colnames(data) <- c("id","x","fitness","origin","location","mother","offspring","migration","age","time")
data <- data %>% # add death and birth variables
  group_by(id) %>%
  mutate(
    birth = min(time[age == 0], na.rm = TRUE),
    death = max(time, na.rm = TRUE)
  ) %>%
  ungroup()

# phylogeny data
phylo <- data %>%
  group_by(id) %>%
  summarise(
    birth = first(birth),
    death = first(death),
    mother = first(mother),
    location = first(location)
  ) %>%
  ungroup()

# colonist data 
v_colonist <- colonist(data)
colonist_data <- data.frame(colonist = v_colonist, colonization_id = NA, time = NA)
colonist_data <- colonist_data %>%
  rowwise() %>%
  mutate(time = colonization_time(colonist, data)) %>%
  ungroup() %>%
  arrange(time) %>%
  mutate(colonization_id = row_number()) 

# Variable : colonist trait at colonization time
colonist_data <- colonist_data %>%
  rowwise() %>%
  mutate(colonist_x = search_trait(colonist,data,time))

# Variables: colonist fitness on the mainland and on the island
opt_main <- 0
opt_isl <- 0 + dopt
colonist_data <- colonist_data %>%
  rowwise() %>%
  mutate(colonist_fit_isl = get_fitness(colonist_x,opt_isl,wmax,sigma)) 
colonist_data <- colonist_data %>%
  rowwise() %>%
  mutate(colonist_fit_main = get_fitness(colonist_x,opt_main,wmax,sigma)) 

# Variables: ancestor
colonist_data <- left_join(colonist_data, phylo, by = c("colonist" = "id")) %>%
  rename(ancestor = mother)

# Variable: acestor_x
colonist_data <- colonist_data %>%
  rowwise() %>%
  mutate(ancestor_x = search_trait(ancestor,data,time))



