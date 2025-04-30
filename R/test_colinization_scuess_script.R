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
library(data.table)
library(future.apply)
library(future)

#### Functions ####
extract_log_file <- function(data_files) {
  log_file <- data_files[grepl("_log\\.txt$", data_files)]
  if (length(log_file) == 0) {
    stop("Aucun fichier log trouvé !")
  }
  return(log_file)
}
read_parameters <- function(log_file) {
  lines <- readLines(log_file)
  param <- c("nsim", "time", "k", "ipk", "dopt", "wopt", "mr", "msri", "msrm", "d", "wmax", "sigma", "mu")
  
  for (p in param) {
    # Cherche la ligne contenant l'assignation du paramètre
    line <- grep(paste0("^\\s*", p, "\\s*<-"), lines, value = TRUE)
    
    if (length(line) == 1) {
      # Evaluer l'expression directement
      expression <- sub(".*<-\\s*(.*)", "\\1", line)  # Extraction de l'expression
      value <- eval(parse(text = expression))  # Evaluer l'expression en R
      assign(p, value, envir = .GlobalEnv)
    } else {
      warning(paste("Paramètre", p, "non trouvé ou multiple fois défini."))
    }
  }
}
extract_results_files <- function(data_files, nsim) {
  result_files <- grep("_results\\.txt$", data_files, value = TRUE)
  sim_ids <- as.integer(sub(".*_(\\d+)_results\\.txt$", "\\1", result_files))
  results_list <- vector("list", nsim)
  names(results_list) <- paste0("sim_", seq_len(nsim))
  for (i in seq_along(result_files)) {
    id <- sim_ids[i]
    if (!is.na(id) && id >= 1 && id <= nsim) {
      results_list[[id]] <- result_files[i]
    }
  }
  return(results_list)
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
search_trait <- function(ind,data,time){
  line <- data[which(data$id == ind & data$time == time),]
  if(nrow(line) == 0) { # individual died at this time, search before
    line <- data[which(data$id == ind & data$time == time - 1),]
  }
  return(line$x)
}
get_fitness <- function(x, opt, wmax, sigma) {
  f <- wmax * exp( - ((x - opt)^2 / sigma^2))
  return(f)
} 
get_colonization_id <- function(spp, location, phylo_dt, colonist_dt) {
  # if mainland indiviudal no colonization event
  if (location == 0) return(NA) 
  # if insular individual search all ancestors
  ancestors <- c(spp) 
  m <- phylo_dt[id == spp, mother]
  ancestors <- c(ancestors, m)
  while (m != 0) {
    m <- phylo_dt[id == m, mother]
    ancestors <- c(ancestors, m)
  }
  # is there a colonist in the ancestors
  matching <- colonist_dt[colonist %in% ancestors]
  if (nrow(matching) > 0) {
    first_match <- matching[match(colonist, matching$colonist, nomatch = 0)[1], ]
    return(first_match$colonization_id)
  } else {
    return(as.numeric(0))
  }
}
nb_generation <- function(colonization) {
  time1 <- colonist_data[which(colonist_data$colonization_id == colonization),]$time
  des <- phylo[which(phylo$colonization_id == colonization),]
  time2 <- max(des$death)
  return(time2 - time1)
}
average_x <- function(colonization) {
  des <- phylo[which(phylo$colonization_id == colonization),]
  subset_data <- data %>%
    filter(id %in% des$id)
  m <- mean(subset_data$x)
  return(m)
}

#### Code ####

# Results directory
folder_path <- "results/"

# List files .tar
tar_files <- list.files(folder_path, pattern = "\\.tar.gz$", full.names = TRUE)

for (tar_file in tar_files) { # Loop over simulations
  # Sim name without extension
  sim_name <- sub("\\.tar\\.gz$", "", basename(tar_file))
  
  # Temporary directory for file extraction
  temp_dir <- tempfile(pattern = "sim_extract_")
  dir.create(temp_dir)
  
  # Extraction
  untar(tar_file, exdir = temp_dir)
  # print(list.files(temp_dir, recursive = TRUE))  
  
  # Read files
  data_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  
  # Read and load simulation parameters
  log_file <- extract_log_file(data_files)
  read_parameters(log_file)
  
  # Create a list for results files
  results_files <- extract_results_files(data_files, nsim)
  
  for (i in seq_len(nsim)) {  # Loop over replicates
    
    file <- results_files[[i]]
    if (!is.null(file)) {
      
      # Read data
      data <- read.table(file)
      data <- data[,-1]
      colnames(data) <- c("id","x","fitness","origin","location","mother","offspring","migration","age","time")
      
      # Add death and birth variables
      data <- data %>% 
        group_by(id) %>%
        mutate(
          birth = min(time[age == 0], na.rm = TRUE),
          death = max(time +1 , na.rm = TRUE) # careful, death time is not the last we observe in table data, it is this time+1 
        ) %>%
        ungroup()
      
      # Phylogeny data
      phylo <- data %>%
        group_by(id) %>%
        summarise(
          birth = first(birth),
          death = first(death),
          mother = first(mother),
          location = first(location)
        ) %>%
        ungroup()
      
      # Colonist data 
      v_colonist <- colonist(data)
      colonist_data <- data.frame(colonist = v_colonist, colonization_id = NA, time = NA)
      colonist_data <- colonist_data %>%
        rowwise() %>%
        mutate(time = colonization_time(colonist, data)) %>%
        ungroup() %>%
        arrange(time) %>%
        mutate(colonization_id = row_number()) 
      
      # Convertir data et colonist_data en data.tables
      data_dt <- as.data.table(data)
      colonist_dt <- as.data.table(colonist_data)
      phylo_dt <- as.data.table(phylo)
      
      # Index pour recherche rapide
      setkey(data_dt, id, time)
      
      # Variable : colonist trait at colonization time
      # HOW TO - DATA.TABLE SQUARE BRACKET WORKFLOW: Structure DT[i, j := valeur] 
      # We want to look at every lines so i is empty here
      # := means we do a juncture between colonist_dt and data_dt which is a SD (subset of data) based on:
      # lines in data_dt which "id" corresponds to "colonist" in colonist_dt
      # and "time" in data_dt corresponds to "time" in colonist_dt
      # for each line of colonist_dt we take the value in the colmun "x" of data_dt
      colonist_dt[, colonist_x := data_dt[.SD, on = .(id = colonist, time = time), x]]
      
      n_missing <- colonist_dt[is.na(colonist_x), .N]
      if (n_missing > 0) {
        warning(n_missing, " missing trait for colonists ")
      }
      
      # Variables: colonist fitness on the mainland and on the island
      opt_main <- 0
      opt_isl <- 0 + dopt
      colonist_dt[, colonist_fit_isl := get_fitness(colonist_x, opt_isl, wmax, sigma)]
      colonist_dt[, colonist_fit_main := get_fitness(colonist_x, opt_main, wmax, sigma)]
      
      # Variables: ancestor
      colonist_dt <- merge(colonist_dt, 
                           phylo_dt[, .(id, mother)], 
                           by.x = "colonist", 
                           by.y = "id", 
                           all.x = TRUE)
      setnames(colonist_dt, "mother", "ancestor")
      
      # Variable: ancestor_x
      colonist_dt[, time_shifted := time - 1]
      colonist_dt[, ancestor_x := data_dt[.SD, on = .(id = ancestor, time = time), x]]
      colonist_dt[is.na(ancestor_x), 
                  ancestor_x := data_dt[.SD, on = .(id = ancestor, time = time_shifted), x]]
      colonist_dt[, time_shifted := NULL]
      
      # variable: ancestor_fit_main
      colonist_dt[, ancestor_fit_main := get_fitness(ancestor_x, opt_main, wmax, sigma)]
      
      # Variable: nb_des
      plan(multicore) # Définir la stratégie de parallélisation (par exemple, multi-core)
      phylo_dt$colonization_id <- future_mapply(function(spp, location) {
        get_colonization_id(spp, location, phylo_dt, colonist_dt)
      }, phylo_dt$id, phylo_dt$location, SIMPLIFY = TRUE)
      result <- phylo_dt[, .(nb_des = .N), by = colonization_id]
      colonist_data <- merge(colonist_data, result, by = "colonization_id", all.x = TRUE)
      
      # Variable: nb gen
      colonist_data <-colonist_data %>%
        rowwise() %>%
        mutate(nb_gen = nb_generation(colonization_id))
      
      # Variable: average_x
      colonist_data <- colonist_data %>%
        rowwise() %>%
        mutate(average_x = average_x(colonization_id))
      
    } else { # no file for simulation
      message(sprintf("Simulation %d encore absente ou incomplète", i))
    }
    
  } # END Loop over replicates
  
  # Clean
  unlink(temp_dir, recursive = TRUE)
  
} # END Loop over simulations



for (n in 1:nsim) { # REPLICATE LOOP ####
  
  # metapopulation data
  test <- read.table(paste0("results/",sim,"_",n,"_results.txt")) # read data
  data <- test[,-1] # remove first column
  colnames(data) <- c("id","x","fitness","origin","location","mother","offspring","migration","age","time")
  data <- data %>% # add death and birth variables
    group_by(id) %>%
    mutate(
      birth = min(time[age == 0], na.rm = TRUE),
      death = max(time +1 , na.rm = TRUE) # careful, death time is not the last we observe in table data, it is this time+1 
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
  colonist_data <- left_join(
    colonist_data,
    phylo %>% select(id, mother),  # <-- select only "id" and "mother"
    by = c("colonist" = "id")
  ) %>%
    rename(ancestor = mother)
  
  # Variable: ancestor_x
  colonist_data <- colonist_data %>%
    rowwise() %>%
    mutate(ancestor_x = search_trait(ancestor,data,time))
  
  # variable: ancestor_fit_main
  colonist_data <- colonist_data %>%
    rowwise() %>%
    mutate(ancestor_fit_main = get_fitness(ancestor_x,opt_main,wmax,sigma)) 
  
  # Variable: nb_des
  phylo <- phylo %>%
    rowwise() %>%
    mutate(colonization_id = search_col_id(id,phylo,colonist_data))
  result <- phylo %>%
    group_by(colonization_id) %>%
    summarise(nb_des = n())
  colonist_data <- colonist_data %>%
    left_join(result, by = "colonization_id")
  
  # Variable: nb gen
  colonist_data <-colonist_data %>%
    rowwise() %>%
    mutate(nb_gen = nb_generation(colonization_id))
  
  # Variable: average_x
  colonist_data <- colonist_data %>%
    rowwise() %>%
    mutate(average_x = average_x(colonization_id))
  
} # END REPLICATE LOOP ####
