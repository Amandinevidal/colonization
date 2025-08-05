#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : amandine.vidalhosteng@gmail.com
#### File path : colonization/R/summary_variable.R
#### 
#### Use simulation output (.tar.gz) to summarize key simulation parameters 
#### (e.g. birth rate, death rate, etc.) by computing their averages across replicates
#### and create file "sim_mean_summary.txt"
#### -----------------------------------------------------------------####
rm(list = ls())

# LIBRARY ####
library(dplyr)
library(ggplot2)
library(tidyr)

# FUNCTIONS ####
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
      warning(paste("Parameter", p, "not found."))
    }
  }
}

extract_summary_files <- function(data_files, nsim) {
  summary_files <- grep("_summary\\.txt$", data_files, value = TRUE)
  sim_ids <- as.integer(sub(".*_(\\d+)_summary\\.txt$", "\\1", summary_files))
  summary_list <- vector("list", nsim)
  names(summary_list) <- paste0("sim_", seq_len(nsim))
  for (i in seq_along(summary_files)) {
    id <- sim_ids[i]
    if (!is.na(id) && id >= 1 && id <= nsim) {
      summary_list[[id]] <- summary_files[i]
    }
  }
  return(summary_list)
}

# CODE  ####

big_data <- c()
folder_path <- "results/"
letters_vec <- LETTERS
letters_vec <- letters_vec[-25]
numbers_vec <- c(1, 2)
tar_files <- unlist(lapply(numbers_vec, function(num) {
  paste0("results//", letters_vec, num, ".tar.gz")
}))
# tar_files <- list.files(folder_path, pattern = "\\.tar\\.gz$", full.names = TRUE)

for (tar_file in tar_files) { # START Loop over simulations
  
  sim_name <- sub("\\.tar\\.gz$", "", basename(tar_file))
  
  message("Simulation ", sim_name, " started.")
  
  temp_dir <- tempfile(pattern = "sim_extract_")
  untar(tar_file, exdir = temp_dir)
  data_files <- list.files(temp_dir, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
  log_file <- extract_log_file(data_files)
  read_parameters(log_file)
  summary_files <- extract_summary_files(data_files, nsim)
  
  cols <- c("ignore","t","nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di",
            "ebm","ebi","emigm","emigi","prpm","prpi","prom","proi","prmm","prmi","prcm","prci")
  
  list_dfs <- lapply(summary_files, function(f) {
    read.table(f, header=FALSE, col.names=cols)
  })
  
  for(i in seq_along(list_dfs)) {
    list_dfs[[i]]$replicate <- i
  }
  
  df_all <- do.call(rbind, list_dfs) # combine everything
  df_all <- df_all[,-1]
  
  variables <- setdiff(colnames(df_all), c("t", "replicate"))
  
  df_mean <- df_all %>%
    group_by(t) %>%
    summarise(across(all_of(variables), mean))
  
  df_mean$sim <- sim_name
  df_mean$continent_size <- k
  df_mean$dopt <- dopt
  df_mean$mr <- mr
  
  write.table(df_mean,file=paste0("results/",sim_name,"_mean_summary.txt"),append = F,sep=" ",col.names = F)
  
  output_dir <- paste0("results/plots/summary_",sim_name)
  if(!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  for (var in variables) {
    p <- ggplot(df_mean, aes_string(x = "t", y = var)) +
      geom_line(color = "blue") +
      theme_bw() +
      labs(title = paste("Average", var),
           x = "Time",
           y = var)
    
    ggsave(filename = paste0(output_dir, "/", var, "_mean_over_time.png"),
           plot = p,
           width = 7, height = 5)
  }
  
  big_data <- rbind(big_data, df_mean)
  unlink(temp_dir, recursive = TRUE) # clean temporary dir
  message("Simulation ", sim_name, " over.")
}

write.table(big_data,file=paste0("results/mean_summary.txt"),append = F,sep=" ",col.names = F)

# PLOTS ####
cols <- c("t","nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di",
          "ebm","ebi","emigm","emigi","prpm","prpi","prom","proi","prmm","prmi","prcm","prci","sim","continent_size","dopt","mr")
big_data <- read.table("results/mean_summary.txt",col.names = cols)
vars_to_plot <- c("ebi", "emigm", "xi")
group_vars <- c("dopt", "continent_size", "mr")

df <- big_data %>%
  mutate(across(all_of(group_vars), as.factor))

df <- df %>%
  filter(mr != 0.01)

for (var in vars_to_plot) {
  df_sub <- df %>%
    select(t, all_of(var), all_of(group_vars)) %>%
    rename(value = all_of(var))
  p <- ggplot(df_sub, aes(x = t, y = value, color = dopt)) +
    geom_line() +
    facet_grid(continent_size ~ mr, scales = "free_y") + 
    theme_bw() +
    labs(title = paste0(var),
         x = "Temps (t)",
         y = "Value",
         color = "Continent Size") 
  print(p)
  # ggsave(filename = paste0(var, "_plot.png"), plot = p, width = 8, height = 5)
}


