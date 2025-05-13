#### PLOTS ####
rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)

# color <- c("A1" = "blue", "B1" = "red", "C1" = "green", "D1" = "purple")
param_glossary <- data.frame(sim_id = NA, k = NA, dopt = NA, mr = NA)
param_glossary[1,] <- c("A1",500,0,0.5)
param_glossary[2,] <- c("B1",1000,0,0.5)
param_glossary[3,] <- c("C1",2000,0,0.5)
param_glossary[4,] <- c("D1",4000,0,0.5)
param_glossary[5,] <- c("E1",5000,0,0.5)
param_glossary[6,] <- c("F1",500,1,0.5)
param_glossary[7,] <- c("G1",1000,1,0.5)
param_glossary[8,] <- c("H1",2000,1,0.5)
param_glossary[9,] <- c("I1",4000,1,0.5)
param_glossary[10,] <- c("J1",5000,1,0.5)
param_glossary[11,] <- c("K1",500,2,0.5)
param_glossary[12,] <- c("L1",1000,2,0.5)
param_glossary[13,] <- c("M1",2000,2,0.5)
param_glossary[14,] <- c("N1",4000,2,0.5)
param_glossary[15,] <- c("O1",5000,2,0.5)
param_glossary[16,] <- c("P1",500,5,0.5)
param_glossary[17,] <- c("Q1",1000,5,0.5)
param_glossary[18,] <- c("R1",2000,5,0.5)
param_glossary[19,] <- c("S1",4000,5,0.5)
param_glossary[20,] <- c("T1",5000,5,0.5)
param_glossary[21,] <- c("U1",500,10,0.5)
param_glossary[22,] <- c("V1",1000,10,0.5)
param_glossary[23,] <- c("W1",2000,10,0.5)
param_glossary[24,] <- c("X1",4000,10,0.5)
param_glossary[25,] <- c("Z1",5000,10,0.5)
param_glossary[26,] <- c("A2",500,0,0.2)
param_glossary[27,] <- c("B2",1000,0,0.2)
param_glossary[28,] <- c("C2",2000,0,0.2)
param_glossary[29,] <- c("D2",4000,0,0.2)
param_glossary[30,] <- c("E2",5000,0,0.2)
param_glossary[31,] <- c("F2",500,1,0.2)
param_glossary[32,] <- c("G2",1000,1,0.2)
param_glossary[33,] <- c("H2",2000,1,0.2)
param_glossary[34,] <- c("I2",4000,1,0.2)
param_glossary[35,] <- c("J2",5000,1,0.2)

# DATA ####
folder_path <- "results/"
tar_files <- list.files(folder_path, pattern = "\\.tar\\.gz$", full.names = TRUE)
data <- data.table()

for (tar_file in tar_files) { # START Loop over simulations
  sim_name <- sub("\\.tar\\.gz$", "", basename(tar_file))
  all_colonist <- rbindlist(lapply(1:30, function(i) {
    file <- paste0(folder_path, sim_name, "_", i, "_colonist.txt")
    if (file.exists(file)) {
      dt <- fread(file)
      dt[, replicate := i]
      dt[, sim_id := sim_name]
      return(dt)
    } else {
      return(NULL) # ou data.table() pour rester explicite
    }
  }), use.names = TRUE, fill = TRUE)
  data <- rbind(data, all_colonist, use.names = TRUE, fill = TRUE)
}
data[is.na(p_des), p_des := 0]

# DATA SUBSET ####

# data_size_effet = same habitat increasing size
select <- param_glossary[which(param_glossary$dopt == 0 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_hm <- data[sim_id %in% select]
data_size_effect_hm <- merge(data_size_effect_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_hm)[names(data_size_effect_hm) == "k"] <- "continent_size"

# data_size_effet = same habitat increasing size + LOW MIG
select <- param_glossary[which(param_glossary$dopt == 0 & param_glossary$mr == 0.2),]$sim_id
data_size_effect_lm <- data[sim_id %in% select]
data_size_effect_lm <- merge(data_size_effect_lm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_lm)[names(data_size_effect_lm) == "k"] <- "continent_size"

# data_size_effect_hab1
select <- param_glossary[which(param_glossary$dopt == 1 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h1_hm <- data[sim_id %in% select] 
data_size_effect_h1_hm <- merge(data_size_effect_h1_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h1_hm)[names(data_size_effect_h1_hm) == "k"] <- "continent_size"

# data_size_effect_hab1 + LOW MIG
select <- param_glossary[which(param_glossary$dopt == 1 & param_glossary$mr == 0.2),]$sim_id
data_size_effect_h1_lm <- data[sim_id %in% select] 
data_size_effect_h1_lm <- merge(data_size_effect_h1_lm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h1_lm)[names(data_size_effect_h1_lm) == "k"] <- "continent_size"

# data_size_effect_hab2
select <- param_glossary[which(param_glossary$dopt == 2 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h2_hm <- data[sim_id %in% select] 
data_size_effect_h2_hm <- merge(data_size_effect_h2_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h2_hm)[names(data_size_effect_h2_hm) == "k"] <- "continent_size"

# data_size_effect_hab5
select <- param_glossary[which(param_glossary$dopt == 5 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h5_hm <- data[sim_id %in% select] 
data_size_effect_h5_hm <- merge(data_size_effect_h5_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h5_hm)[names(data_size_effect_h5_hm) == "k"] <- "continent_size"

# data_size_effect_hab10
select <- param_glossary[which(param_glossary$dopt == 10 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h10_hm <- data[sim_id %in% select] 
data_size_effect_h10_hm <- merge(data_size_effect_h10_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h10_hm)[names(data_size_effect_h10_hm) == "k"] <- "continent_size"

# data_hab_effect = same size increasing habitat difference
select <- param_glossary[which(param_glossary$k == 500 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_hm <- data[sim_id %in% select] 
data_hab_effect_hm <- merge(data_hab_effect_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)
names(data_hab_effect_hm)[names(data_hab_effect_hm) == "k"] <- "continent_size"

# data_hab_effect_s0.5 = size 0.5 increased habitat difference
select <- param_glossary[which(param_glossary$k == 1000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.5_hm <- data[sim_id %in% select] 
data_hab_effect_s0.5_hm <- merge(data_hab_effect_s0.5_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)
names(data_hab_effect_s0.5_hm)[names(data_hab_effect_s0.5_hm) == "k"] <- "continent_size"

# data_hab_effect_s0.25 = size 0.25 increased habitat difference
select <- param_glossary[which(param_glossary$k == 2000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.25_hm <- data[sim_id %in% select] 
data_hab_effect_s0.25_hm <- merge(data_hab_effect_s0.25_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)
names(data_hab_effect_s0.25_hm)[names(data_hab_effect_s0.25_hm) == "k"] <- "continent_size"

# data_hab_effect_s0.125 = size 0.125 increased habitat difference
select <- param_glossary[which(param_glossary$k == 4000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.125_hm <- data[sim_id %in% select] 
data_hab_effect_s0.125_hm <- merge(data_hab_effect_s0.125_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)
names(data_hab_effect_s0.125_hm)[names(data_hab_effect_s0.125_hm) == "k"] <- "continent_size"

# data_hab_effect_s0.1 = size 0.1 increased habitat difference
select <- param_glossary[which(param_glossary$k == 5000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.1_hm <- data[sim_id %in% select] 
data_hab_effect_s0.1_hm <- merge(data_hab_effect_s0.1_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)
names(data_hab_effect_s0.1_hm)[names(data_hab_effect_s0.1_hm) == "k"] <- "continent_size"

# NB_DES GROUPS ####
breaks <- c(0, 1, 3, 10, 15, 20,Inf)
labels <- c("0", "1-2", "3-10", "10-15", "15-20", "20+")
cut_variable_groups <- function(dt, var, new_col, breaks, labels) {
  # Vérifie la validité des inputs
  if (length(breaks) - 1 != length(labels)) {
    stop("Le nombre de labels doit être égal à length(breaks) - 1.")
  }
  dt[, (new_col) := cut(get(var),
                        breaks = breaks,
                        labels = labels,
                        right = FALSE,
                        include.lowest = TRUE)]
}

cut_variable_groups(data_size_effect_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)


# P_DES GROUPS ####
breaks <- c(0, 10, 20, 50, 100, Inf)
labels <- c("0–10", "10–20", "20–50", "50–100", "100+")
cut_variable_groups(data_size_effect_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)

# SIZE EFFECT ####
data_size_effect_hm$continent_size <- factor(
  data_size_effect_hm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_hm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 0 MR = 0.5",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()
               
p <- ggplot(data_size_effect_hm, aes(x = nb_des_group, fill = as.factor(continent_size))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(
    title = "DOPT = 0 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_hm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(
    title = "DOPT = 0 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT + LOW MIG ####
data_size_effect_lm$continent_size <- factor(
  data_size_effect_lm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_lm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 0 MR = 0.2",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(
    title = "DOPT = 0 ; MR = 0.2",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(
    title = "DOPT = 0 ; MR = 0.2",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_lm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT HABITAT DIFF 1####
data_size_effect_h1_hm$continent_size <- factor(
  data_size_effect_h1_hm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h1_hm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 1 MR = 0.5",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h1_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h1_hm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 1 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h1_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h1_hm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 1 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_h1_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT HABITAT DIFF 1 + LOW MIG####
data_size_effect_h1_lm$continent_size <- factor(
  data_size_effect_h1_lm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h1_lm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 1 MR = 0.2",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h1_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h1_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 1 ; MR = 0.2",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h1_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h1_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 1 ; MR = 0.2",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_h1_lm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT HABITAT DIFF 2####
data_size_effect_h2_hm$continent_size <- factor(
  data_size_effect_h2_hm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h2_hm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 2 MR = 0.5",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h2_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h2_hm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 2 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h2_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h2_hm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 2 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_h2_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT HABITAT DIFF 5 ####
data_size_effect_h5_hm$continent_size <- factor(
  data_size_effect_h5_hm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h5_hm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 5 MR = 0.5",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h5_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_hm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h5_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_hm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_h5_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# SIZE EFFECT HABITAT DIFF 10 ####
data_size_effect_h10_hm$continent_size <- factor(
  data_size_effect_h10_hm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h10_hm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 10 MR = 0.5",
       fill = "Continent size") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h10_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h10_hm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 10 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/size_effect_h10_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h10_hm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 10 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/size_effect_h10_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# HAB EFFECT ####
data_hab_effect_hm$dopt <- factor(
  data_hab_effect_hm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_hm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 500 MR = 0.5",
       fill = "Habitat difference") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/data_hab_effect_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_hm, aes(x = nb_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 500 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/habitat_effect_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_hm, aes(x = p_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 500 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/hab_effect_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# HAB EFFECT - SIZE1000 ####
data_hab_effect_s0.5_hm$dopt <- factor(
  data_hab_effect_s0.5_hm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_s0.5_hm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 1000 MR = 0.5",
       fill = "Habitat difference") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/data_hab_effect_size_1000_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.5_hm, aes(x = nb_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 1000 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/habitat_effect_size_1000_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.5_hm, aes(x = p_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 1000 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/hab_effect_size_1000_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# HAB EFFECT - SIZE2000 ####
data_hab_effect_s0.25_hm$dopt <- factor(
  data_hab_effect_s0.25_hm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_s0.25_hm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 2000 MR = 0.5",
       fill = "Habitat difference") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/data_hab_effect_size_2000_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.25_hm, aes(x = nb_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 2000 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/habitat_effect_size_2000_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.25_hm, aes(x = p_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 2000 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/hab_effect_size_2000_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# HAB EFFECT - SIZE4000 ####
data_hab_effect_s0.125_hm$dopt <- factor(
  data_hab_effect_s0.125_hm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_s0.125_hm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 4000 MR = 0.5",
       fill = "Habitat difference") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/data_hab_effect_size_4000_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.125_hm, aes(x = nb_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 4000 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/habitat_effect_size_4000_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.125_hm, aes(x = p_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 4000 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/hab_effect_size_4000_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()

# HAB EFFECT - SIZE5000 ####
data_hab_effect_s0.1_hm$dopt <- factor(
  data_hab_effect_s0.1_hm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_s0.1_hm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 5000 MR = 0.5",
       fill = "Habitat difference") +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )

png("results/plots/data_hab_effect_size_5000_hm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.1_hm, aes(x = nb_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 5000 ; MR = 0.5",
    x = "Number of descendants",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/habitat_effect_size_5000_hm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_hab_effect_s0.1_hm, aes(x = p_des_group, fill = as.factor(dopt))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(
    title = "Continent size = 5000 ; MR = 0.5",
    x = "Time persistence",
    y = "Events",
    fill = "Habitat difference"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),               # taille générale du texte
    plot.title = element_text(size = 20, face = "bold"),  # titre du graphique
    axis.title = element_text(size = 20),         # titres des axes
    axis.text = element_text(size = 20),          # texte des axes
    legend.text = element_text(size = 20),        # texte de la légende
    legend.title = element_text(size = 20)        # titre de la légende
  )
png("results/plots/hab_effect_size_5000_hm_hist_p_des.png",width=1000,height = 800)
print(p)
dev.off()



# SUMMARY DES VARIABLES IMPORTANTES
summary_dt <- data_size_effect[, .(
  mean_nb_des = mean(nb_des,na.rm=T),
  max_nb_des = max(nb_des,na.rm=T),
  min_nb_des = min(nb_des,na.rm=T),
  mean_p_des = mean(p_des,na.rm=T),
  max_p_des = max(p_des,na.rm=T),
  min_p_des = min(p_des,na.rm=T),
  mean_av_x = mean(av_x,na.rm=T),
  max_av_x = max(av_x,na.rm=T),
  min_av_x = min(av_x,na.rm=T)
), by = .(continent_size)]

# PLOT SUMMARY VAR
ggplot(summary_dt, aes(x = taille_pop, y = mean_nb_des, color = ecart_optimum)) +
  geom_line() +
  geom_point() +
  labs(x = "Taille de la population source", y = "Nb moyen de descendants",
       title = "Effet de la taille de population et de l’écart écologique")

# TEST EN BOXPLOT
ggplot(all_colonist, aes(x = sim_id, y = nb_des)) +
  geom_boxplot() +
  labs(x = "Simulation", y = "Nombre de descendants",
       title = "Succès de colonisation par simulation")
















# OLD CODE ####


library(ggplot2)
library(ggridges)

#### FUNCTIONS ####
mean_ci <- function(vec){ # calcul mean and confidence interval for each line (time)
  result <- c(NA,NA,NA)
  if(!is.null(vec)){
    mean <- mean(vec,na.rm=T) # mean
    sd <- sd(vec,na.rm=T) # standard deviation
    se <- sd/(sqrt(length(vec))-1) # standard error
    ci <- 1.96*se # confidence interval
    result[1] <- mean # save for each time
    result[2] <- mean+ci # save upper values ci
    result[3] <- mean-ci # save lower values ci
  }
  return(result) # return a table of mean and ci values for each time
}

select_column <- function(column,mylist) { # for each column (each var) concatenate values and calculate mean and confidence interval
  ref_column <- mylist[[1]][,column]
  concatenate <- ref_column
  for (i in 2:length(mylist)) {
    take_column <- mylist[[i]][1:length(ref_column),column]
    concatenate <- cbind(concatenate,take_column)
  }
  mean_df <- data.frame(mean=rep(NA,length(ref_column)),ciupp=NA,cidown=NA)
  for(i in 1:dim(concatenate)[1]){
    v <- mean_ci(concatenate[i,])
    mean_df[i,]$mean <- v[1]
    mean_df[i,]$ciupp <- v[2]
    mean_df[i,]$cidown <- v[3]
  }
  return(mean_df) # vecteur avec les bonnes infos info <- (mean, ci, sd)
}

#### DATA GENE ####
sim <- "sim14"
run <- seq(1,2)
list_files <- list()
for(r in run){
  file  <- read.table(paste0("results/",sim,"_",r,"_summary.txt"))
  list_files[[r]] <- file[,-1]
}
var <- c("nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di","ebm","ebi","emigm","emigi","prpm","prpi","prom","proi","prmm","prmi","prcm","prci")
mean_df_fin <- data.frame(time=seq(1,250))
for(v in 1:length(var)){
  c <- select_column(v+1,list_files)
  colnames(c) <- c(paste0(var[v],"_mean"),paste0(var[v],"_ciupp"),paste0(var[v],"_cidown"))
  mean_df_fin <- cbind(mean_df_fin,c)
}

write.table(mean_df_fin,file=paste0("results/",sim,"_summary_mean.txt"),sep=" ",col.names = T)

#verify means
# mean_df_fin[1,]$xm_mean # -0.002526871
# mean_df_fin[1,]$muti_mean # 18
# mean_df_fin[1,]$emigm_mean #35.1
# testxm <- c()
# testmuti <- c()
# testemigm <- c()
# for(i in seq(1,10)){
#   testemigm <- c(testemigm,list_files[[i]][1,16])
# }
# mean(testemigm)


#### DATASET ####
# variables average over time in summary file
# "nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di","ebm","ebi","emigm","emigi","prpm","prpi","prom","proi","prmm","prmi","prcm","prci"
baseline <- read.table("results/sim2_summary_mean.txt")
baseline$probacolm <- baseline$emigm_mean/baseline$migm_mean
baseline$probacoli <- baseline$emigi_mean/baseline$migi_mean
longtime <- read.table("results/sim3_summary_mean.txt")
diffopt2 <- read.table("results/sim4_summary_mean.txt")
diffopt4 <- read.table("results/sim6_summary_mean.txt")
bigpop <- read.table("results/sim8_summary_mean.txt")
diffopt0.5 <- read.table("results/sim10_summary_mean.txt")
nomig <- read.table("results/sim11_summary_mean.txt")
baselinewopt <- read.table("results/sim12_summary_mean.txt")
baselinewopt$probacolm <- baselinewopt$emigm_mean/baselinewopt$migm_mean
baselinewopt$probacoli <- baselinewopt$emigi_mean/baselinewopt$migi_mean
baselinewopt1d <- read.table("results/sim13_summary_mean.txt")
baselinewopt1d$probacolm <- baselinewopt1d$emigm_mean/baselinewopt1d$migm_mean
baselinewopt1d$probacoli <- baselinewopt1d$emigi_mean/baselinewopt1d$migi_mean
baselinewopt2d <- read.table("results/sim14_summary_mean.txt")
baselinewopt2d$probacolm <- baselinewopt2d$emigm_mean/baselinewopt2d$migm_mean
baselinewopt2d$probacoli <- baselinewopt2d$emigi_mean/baselinewopt2d$migi_mean

# variables at each time in results file
# "i","x","fit","or","loc","mom","off","mig","age","t"
baseline.pop <- read.table("results/sim2_1_results.txt")
baseline.pop <- baseline.pop[,-1]
colnames(baseline.pop) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
baseline.popinit <- baseline.pop[which(baseline.pop$t==0),]
baseline.popmid <- baseline.pop[which(baseline.pop$t==50),]

baselinewopt.pop <- read.table("results/sim12_1_results.txt")
baselinewopt.pop <- baselinewopt.pop[,-1]
colnames(baselinewopt.pop) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
baselinewopt.pop.mig <- subset(baselinewopt.pop,mig==1)
baselinewopt.pop.mig.sub <- subset(baselinewopt.pop.mig,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt.pop.sub <- subset(baselinewopt.pop,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt.pop.main <- subset(baselinewopt.pop,loc==0)
baselinewopt.pop.main.sub <- subset(baselinewopt.pop.main,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt.pop.isl <- subset(baselinewopt.pop,loc==1)
baselinewopt.pop.isl.sub <- subset(baselinewopt.pop.isl,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)

longtime.popinit <- read.table("results/sim3_1_results.txt")
longtime.popinit <- longtime.popinit[,-1]
colnames(longtime.popinit) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
longtime.popinit <- longtime.popinit[which(longtime.popinit$t==0),]
diffopt0.5.pop <- read.table("results/sim10_1_results.txt")
diffopt0.5.popinit <- diffopt0.5.popinit[,-1]
colnames(diffopt0.5.popinit) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
diffopt0.5.popinit <- diffopt0.5.popinit[which(diffopt0.5.popinit$t==0),]
diffopt2.popinit <- read.table("results/sim4_1_results.txt")
diffopt2.popinit <- diffopt2.popinit[,-1]
colnames(diffopt2.popinit) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
diffopt2.popinit <- diffopt2.popinit[which(diffopt2.popinit$t==0),]
diffopt4.pop <- read.table("results/sim6_1_results.txt")
diffopt4.pop <- diffopt4.pop[,-1]
colnames(diffopt4.pop) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
diffopt4.popinit <- diffopt4.pop[which(diffopt4.pop$t==0),]
bigpop.popinit <- read.table("results/sim8_1_results.txt")
bigpop.popinit <- bigpop.popinit[,-1]
colnames(bigpop.popinit) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
bigpop.popinit <- bigpop.popinit[which(bigpop.popinit$t==0),]

baselinewopt2d.pop <- read.table("results/sim14_1_results.txt")
baselinewopt2d.pop <- baselinewopt2d.pop[,-1]
colnames(baselinewopt2d.pop) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
baselinewopt2d.pop.mig <- subset(baselinewopt2d.pop,mig==1)
baselinewopt2d.pop.mig.sub <- subset(baselinewopt2d.pop.mig,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt2d.pop.sub <-subset(baselinewopt2d.pop,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt2d.pop.main <- subset(baselinewopt2d.pop,loc==0)
baselinewopt2d.pop.main.sub <-subset(baselinewopt2d.pop.main,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt2d.pop.isl <- subset(baselinewopt2d.pop,loc==1)
baselinewopt2d.pop.isl.sub <-subset(baselinewopt2d.pop.isl,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)

# dopt1
baselinewopt1d.pop <- read.table("results/sim13_1_results.txt")
baselinewopt1d.pop <- baselinewopt1d.pop[,-1]
colnames(baselinewopt1d.pop) <- c("i","x","fit","or","loc","mom","off","mig","age","t")
baselinewopt1d.pop.mig <- subset(baselinewopt1d.pop,mig==1)
baselinewopt1d.pop.mig.sub <- subset(baselinewopt1d.pop.mig,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt1d.pop.sub <- subset(baselinewopt1d.pop,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt1d.pop.main <- subset(baselinewopt1d.pop,loc==0)
baselinewopt1d.pop.main.sub <-subset(baselinewopt1d.pop.main,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)
baselinewopt1d.pop.isl <- subset(baselinewopt1d.pop,loc==1)
baselinewopt1d.pop.isl.sub <-subset(baselinewopt1d.pop.isl,t==0|t==30|t==60|t==90|t==120|t==150|t==180|t==210|t==240)

#### HYPOTHESIS 1 ####
colors <- c("Mainland"="#E69F00","Island"="#0072B2")
h1.1 <- ggplot2::ggplot(data = baselinewopt, aes(x=time,y=probacolm)) +
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=probacolm,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=probacoli,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Probability of successful colonization", color="model")+
  ylim(c(0,0.5))+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/proba_col_baseline.png",width=800,height = 800)
print(h1.1)
dev.off()
h1.1b <- ggplot2::ggplot(data = baselinewopt1d, aes(x=time,y=probacolm)) +
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=probacolm,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=probacoli,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Probability of successful colonization", color="model")+
  ylim(c(0,0.5))+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/proba_col_baselined1.png",width=800,height = 800)
print(h1.1b)
dev.off()
h1.1c <- ggplot2::ggplot(data = baselinewopt2d, aes(x=time,y=probacolm)) +
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=probacolm,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=probacoli,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Probability of successful colonization", color="model")+
  ylim(c(0,0.5))+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/proba_col_baselined2.png",width=800,height = 800)
print(h1.1c)
dev.off()

h1.2 <- ggplot2::ggplot(data = baselinewopt, aes(x=time,y=migm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=migm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=migi_mean,color="Island"),linewidth=1) +
  ylim(c(0,160))+
  ggplot2::labs(x="Time (in generation)", y="Propagule pressure", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/propagpre_baseline.png",width=800,height = 800)
print(h1.2)
dev.off()
h1.2b <- ggplot2::ggplot(data = baselinewopt1d, aes(x=time,y=migm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=migm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=migi_mean,color="Island"),linewidth=1) +
  ylim(c(0,160))+
  ggplot2::labs(x="Time (in generation)", y="Propagule pressure", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/propagpre_baseline1d.png",width=800,height = 800)
print(h1.2b)
dev.off()
h1.2c <- ggplot2::ggplot(data = baselinewopt2d, aes(x=time,y=migm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=migm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=migi_mean,color="Island"),linewidth=1) +
  ylim(c(0,160))+
  ggplot2::labs(x="Time (in generation)", y="Propagule pressure", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 40),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/propagpre_baseline2d.png",width=800,height = 800)
print(h1.2c)
dev.off()

h1.3 <- ggplot2::ggplot(data = baselinewopt, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=emigm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=emigi_mean,color="Island"),linewidth=1) +
  ylim(c(0,25))+
  ggplot2::labs(x="Time (in generation)", y="Sucessful colonizations", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/successcol_baseline.png",width=800,height = 800)
print(h1.3)
dev.off()

h1.3b <- ggplot2::ggplot(data = baselinewopt1d, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=emigm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=emigi_mean,color="Island"),linewidth=1) +
  ylim(c(0,25))+
  ggplot2::labs(x="Time (in generation)", y="Succesful colonization", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/successcol_baseline1d.png",width=800,height = 800)
print(h1.3b)
dev.off()

h1.3c <- ggplot2::ggplot(data = baselinewopt2d, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=emigm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=emigi_mean,color="Island"),linewidth=1) +
  ylim(c(0,25))+
  ggplot2::labs(x="Time (in generation)", y="Succesful colonization", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/successcol_baseline2d.png",width=800,height = 800)
print(h1.3c)
dev.off()

h1.4 <- ggplot2::ggplot(data = baselinewopt, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=xm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtrait_baseline",width=800,height = 800)
print(h1.4)
dev.off()

h1.4b <- ggplot2::ggplot(data = baselinewopt1d, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=xm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtrait_baseline1d",width=800,height = 800)
print(h1.4b)
dev.off()

h1.4c <- ggplot2::ggplot(data = baselinewopt2d, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=xm_mean,color="Mainland"),linewidth=1) +
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtrait_baseline2d.png",width=800,height = 800)
print(h1.4c)
dev.off()

h1.4d <- ggplot2::ggplot(data = baselinewopt2d, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt2d,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt2d,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtraiti_baseline2d.png",width=800,height = 800)
print(h1.4d)
dev.off()

h1.4e <- ggplot2::ggplot(data = baselinewopt1d, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baselinewopt1d,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baselinewopt1d,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtraiti_baseline1d.png",width=800,height = 800)
print(h1.4e)
dev.off()

h1.4f <- ggplot2::ggplot(data = baseline, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=xi_mean,color="Island"),linewidth=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/h1/avtraiti_baseline.png",width=800,height = 800)
print(h1.4f)
dev.off()

main.baselinewopt.pop <- ggplot(baselinewopt.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(loc), point_color = as.factor(loc), fill = as.factor(loc))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Location") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of populations") +
  theme_ridges(center = T)
png("results/plot/h1/poploc_traitdist_baseline.png",width=800,height = 800)
print(main.baselinewopt.pop)
dev.off()

main.baselinewopt.pop <- ggplot(baselinewopt.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of populations") +
  theme_ridges(center = T)
png("results/plot/h1/popor_traitdist_baseline.png",width=800,height = 800)
print(main.baselinewopt.pop)
dev.off()

main.baselinewopt <- ggplot(baselinewopt.pop.mig.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of successful migrants") +
  theme_ridges(center = T)
png("results/plot/h1/successcol_traitdist_baseline.png",width=800,height = 800)
print(main.baselinewopt)
dev.off()

main.baselinewopt2dpop <- ggplot(baselinewopt2d.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of local populations") +
  theme_ridges(center = T)
png("results/plot/h1/popor_traitdist_baseline2d.png",width=800,height = 800)
print(main.baselinewopt2dpop)
dev.off()

main.baselinewopt2dpop2 <- ggplot(baselinewopt2d.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(loc), point_color = as.factor(loc), fill = as.factor(loc))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Location") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of local populations") +
  theme_ridges(center = T)
png("results/plot/h1/poploc_traitdist_baseline2d2.png",width=800,height = 800)
print(main.baselinewopt2dpop2)
dev.off()

main.baselinewopt2d <- ggplot(baselinewopt2d.pop.mig.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of successful migrants") +
  theme_ridges(center = T)
png("results/plot/h1/successcol_traitdist_baselined2.png",width=800,height = 800)
print(main.baselinewopt2d)
dev.off()

main.baselinewopt1d.mig <- ggplot(baselinewopt1d.pop.mig.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of successful migrants") +
  theme_ridges(center = T)
png("results/plot/h1/successcol_traitdist_baselined1.png",width=800,height = 800)
print(main.baselinewopt1d.mig)
dev.off()

main.baselinewopt1d.pop <- ggplot(baselinewopt1d.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(loc), point_color = as.factor(loc), fill = as.factor(loc))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Location") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of populations") +
  theme_ridges(center = T)
png("results/plot/h1/poploc_traitdist_baselined1.png",width=800,height = 800)
print(main.baselinewopt1d.pop)
dev.off()

main.baselinewopt1d.pop2 <- ggplot(baselinewopt1d.pop.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of populations") +
  theme_ridges(center = T)
png("results/plot/h1/popor_traitdist_baselined1.png",width=800,height = 800)
print(main.baselinewopt1d.pop2)
dev.off()

h1.5 <- ggplot(baselinewopt.pop.main.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of mainland population") +
  theme_ridges(center = T)
png("results/plot/h1/mainpop_traitdist_baseline.png",width=800,height = 800)
print(h1.5)
dev.off()

h1.5c <- ggplot(baselinewopt2d.pop.main.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of mainland population") +
  theme_ridges(center = T)
png("results/plot/h1/mainpop_traitdist_baselined2.png",width=800,height = 800)
print(h1.5)
dev.off()

h1.6 <- ggplot(baselinewopt.pop.isl.sub, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0),name="Time") +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution of island population") +
  theme_ridges(center = T)
png("results/plot/h1/islpop_traitdist_baseline.png",width=800,height = 800)
print(h1.6)
dev.off()

test <- subset(baselinewopt2d.pop.main,t==240)
test2 <-subset(baselinewopt2d.pop.isl,t==240)
test3 <- subset(baselinewopt.pop.main, t==240)
test4 <- subset(baselinewopt.pop.isl, t==240)
test5 <- subset(baselinewopt1d.pop.main, t==240)
test6 <- subset(baselinewopt1d.pop.isl, t==240)

h1.7i <- ggplot(test2, aes(x=x)) +
  geom_histogram(data=subset(test2,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test2,or==1), aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on island", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/islpop_traitdist_baseline2d.png",width=800,height = 800)
print(h1.7i)
dev.off()

h1.7m <- ggplot(test, aes(x=x)) +
  geom_histogram(data=subset(test,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test,or==1), aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on mainland", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/mainpop_traitdist_baseline2d.png",width=800,height = 800)
print(h1.7m)
dev.off()

h1.7cm <- ggplot(test3, aes(x=x)) +
  geom_histogram(data=subset(test3,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test3,or==1), aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on mainland", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/mainpop_traitdist_baseline.png",width=800,height = 800)
print(h1.7cm)
dev.off()

hi.7ci <- ggplot(test4, aes(x=x)) +
  geom_histogram(data=subset(test4,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test4,or==1),aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on island", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/islpop_traitdist_baseline.png",width=800,height = 800)
print(hi.7ci)
dev.off()

h1.7bm <- ggplot(test5, aes(x=x)) +
  geom_histogram(data=subset(test5,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test5,or==1), aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on mainland", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/mainpop_traitdist_baselined1.png",width=800,height = 800)
print(h1.7bm)
dev.off()

hi.7bi <- ggplot(test6, aes(x=x)) +
  geom_histogram(data=subset(test6,or==0), aes(fill="Mainland"),alpha=0.4) +
  geom_histogram(data=subset(test6,or==1),aes(fill="Island"),alpha=0.4)+
  ggplot2::labs(x="Ecological trait on island", color="Origin")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_fill_manual(values=colors)
png("results/plot/h1/islpop_traitdist_baselined1.png",width=800,height = 800)
print(hi.7bi)
dev.off()












# NUMBER OF INDIVIDUALS MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
nm <- ggplot2::ggplot(data = longtime, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_line(data=baseline,aes(x=time,y=nm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=nm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=nm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=nm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=nm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = baseline, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="baseline")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = longtime, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/nm.png",width=1400,height = 1400)
print(nm)
dev.off()
nm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=nm_mean)) +
  ggplot2::geom_line(aes(color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/nm.png",width=1400,height = 1400)
print(nm)
dev.off()

# NUMBER OF INDIVIDUALS isl ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
ni <- ggplot2::ggplot(data = longtime, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_line(data=baseline,aes(x=time,y=ni_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=ni_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=ni_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=ni_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=ni_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = baseline, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="baseline")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = longtime, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="diffopt2")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="diffopt4")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/ni.png",width=1400,height = 1400)
print(ni)
dev.off()
ni <- ggplot2::ggplot(data = bigpop, aes(x=time,y=ni_mean)) +
  ggplot2::geom_line(aes(color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="Number of ind isl", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/ni.png",width=1400,height = 1400)
print(ni)
dev.off()

# AVERAGE TRAIT MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9","nomig"="grey5")
xm <- ggplot2::ggplot(data = longtime, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=nomig,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=xm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=xm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=xm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=xm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=xm_mean,color="bigpop")) +
  ggplot2::geom_line(data=nomig,aes(x=time,y=xm_mean,color="nomig")) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=xm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=nomig,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=xm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=xm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=xm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=xm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=xm_mean,color="bigpop"),size=1) +
  ggplot2::geom_line(data=nomig,aes(x=time,y=xm_mean,color="nomig"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/xm2.png",width=1400,height = 1400)
print(xm2)
dev.off()
xm <- ggplot2::ggplot(data = baseline, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm <- ggplot2::ggplot(data = longtime, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/xm.png",width=1400,height = 1400)
print(xm)
dev.off()
xm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=xm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xm_ciupp,ymin=xm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/xm.png",width=1400,height = 1400)
print(xm)
dev.off()

# AVERAGE TRAIT ISL ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
xi <- ggplot2::ggplot(data = longtime, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=nomig,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=xi_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=xi_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=xi_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=xi_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=xi_mean,color="bigpop")) +
  ggplot2::geom_line(data=nomig,aes(x=time,y=xi_mean,color="nomig")) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=xi_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=nomig,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=xi_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=xi_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=xi_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=xi_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=xi_mean,color="bigpop"),size=1) +
  ggplot2::geom_line(data=nomig,aes(x=time,y=xi_mean,color="nomig"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/xi2.png",width=1400,height = 1400)
print(xi2)
dev.off()
xi <- ggplot2::ggplot(data = baseline, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi <- ggplot2::ggplot(data = longtime, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/xi.png",width=1400,height = 1400)
print(xi)
dev.off()
xi <- ggplot2::ggplot(data = bigpop, aes(x=time,y=xi_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=xi_ciupp,ymin=xi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Average ecological trait island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/xi.png",width=1400,height = 1400)
print(xi)
dev.off()




# BIRTH MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
bm <- ggplot2::ggplot(data = longtime, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=bm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=bm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=bm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=bm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=bm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=bm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=bm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=bm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=bm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=bm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=bm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/bm2.png",width=1400,height = 1400)
print(bm2)
dev.off()
bm <- ggplot2::ggplot(data = baseline, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm <- ggplot2::ggplot(data = longtime, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/bm.png",width=1400,height = 1400)
print(bm)
dev.off()
bm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=bm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bm_ciupp,ymin=bm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth main", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/bm.png",width=1400,height = 1400)
print(bm)
dev.off()

# BIRTH ISL ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
bi <- ggplot2::ggplot(data = longtime, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=bi_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=bi_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=bi_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=bi_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=bi_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=bi_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=bi_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=bi_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=bi_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=bi_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=bi_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/bi2.png",width=1400,height = 1400)
print(bi2)
dev.off()
bi <- ggplot2::ggplot(data = baseline, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi <- ggplot2::ggplot(data = longtime, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/bi.png",width=1400,height = 1400)
print(bi)
dev.off()
bi <- ggplot2::ggplot(data = bigpop, aes(x=time,y=bi_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=bi_ciupp,ymin=bi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="birth island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/bi.png",width=1400,height = 1400)
print(bi)
dev.off()




# MUTATION MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
mutm <- ggplot2::ggplot(data = longtime, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=mutm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=mutm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=mutm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=mutm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=mutm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=mutm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=mutm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=mutm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=mutm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=mutm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/mutm2.png",width=1400,height = 1400)
print(mutm2)
dev.off()
mutm <- ggplot2::ggplot(data = baseline, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm <- ggplot2::ggplot(data = longtime, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()
mutm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=mutm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=mutm_ciupp,ymin=mutm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/mutm.png",width=1400,height = 1400)
print(mutm)
dev.off()

# MUTATION isl ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
muti <- ggplot2::ggplot(data = longtime, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=muti_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=muti_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=muti_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=muti_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=muti_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=muti_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=muti_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=muti_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=muti_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=muti_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=muti_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/muti2.png",width=1400,height = 1400)
print(muti2)
dev.off()
muti <- ggplot2::ggplot(data = baseline, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti <- ggplot2::ggplot(data = longtime, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/muti.png",width=1400,height = 1400)
print(muti)
dev.off()
muti <- ggplot2::ggplot(data = bigpop, aes(x=time,y=muti_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=muti_ciupp,ymin=muti_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="mutation island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/muti.png",width=1400,height = 1400)
print(muti)
dev.off()

# MIG MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
migm <- ggplot2::ggplot(data = longtime, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=migm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=migm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=migm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=migm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=migm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=migm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=migm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=migm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=migm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=migm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=migm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/migm2.png",width=1400,height = 1400)
print(migm2)
dev.off()
migm <- ggplot2::ggplot(data = baseline, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm <- ggplot2::ggplot(data = longtime, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/migm.png",width=1400,height = 1400)
print(migm)
dev.off()
migm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/migm.png",width=1400,height = 1400)
print(migm)
dev.off()

# MIGRATION ISL ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
migi <- ggplot2::ggplot(data = longtime, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=migi_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=migi_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=migi_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=migi_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=migi_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=migi_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=migi_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=migi_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=migi_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=migi_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=migi_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/migi2.png",width=1400,height = 1400)
print(migi2)
dev.off()
migi <- ggplot2::ggplot(data = baseline, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi <- ggplot2::ggplot(data = longtime, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/migi.png",width=1400,height = 1400)
print(migi)
dev.off()
migi <- ggplot2::ggplot(data = bigpop, aes(x=time,y=migi_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="migration island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/migi.png",width=1400,height = 1400)
print(migi)
dev.off()



# DEATH MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
dm <- ggplot2::ggplot(data = longtime, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=dm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=dm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=dm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=dm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=dm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=dm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=dm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=dm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=dm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=dm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=dm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/dm2.png",width=1400,height = 1400)
print(dm2)
dev.off()
dm <- ggplot2::ggplot(data = baseline, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm <- ggplot2::ggplot(data = longtime, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/dm.png",width=1400,height = 1400)
print(dm)
dev.off()
dm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=dm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=dm_ciupp,ymin=dm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/dm.png",width=1400,height = 1400)
print(dm)
dev.off()


# DEATH ISL ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
di <- ggplot2::ggplot(data = longtime, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=di_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=di_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=di_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=di_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=di_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/di.png",width=1400,height = 1400)
print(di)
dev.off()
di2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=di_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=di_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=di_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=di_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=di_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=di_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/di2.png",width=1400,height = 1400)
print(di2)
dev.off()
di <- ggplot2::ggplot(data = baseline, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/di.png",width=1400,height = 1400)
print(di)
dev.off()
di <- ggplot2::ggplot(data = longtime, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/di.png",width=1400,height = 1400)
print(di)
dev.off()
di <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/di.png",width=1400,height = 1400)
print(di)
dev.off()
di <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/di.png",width=1400,height = 1400)
print(di)
dev.off()
di <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/di.png",width=1400,height = 1400)
print(di)
dev.off()
di <- ggplot2::ggplot(data = bigpop, aes(x=time,y=di_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=di_ciupp,ymin=di_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="death island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/di.png",width=1400,height = 1400)
print(di)
dev.off()


# SUCCESSFUL NATIVES MAIN ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
ebm <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=ebm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=ebm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=ebm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=ebm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=ebm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=ebm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=ebm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=ebm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=ebm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=ebm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/ebm2.png",width=1400,height = 1400)
print(ebm2)
dev.off()
ebm <- ggplot2::ggplot(data = baseline, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()
ebm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=ebm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/ebm.png",width=1400,height = 1400)
print(ebm)
dev.off()

# SUCCESSFUL NATIVES ISL ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
ebi <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=ebi_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=ebi_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=ebi_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=ebi_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=ebi_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=ebi_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=ebi_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=ebi_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=ebi_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=ebi_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/ebi2.png",width=1400,height = 1400)
print(ebi2)
dev.off()
ebi <- ggplot2::ggplot(data = baseline, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi <- ggplot2::ggplot(data = longtime, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()
ebi <- ggplot2::ggplot(data = bigpop, aes(x=time,y=ebi_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful natives island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/ebi.png",width=1400,height = 1400)
print(ebi)
dev.off()

# SUCCESSFUL MIGRATORS FROM MAINALND ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
emigm <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=emigm_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=emigm_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=emigm_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=emigm_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=emigm_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=emigm_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=emigm_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=emigm_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=emigm_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=emigm_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/emigm2.png",width=1400,height = 1400)
print(emigm2)
dev.off()
emigm <- ggplot2::ggplot(data = baseline, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()
emigm <- ggplot2::ggplot(data = bigpop, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="successful migrators from mainland", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/emigm.png",width=1400,height = 1400)
print(emigm)
dev.off()


# SUCCESSFUL MIGRATORS FROM ISLAND ####
colors <- c("baseline"="black","longtime"="#E69F00","diffopt2"="#0072B2","diffopt4"="#009E73","bigpop"="#CC79A7","diffopt0.5"="#56B4E9")
emigi <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="longtime")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=emigi_mean,color="baseline")) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=emigi_mean,color="diffopt2")) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=emigi_mean,color="diffopt4")) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=emigi_mean,color="diffopt0.5")) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=emigi_mean,color="bigpop")) +
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi2 <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(data=baseline,aes(x=time,y=emigi_mean,color="baseline"),size=1) +
  ggplot2::geom_line(data=diffopt2,aes(x=time,y=emigi_mean,color="diffopt2"),size=1) +
  ggplot2::geom_line(data=diffopt4,aes(x=time,y=emigi_mean,color="diffopt4"),size=1) +
  ggplot2::geom_line(data=diffopt0.5,aes(x=time,y=emigi_mean,color="diffopt0.5"),size=1) +
  ggplot2::geom_line(data=bigpop,aes(x=time,y=emigi_mean,color="bigpop"),size=1) +
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/comp/emigi2.png",width=1400,height = 1400)
print(emigi2)
dev.off()
emigi <- ggplot2::ggplot(data = baseline, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="baseline"),size=1) +
  ggplot2::geom_ribbon(data=baseline,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/baseline/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi <- ggplot2::ggplot(data = longtime, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="longtime"),size=1) +
  ggplot2::geom_ribbon(data=longtime,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/longtime/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi <- ggplot2::ggplot(data = diffopt0.5, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="diffopt0.5"),size=1) +
  ggplot2::geom_ribbon(data=diffopt0.5,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt0.5/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi <- ggplot2::ggplot(data = diffopt2, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="diffopt2"),size=1) +
  ggplot2::geom_ribbon(data=diffopt2,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt2/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi <- ggplot2::ggplot(data = diffopt4, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="diffopt4"),size=1) +
  ggplot2::geom_ribbon(data=diffopt4,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/diffopt4/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()
emigi <- ggplot2::ggplot(data = bigpop, aes(x=time,y=emigi_mean)) +
  ggplot2::geom_line(aes(color="bigpop"),size=1) +
  ggplot2::geom_ribbon(data=bigpop,aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="sucessful migrators from island", color="model")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) +
  ggplot2::scale_color_manual(values=colors)
png("results/plot/bigpop/emigi.png",width=1400,height = 1400)
print(emigi)
dev.off()

# POP INIT ####
popinit<- ggplot2::ggplot(data = baseline.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/baseline/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()
popinit <- ggplot2::ggplot(data = longtime.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/longtime/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()
popinit <- ggplot2::ggplot(data = diffopt0.5.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/diffopt0.5/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()
popinit <- ggplot2::ggplot(data = diffopt2.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/diffopt2/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()
popinit <- ggplot2::ggplot(data = diffopt4.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/diffopt4/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()
popinit <- ggplot2::ggplot(data = bigpop.popinit, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/bigpop/popinit.png",width=1400,height = 1400)
print(popinit)
dev.off()

# POP MID SIM ####
popmid<- ggplot2::ggplot(data = baseline.popmid, aes(x=x,group=as.factor(loc)) )+
  ggplot2::geom_histogram(aes(x=x,fill=as.factor(loc)),size=1) +
  ggplot2::labs(x="Time (in generation)", y="trait", color="Location")+
  ggplot2::theme(legend.position = "top",text = element_text(size = 35),panel.background = element_rect(fill = "white", colour = "black")) 
png("results/plot/baseline/popmid.png",width=1400,height = 1400)
print(popmid)
dev.off()


