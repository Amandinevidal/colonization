#### PLOTS ####
rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggridges)


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
param_glossary[36,] <- c("K2",500,2,0.2)
param_glossary[37,] <- c("L2",1000,2,0.2)
param_glossary[38,] <- c("M2",2000,2,0.2)
param_glossary[39,] <- c("N2",4000,2,0.2)
param_glossary[40,] <- c("O2",5000,2,0.2)
param_glossary[41,] <- c("P2",500,5,0.2)
param_glossary[42,] <- c("Q2",1000,5,0.2)
param_glossary[43,] <- c("R2",2000,5,0.2)
param_glossary[44,] <- c("S2",4000,5,0.2)
param_glossary[45,] <- c("T2",5000,5,0.2)
param_glossary[46,] <- c("U2",500,10,0.2)
param_glossary[47,] <- c("V2",1000,10,0.2)
param_glossary[48,] <- c("W2",2000,10,0.2)
param_glossary[49,] <- c("X2",4000,10,0.2)
param_glossary[50,] <- c("Z2",5000,10,0.2)
param_glossary[51,] <- c("A3",500,0,0.01)
param_glossary[52,] <- c("B3",1000,0,0.01)
param_glossary[53,] <- c("C3",2000,0,0.01)
param_glossary[54,] <- c("D3",4000,0,0.01)
param_glossary[55,] <- c("E3",5000,0,0.01)

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

data <- subset(data, nb_des > 0)

df <- merge(data, param_glossary, by = "sim_id")
names(df)[names(df) == "k"] <- "continent_size"

breaks <- c(0, 1, 3, 10, Inf)
labels <- c("0", "1-2", "3-10", "10+")
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

cut_variable_groups(df, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)

breaks <- c(0, 10, 20, 50, 100, Inf)
labels <- c("0–10", "10–20", "20–50", "50–100", "100+")
cut_variable_groups(df, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)

setDT(df)  # transforme proprement df en data.table sans copie superflue
breaks <- c(0, 25, 50, 100, 200, 300, 400, 500, Inf)
labels <- c("0-25","25-50","50-100","100-200","200-300","300-400","400-500", "500+")
df[, time_group := cut(time, breaks = breaks, labels = labels, right = FALSE)]

df <- df %>%
  filter(mr != 0.01, dopt != 1)

df$dopt <- factor(
  df$dopt,
  levels = c(0, 1, 2, 5, 10)
)

df$continent_size <- factor(
  df$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- df %>%
  group_by(continent_size, dopt, mr) %>%
  summarise(n_events = n(), .groups = "drop")

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events, fill = as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "yellow", "4000" = "orange", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations",
       fill = "Continent size") +
  facet_grid(
    dopt ~ mr,
    labeller = labeller(
      dopt = function(x) paste("Δ habitat:", x),
      mr = function(x) paste("Migration rate:", x)
    ),
    scales = "free_y"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.background = element_rect(fill = "grey90", color = "grey50"),  # fond des étiquettes
    strip.text = element_text(size = 18)  # taille du texte des étiquettes
  )

png("results/plots/total_number_of_colonizations.png", width = 1000, height = 800)
print(p)
dev.off()


p <- ggplot(df, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  facet_grid(mr ~ dopt,
             labeller = labeller(
               dopt = function(x) paste("Δ habitat:", x),
               mr = function(x) paste("Migration rate:", x)
             ),
             scales = "free_y")+
  labs(
    x = "Number of descendants",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.background = element_rect(fill = "grey90", color = "grey50"),  # fond des étiquettes
    strip.text = element_text(size = 18)  # taille du texte des étiquettes
  )

png("results/plots/total_number_of_descendants.png", width = 1000, height = 800)
print(p)
dev.off()

p <- ggplot(df, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  facet_grid(mr ~ dopt,
             labeller = labeller(
               dopt = function(x) paste("Δ habitat:", x),
               mr = function(x) paste("Migration rate:", x)
             ),
             scales = "free_y")+
  labs(
    x = "Time persistence of lineages",
    y = "Events",
    fill = "Continent size"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90", color = "grey50"),  # fond des étiquettes
    strip.text = element_text(size = 18)  # taille du texte des étiquettes
  )

png("results/plots/total_time_persistence_of_lineages.png", width = 1000, height = 800)
print(p)
dev.off()

# Assure que colonist_x est numérique
df$colonist_x <- as.numeric(df$colonist_x)

# Prépare vline_df avec dopt numérique et facteurs pour le faceting
vline_df <- df %>%
  distinct(dopt, mr) %>%
  mutate(
    dopt_num = as.numeric(as.character(dopt)),
    dopt = as.factor(dopt),
    mr = as.factor(mr)
  )

# Convertir aussi dans df pour cohérence
df <- df %>%
  mutate(
    dopt = as.factor(dopt),
    mr = as.factor(mr)
  )

p <- ggplot(df, aes(x = colonist_x, color = as.factor(continent_size), fill = as.factor(continent_size))) + 
  geom_density(alpha = 0.2) +
  geom_vline(data = vline_df, aes(xintercept = dopt_num), linetype = "dashed", color = "black") +
  geom_vline(aes(xintercept = 0), color = "black") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) +
  scale_color_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) +
  facet_grid(mr ~ dopt,
             labeller = labeller(
               dopt = function(x) paste("Δ habitat:", x),
               mr = function(x) paste("Migration rate:", x)
             ),
             scales = "free_y") +
  labs(
    x = "Colonist trait",
    y = "Density",
    fill = "Continent size",
    color = "Continent size"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 18)
  )

png("results/plots/total_colonist_trait.png", width = 1000, height = 800)
print(p)
dev.off()


vline_df <- df %>%
  distinct(dopt, mr) %>%
  mutate(
    dopt_num = as.numeric(as.character(dopt)),
    dopt = as.factor(dopt),
    mr = as.factor(mr)
  )

# Plot avec ridge plots
p_ridge <- ggplot(df, aes(x = av_x, y = time_group, fill = as.factor(continent_size), color = as.factor(continent_size))) +
  geom_density_ridges(alpha = 0.3, scale = 1.2, rel_min_height = 0.01, size = 0.3) +
  geom_vline(data = vline_df, aes(xintercept = dopt_num), linetype = "dashed", color = "black") +
  geom_vline(aes(xintercept = 0), color = "black") +
  scale_fill_manual(values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")) +
  scale_color_manual(values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")) +
  facet_grid(. ~ dopt, labeller = labeller(dopt = function(x) paste("Δ habitat:", x))) +
  labs(
    x = "Lineage average trait",
    y = "Time group",
    fill = "Continent size",
    color = "Continent size"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    text = element_text(size = 16),
    strip.background = element_rect(fill = "grey90", color = "grey50"),
    strip.text = element_text(size = 14)
  )

png("results/plots/average_lineage_trait_trait.png", width = 1500, height = 800)
print(p_ridge)
dev.off()


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

# data_size_effet = same habitat increasing size + RARE MIG
select <- param_glossary[which(param_glossary$dopt == 0 & param_glossary$mr == 0.01),]$sim_id
data_size_effect_rm <- data[sim_id %in% select]
data_size_effect_rm <- merge(data_size_effect_rm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_rm)[names(data_size_effect_rm) == "k"] <- "continent_size"

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

# data_size_effect_hab2 + LOW MIG
select <- param_glossary[which(param_glossary$dopt == 2 & param_glossary$mr == 0.2),]$sim_id
data_size_effect_h2_lm <- data[sim_id %in% select] 
data_size_effect_h2_lm <- merge(data_size_effect_h2_lm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h2_lm)[names(data_size_effect_h2_lm) == "k"] <- "continent_size"

# data_size_effect_hab5
select <- param_glossary[which(param_glossary$dopt == 5 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h5_hm <- data[sim_id %in% select] 
data_size_effect_h5_hm <- merge(data_size_effect_h5_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h5_hm)[names(data_size_effect_h5_hm) == "k"] <- "continent_size"

# data_size_effect_hab5 + LOW MIG
select <- param_glossary[which(param_glossary$dopt == 5 & param_glossary$mr == 0.2),]$sim_id
data_size_effect_h5_lm <- data[sim_id %in% select] 
data_size_effect_h5_lm <- merge(data_size_effect_h5_lm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h5_lm)[names(data_size_effect_h5_lm) == "k"] <- "continent_size"

# data_size_effect_hab10
select <- param_glossary[which(param_glossary$dopt == 10 & param_glossary$mr == 0.5),]$sim_id
data_size_effect_h10_hm <- data[sim_id %in% select] 
data_size_effect_h10_hm <- merge(data_size_effect_h10_hm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h10_hm)[names(data_size_effect_h10_hm) == "k"] <- "continent_size"

# data_size_effect_hab10 + LOW MIG
select <- param_glossary[which(param_glossary$dopt == 10 & param_glossary$mr == 0.2),]$sim_id
data_size_effect_h10_lm <- data[sim_id %in% select] 
data_size_effect_h10_lm <- merge(data_size_effect_h10_lm, param_glossary[, c("sim_id", "k")], by = "sim_id", all.x = TRUE)
names(data_size_effect_h10_lm)[names(data_size_effect_h10_lm) == "k"] <- "continent_size"

# data_hab_effect = same size increasing habitat difference
select <- param_glossary[which(param_glossary$k == 500 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_hm <- data[sim_id %in% select] 
data_hab_effect_hm <- merge(data_hab_effect_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect + LOW MIG
select <- param_glossary[which(param_glossary$k == 500 & param_glossary$mr == 0.2),]$sim_id
data_hab_effect_lm <- data[sim_id %in% select] 
data_hab_effect_lm <- merge(data_hab_effect_lm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.5 = size 0.5 increased habitat difference
select <- param_glossary[which(param_glossary$k == 1000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.5_hm <- data[sim_id %in% select] 
data_hab_effect_s0.5_hm <- merge(data_hab_effect_s0.5_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.5 + LOW MIG
select <- param_glossary[which(param_glossary$k == 1000 & param_glossary$mr == 0.2),]$sim_id
data_hab_effect_s0.5_lm <- data[sim_id %in% select] 
data_hab_effect_s0.5_lm <- merge(data_hab_effect_s0.5_lm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.25 = size 0.25 increased habitat difference
select <- param_glossary[which(param_glossary$k == 2000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.25_hm <- data[sim_id %in% select] 
data_hab_effect_s0.25_hm <- merge(data_hab_effect_s0.25_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.25 + LOW MIG
select <- param_glossary[which(param_glossary$k == 2000 & param_glossary$mr == 0.2),]$sim_id
data_hab_effect_s0.25_lm <- data[sim_id %in% select] 
data_hab_effect_s0.25_lm <- merge(data_hab_effect_s0.25_lm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.125 = size 0.125 increased habitat difference
select <- param_glossary[which(param_glossary$k == 4000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.125_hm <- data[sim_id %in% select] 
data_hab_effect_s0.125_hm <- merge(data_hab_effect_s0.125_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.125 + LOW MIG
select <- param_glossary[which(param_glossary$k == 4000 & param_glossary$mr == 0.2),]$sim_id
data_hab_effect_s0.125_lm <- data[sim_id %in% select] 
data_hab_effect_s0.125_lm <- merge(data_hab_effect_s0.125_lm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.1 = size 0.1 increased habitat difference
select <- param_glossary[which(param_glossary$k == 5000 & param_glossary$mr == 0.5),]$sim_id
data_hab_effect_s0.1_hm <- data[sim_id %in% select] 
data_hab_effect_s0.1_hm <- merge(data_hab_effect_s0.1_hm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# data_hab_effect_s0.5 + LOW MIG
select <- param_glossary[which(param_glossary$k == 5000 & param_glossary$mr == 0.2),]$sim_id
data_hab_effect_s0.1_lm <- data[sim_id %in% select] 
data_hab_effect_s0.1_lm <- merge(data_hab_effect_s0.1_lm, param_glossary[, c("sim_id", "dopt")], by = "sim_id", all.x = TRUE)

# NB_DES GROUPS ####
breaks <- c(0, 1, 3, 10, Inf)
labels <- c("0", "1-2", "3-10", "10+")
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
cut_variable_groups(data_size_effect_rm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_hm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_lm, var = "nb_des", new_col = "nb_des_group", breaks = breaks, labels = labels)


# P_DES GROUPS ####
breaks <- c(0, 10, 20, 50, 100, Inf)
labels <- c("0–10", "10–20", "20–50", "50–100", "100+")
cut_variable_groups(data_size_effect_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_rm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h1_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h2_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h5_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_size_effect_h10_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.5_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.25_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.125_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_hm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)
cut_variable_groups(data_hab_effect_s0.1_lm, var = "p_des", new_col = "p_des_group", breaks = breaks, labels = labels)

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

# SIZE EFFECT HABITAT DIFF 2 + LOW MIG####
data_size_effect_h2_lm$continent_size <- factor(
  data_size_effect_h2_lm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h2_lm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 2 MR = 0.2",
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

png("results/plots/size_effect_h2_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h2_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 2 ; MR = 0.2",
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

png("results/plots/size_effect_h2_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h2_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 2 ; MR = 0.2",
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
png("results/plots/size_effect_h2_lm_hist_p_des.png",width=1000,height = 800)
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


# SIZE EFFECT HABITAT DIFF 5 + LOW MIG####
data_size_effect_h5_lm$continent_size <- factor(
  data_size_effect_h5_lm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h5_lm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 5 MR = 0.2",
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

png("results/plots/size_effect_h5_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.2",
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

png("results/plots/size_effect_h5_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.2",
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
png("results/plots/size_effect_h5_lm_hist_p_des.png",width=1000,height = 800)
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

# SIZE EFFECT HABITAT DIFF 10 + LOW MIG####
data_size_effect_h10_lm$continent_size <- factor(
  data_size_effect_h10_lm$continent_size,
  levels = c(500, 1000, 2000, 4000, 5000)
)

colonisation_counts <- data_size_effect_h10_lm %>%
  group_by(continent_size) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(continent_size), y = n_events,fill=as.factor(continent_size))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow", "5000" = "red")
  ) + 
  labs(x = "Continent size", y = "Number of colonizations",
       title = "Total number of colonizations per continent size \nDOPT = 10 MR = 0.2",
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

png("results/plots/size_effect_h10_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h10_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 10 ; MR = 0.2",
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

png("results/plots/size_effect_h10_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h10_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 10 ; MR = 0.2",
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
png("results/plots/size_effect_h10_lm_hist_p_des.png",width=1000,height = 800)
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

# HAB EFFECT + LOW MIG ####
data_hab_effect_lm$dopt <- factor(
  data_hab_effect_lm$dopt,
  levels = c(0, 1, 2, 5, 10)
)

colonisation_counts <- data_hab_effect_lm %>%
  group_by(dopt) %>%
  summarise(n_events = n())

p <- ggplot(colonisation_counts, aes(x = as.factor(dopt), y = n_events,fill=as.factor(dopt))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("0" = "blue", "1" = "green", "2" = "yellow", "5" = "orange", "10" = "red")
  ) + 
  labs(x = "habitat difference", y = "Number of colonizations",
       title = "Total number of colonizations per habitat difference \nK = 500 MR = 0.2",
       fill = "Dopt") +
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

png("results/plots/size_effect_h5_lm_tot_col.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_lm, aes(x = nb_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500"="blue","1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.2",
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

png("results/plots/size_effect_h5_lm_hist_nb_des.png",width=1000,height = 800)
print(p)
dev.off()

p <- ggplot(data_size_effect_h5_lm, aes(x = p_des_group, fill = as.factor(continent_size))) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(
    values = c("500" = "blue", "1000" = "green", "2000" = "orange", "4000" = "yellow","5000"="red")
  ) + 
  labs(
    title = "DOPT = 5 ; MR = 0.2",
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
png("results/plots/size_effect_h5_lm_hist_p_des.png",width=1000,height = 800)
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



#### data clean ####

data_clean <- data[data$sim_id %in% param_glossary$sim_id, ]
data_clean <- merge(data_clean, param_glossary, by = "sim_id")
names(data_clean)[names(data_clean) == "k"] <- "continent_size"
data_clean$continent_size <- as.numeric(data_clean$continent_size)
data_clean$dopt <- as.numeric(data_clean$dopt)
data_clean$mr <- as.numeric(data_clean$mr)
data_clean$continent_size <- factor(data_clean$continent_size, levels = sort(unique(data_clean$continent_size)))
data_clean$dopt <- factor(data_clean$dopt, levels = sort(unique(data_clean$dopt)))
data_clean$mr <- factor(data_clean$mr, levels = sort(unique(data_clean$mr)))

# what is the distribution of the trait of the colonist
data_filtered <- data_clean %>%
  filter(mr %in% c(0.2, 0.5))
data_filtered <- data_filtered %>%
  mutate(dopt_num = as.numeric(as.character(dopt)))
dopt_lines <- data_filtered %>%
  distinct(dopt, dopt_num) %>%
  mutate(xintercept = dopt_num)
p <- ggplot(data_filtered, aes(x = colonist_x, color = as.factor(continent_size), fill = as.factor(continent_size))) +
  geom_density(alpha = 0.2) +
  geom_vline(data = dopt_lines, aes(xintercept = xintercept),
             linetype = "dashed", color = "red") +
  facet_grid(mr ~ dopt, labeller = label_both) +
  labs(
    title = "Trait distribution of colonist",
    x = "Colonist trait",
    y = "Density",
    color = "Mainland size",
    fill = "Mainland size"
  ) +
  theme_minimal(base_size = 16)
png("results/plots/trait_distribution_colonists.png",width=1000,height = 800)
print(p)
dev.off()

# what is the distribution of the number of descendants ~ colonist trait ####
# Préparation des données
data_sample <- data_filtered %>%
  filter(nb_des > 0, !is.na(colonist_x), continent_size != 2000, continent_size != 4000, dopt!=1) %>%
  group_by(sim_id) %>%
  sample_n(size = min(n(), 20000)) %>%
  ungroup()
# Enlever les outliers
threshold <- quantile(data_sample$nb_des, 0.95)
data_sample_filtered <- data_sample %>%
  filter(nb_des <= threshold)
# Binariser le trait du colonisateur (par exemple 20 classes)
data_binned <- data_sample_filtered %>%
  mutate(colonist_x_bin = cut(colonist_x, breaks = seq(round(min(colonist_x)), round(max(colonist_x)), by = 2), include.lowest = TRUE, right = T)) %>%
  group_by(colonist_x_bin, mr, dopt, continent_size) %>%
  summarise(
    mean_nb_des = mean(nb_des,na.rm = T),
    sd_nb_des = sd(nb_des),
    n = n(),
    se_nb_des = sd_nb_des / sqrt(n),
    .groups = "drop"
  ) %>%
  ungroup()
data_binned <- data_binned %>% filter(!is.na(colonist_x_bin))
# Récupérer les niveaux du facteur colonist_x_bin (c’est un facteur, on doit extraire les labels)
levels_bins <- levels(data_binned$colonist_x_bin)
# Fonction pour trouver la position x (niveau du facteur) correspondant à un dopt numérique arrondi
get_vline_pos <- function(dopt_value) {
  # trouver le bin qui contient dopt_value
  # colonist_x_bin est sous forme "[a,b]", on va chercher le bin qui inclut dopt_value
  for (i in seq_along(levels_bins)) {
    # extraire bornes
    bin_str <- levels_bins[i]
    # retirer crochets et splitter
    bin_bounds <- gsub("\\[|\\]|\\(|\\)", "", bin_str)
    bounds <- strsplit(bin_bounds, ",")[[1]]
    lower <- as.numeric(bounds[1])
    upper <- as.numeric(bounds[2])
    if (!is.na(lower) && !is.na(upper)) {
      if (dopt_value >= lower && dopt_value <= upper) {
        return(i)  # position du niveau facteur
      }
    }
  }
  return(NA)
}
# Créer une data.frame pour geom_vline avec les valeurs uniques de dopt et continent_size
vline_data <- data_binned %>%
  distinct(dopt, continent_size) %>%
  rowwise() %>%
  mutate(xintercept = get_vline_pos(as.numeric(as.character(dopt)))) %>%
  ungroup() %>%
  filter(!is.na(xintercept))
# Plot avec geom_vline
p <- ggplot(data_binned, aes(x = colonist_x_bin, y = mean_nb_des, fill = as.factor(mr))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = mean_nb_des - se_nb_des, ymax = mean_nb_des + se_nb_des),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_vline(data = vline_data, aes(xintercept = xintercept), linetype = "dashed", color = "darkgreen", size = 1) +
  facet_grid(continent_size ~ dopt, labeller = label_both) +
  labs(
    title = "Mean number of descendants by colonist trait and migration rate",
    x = "Binned ecological trait of colonist",
    y = "Mean number of descendants",
    fill = "Migration rate"
  ) +
  scale_y_continuous(limits = c(0, 17)) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
png("results/plots/nb_des_colonists_trait.png",width=1000,height = 800)
print(p)
dev.off()


# what is the distribution of the persistence time ~ colonist trait ####
# Préparation des données
data_sample <- data_filtered %>%
  filter(nb_des > 0, !is.na(colonist_x), continent_size != 2000, continent_size != 4000, dopt != 0) %>%
  group_by(sim_id) %>%
  sample_n(size = min(n(), 20000)) %>%
  ungroup()
# Enlever les outliers
threshold <- quantile(data_sample$p_des, 0.99)
data_sample_filtered <- data_sample %>%
  filter(p_des <= threshold)
# Binariser le trait du colonisateur (par exemple 20 classes)
data_binned <- data_sample_filtered %>%
  mutate(colonist_x_bin = cut(colonist_x, breaks = seq(round(min(colonist_x)), round(max(colonist_x)), by = 2), include.lowest = TRUE, right = T)) %>%
  group_by(colonist_x_bin, mr, dopt, continent_size) %>%
  summarise(
    mean_p_des = mean(p_des,na.rm = T),
    sd_p_des = sd(p_des),
    n = n(),
    se_p_des = sd_p_des / sqrt(n),
    .groups = "drop"
  ) %>%
  ungroup()
data_binned <- data_binned %>% filter(!is.na(colonist_x_bin))
# Récupérer les niveaux du facteur colonist_x_bin (c’est un facteur, on doit extraire les labels)
levels_bins <- levels(data_binned$colonist_x_bin)
# Fonction pour trouver la position x (niveau du facteur) correspondant à un dopt numérique arrondi
get_vline_pos <- function(dopt_value) {
  # trouver le bin qui contient dopt_value
  # colonist_x_bin est sous forme "[a,b]", on va chercher le bin qui inclut dopt_value
  for (i in seq_along(levels_bins)) {
    # extraire bornes
    bin_str <- levels_bins[i]
    # retirer crochets et splitter
    bin_bounds <- gsub("\\[|\\]|\\(|\\)", "", bin_str)
    bounds <- strsplit(bin_bounds, ",")[[1]]
    lower <- as.numeric(bounds[1])
    upper <- as.numeric(bounds[2])
    if (!is.na(lower) && !is.na(upper)) {
      if (dopt_value >= lower && dopt_value <= upper) {
        return(i)  # position du niveau facteur
      }
    }
  }
  return(NA)
}
# Créer une data.frame pour geom_vline avec les valeurs uniques de dopt et continent_size
vline_data <- data_binned %>%
  distinct(dopt, continent_size) %>%
  rowwise() %>%
  mutate(xintercept = get_vline_pos(as.numeric(as.character(dopt)))) %>%
  ungroup() %>%
  filter(!is.na(xintercept))
# Plot avec geom_vline
p <- ggplot(data_binned, aes(x = colonist_x_bin, y = mean_p_des, fill = as.factor(mr))) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  geom_errorbar(aes(ymin = mean_p_des - se_p_des, ymax = mean_p_des + se_p_des),
                position = position_dodge(width = 0.9), width = 0.3) +
  geom_vline(data = vline_data, aes(xintercept = xintercept), linetype = "dashed", color = "darkgreen", size = 1) +
  facet_grid(continent_size ~ dopt, labeller = label_both) +
  labs(
    title = "Mean persistence time by colonist trait",
    x = "Ecological trait of colonist",
    y = "Mean persistence time",
    fill = "Migration rate"
  ) +
  scale_y_continuous(limits = c(0, 75)) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
png("results/plots/p_des_colonists_trait.png",width=1000,height = 800)
print(p)
dev.off()
















data_clean_mr0.5 <- subset(data_clean, mr == 0.5)
ggplot(data_clean_mr0.5, aes(x = as.factor(dopt), y = log10(nb_des), fill = as.factor(continent_size))) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "Dopt", y = "log10 nb des",
       fill = "Size") +
  theme_minimal()


data_clean_mr0.2 <- subset(data_clean, mr == 0.2)
ggplot(data_clean_mr0.2, aes(x = as.factor(dopt), y = log10(nb_des), fill = as.factor(continent_size))) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "Dopt", y = "log10 nb des",
       fill = "Size") +
  theme_minimal()

data_clean_mr0.01 <- subset(data_clean, mr == 0.01)
ggplot(data_clean_mr0.01, aes(x = as.factor(dopt), y = log10(nb_des), fill = as.factor(continent_size))) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "Dopt", y = "log10 nb des",
       fill = "Size") +
  theme_minimal()

data_summary <- data_clean %>%
  group_by(continent_size, dopt) %>%
  summarise(mean_descendants = mean(nb_des), .groups = "drop")

ggplot(data_summary, aes(x = as.factor(continent_size), y = mean_descendants, fill = as.factor(dopt))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Taille de population source", y = "Nombre moyen de descendants",
       fill = "Différence d'habitat") +
  theme_minimal()





# #### RESUMÉ ET COMPARER VACE UN HEAT MAP X CONNENT_SIZE Y DOPT UNE FACETTE POUR CHAQUE TAUX DE MIGRATION, LES COULEURS INDIQUES LA VALEUR DE LA VARIABLE D'INTERET NB DES OU P DES####
# 
# Moyenne du nombre de descendants pour chaque combinaison de paramètres
summary_df <- data_clean %>%
  group_by(continent_size, dopt, mr) %>%
  summarise(mean_nb_des = mean(nb_des, na.rm = TRUE), .groups = "drop")

# Plot : heatmap pour chaque valeur de mr
ggplot(summary_df, aes(x = continent_size, y = dopt, fill = mean_nb_des)) +
  geom_tile(color = "white") +
  facet_wrap(~ mr, labeller = label_both) +
  scale_fill_viridis_c(name = "Moyenne nb_des", option = "C") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Succès de colonisation (nb descendants)",
    x = "Taille du continent",
    y = "Différence d'habitat (dopt)"
  )


# Moyenne du nombre de descendants pour chaque combinaison de paramètres
summary_df <- data_clean %>%
  group_by(continent_size, dopt, mr) %>%
  summarise(mean_p_des = mean(p_des, na.rm = TRUE), .groups = "drop")

# Plot : heatmap pour chaque valeur de mr
ggplot(summary_df, aes(x = continent_size, y = dopt, fill = mean_p_des)) +
  geom_tile(color = "white") +
  facet_wrap(~ mr, labeller = label_both) +
  scale_fill_viridis_c(name = "Moyenne p_des", option = "C") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Succès de colonisation (persistence)",
    x = "Taille du continent",
    y = "Différence d'habitat (dopt)"
  )


# Somme du nombre de descendants pour chaque combinaison de continent_size, dopt et mr
summary_df <- data_clean %>%
  filter(nb_des > 0) %>%
  group_by(continent_size, dopt, mr) %>%
  summarise(total_nb_des = sum(nb_des, na.rm = TRUE), .groups = "drop")

# Créer la heatmap avec le total de nb_des
ggplot(summary_df, aes(x = continent_size, y = as.factor(dopt), fill = total_nb_des)) +
  geom_tile(color = "white") +  # Heatmap
  scale_fill_viridis_c(name = "Total nb_des", option = "C") +  # Palette de couleurs pour la somme du nombre de descendants
  facet_wrap(~ mr, labeller = label_both) +  # Facettes pour 'mr'
  theme_minimal(base_size = 14) +
  labs(
    title = "Heatmap: Total des descendants ",
    x = "Taille du continent",
    y = "Différence d'habitat (dopt)"
  )

summary_df <- data_clean %>%
  filter(nb_des > 0) %>%
  group_by(continent_size, dopt, mr) %>%
  summarise(max_p_des = max(nb_des, na.rm = TRUE), .groups = "drop")

# Créer la heatmap avec le total de nb_des
ggplot(summary_df, aes(x = continent_size, y = as.factor(dopt), fill = max_p_des)) +
  geom_tile(color = "white") +  # Heatmap
  scale_fill_viridis_c(name = "Max p_des", option = "C") +  # Palette de couleurs pour la somme du nombre de descendants
  facet_wrap(~ mr, labeller = label_both) +  # Facettes pour 'mr'
  theme_minimal(base_size = 14) +
  labs(
    title = "Heatmap: Max temps de persistence ",
    x = "Taille du continent",
    y = "Différence d'habitat (dopt)"
  )


#### ECOLOGICAL TRAIT ####








