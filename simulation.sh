#!/bin/bash

# Demander à l'utilisateur d'entrer le nom de la simulation
echo "Please enter the simulation name: "
read sim_name

# Définir le nom du fichier de sortie en fonction du nom de la simulation
output_file="results/${sim_name}_main.Rout"

# Créer le dossier results s'il n'existe pas déjà
mkdir -p results

# Exécuter la simulation avec le nom de fichier de sortie donné
R CMD BATCH --no-save R/main.R $output_file &

# Afficher un message de confirmation
echo "Simulation $sim_name is running. Output will be saved to $output_file"
