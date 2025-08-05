#!/bin/bash

# Ask user for a simulation name
echo "Please enter the simulation name: "
read sim_name

# Define output file name with the simulation name given earlier
output_file="results/${sim_name}_main.Rout"

# Create result file if it doesn't exist yet
mkdir -p results

# Execute simulation with accurate simulation name
R CMD BATCH --no-save R/main_parallel.R $output_file &

# Confirmation message
echo "Simulation $sim_name is running. Output will be saved to $output_file"
