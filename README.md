
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Colonization IBM

This repository contains the code for IBM simulations.

## Contents

- [:file_folder: R](R)
  - [check_param.R]() - Functions to check parameter validity and proper
    simulation setup.
  - [colonization_success.R]() - Use simulation output (.tar.gz) to
    calculate colonization success
  - [functions.R]() - This file contains a suite of functions designed
    to run the simulation
  - [main_parallel.R]() - Main script - Parallelized version
  - [main.R]() - Main script - Sequential version
  - [parameters_calibration.R]() - Tools for manual testing and
    visualization of all parameters and fitness function
  - [parameters.R]() - Parameters set
  - [summary_variables.R]() - Use simulation output (.tar.gz) to
    summarize key simulation variables
- [:rocket: simulation.sh]() - bash script to run a simulation from the
  command line.  
  :warning: *Caution: This bash script is set up to launch the
  **parallelized** version of the main R simulation (main_parallel.R).
  Make sure your system supports parallel execution and adjust the
  script if you want to run the sequential version instead.*

### Licenses

**Code :** See the [DESCRIPTION](DESCRIPTION) file
