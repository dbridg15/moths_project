#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc: runs the project
# Author: David Bridgwood (dmb2417@ic.ac.uk)

rm(list = ls())
graphics.off()

###############################################################################
#
###############################################################################

print("Reading in data...")
source("1_read_in_data.R")

print("Sorting data...")
source("2_sorting_data.R")

print("Adjusting for variable yearly abundances")
source("3_adjusting_for_abundance.R")

print("Classifying species based on season of flight")
source("4_species_classifications.R")

print("Analysising climate data")
source("5_climate_analysis.R")

print("Analysing the effect of temperature on moths flight times")
source("6_effect_on_moths.R")

print("setting up model and producing various plots")
source("7_the_model.R")

print("Finished!!")
