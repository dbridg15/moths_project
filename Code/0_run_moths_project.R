#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc: runs the project
# Author: David Bridgwood (dmb2417@ic.ac.uk)

rm(list = ls())


###############################################################################
#
###############################################################################

# generate plots?
PLOTS <- FALSE

source("1_read_in_data.R")

source("2_sorting_data.R")

source("3_adjusting_for_abundance.R")

source("4_species_classifications.R")

source("5_climate_analysis.R")

source("6_effect_on_moths.R")
