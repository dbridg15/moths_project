#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc: runs the project
# Author: David Bridgwood (dmb2417@ic.ac.uk)

rm(list = ls())
graphics.off()

# require
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(lme4)


###############################################################################
#
###############################################################################

message("Reading in data...")
source("1_read_in_data.R")

for (X in c(1, 3, 5, 10, 15, 25)){  # sightings for mean ffd day
  for (N in c(1, 5, 10, 15, 20, 15)){  # min years
    message(paste0("starting analysis with X = ", X, " and N = ", N))

    message("Sorting data...")
    source("2_sorting_data.R")

    message("Adjusting for variable yearly abundances")
    source("3_adjusting_for_abundance.R")

    message("Classifying species based on season of flight")
    source("4_species_classifications.R")

    message("Analysising climate data")
    source("5_climate_analysis.R")

    message("Analysing the effect of temperature on moths flight times")
    source("6_effect_on_moths.R")

    message("setting up model and producing various plots")
    source("7.0_the_model.R")
  }
}

message("Finished!!")
