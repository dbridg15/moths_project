#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc:   runs the project
# Author: David Bridgwood (dmb2417@ic.ac.uk)

rm(list = ls())
graphics.off()
# options(warn=2)  # warnings to errors

# require
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(lme4)

###############################################################################
#
###############################################################################

# setting values for sesitivity analysis
X.vals <- c(1, 3, 5, 10, 15, 25)    # sightings for mean ffd day
N.vals <- c(3, 5, 10, 15, 20, 25)   # minimum years to include


message("\nReading in functions")
source("0.1_functions.R")
message("-Done\n")

message("\nReading in data...")
source("1_read_sort_data.R")
message("-Done\n")

message("\nAnalysing climate data")
source("2_analyse_climate.R")
message("-Done\n")

for (X in X.vals){
  for (N in N.vals){
    cat("\n==================================================================")
    message(paste0("\nstarting analysis with X = ", X, " and N = ", N))
    cat("==================================================================\n")

    message("\nSelecting species and adjusting for variable yr abundances...")
    source("3_select_&_adjust_abundance.R")
    message("-Done\n")

    message("\nAnalysing the effect of temperature on moths flight times...")
    source("4_effects_on_moths.R")
    message("-Done\n")

    message("\nsetting up model and producing various plots...")
    source("5_the_model.R")
    message("-Done\n")

    message("\nsaving the data!")
    filename <- paste0("../Results/r_data/", X, "_", N, ".rda")
    save(aa.ss.flight, chi.rslts, mdl.df, ss.df, stemp.models,
         winter.models, ytemp.models, flight, file = filename)
    rm(list= ls()[!(ls() %in% c('all.spc.df',
                                'cons',
                                'daily.temp',
                                'individual.flights',
                                'moths',
                                'moths.yrsum',
                                'msummary',
                                'PrettyPlots',
                                'seas.list',
                                'seasons',
                                'temperature.analysis',
                                'temperatures',
                                'X',
                                'N',
                                'start.time',
                                'X.vals',
                                'N.vals'))])
    message("-Done\n")
  }
}

message("\nsorting results\n")
source("6_sorting_results.R")
message("-Done\n")

cat("\n==================================================================")
message("\nFinished!!")
cat("==================================================================\n\n")

message("Warnings:\n")
warnings()
