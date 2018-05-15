#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc:   source/Rscript this file to run project
# Author: David Bridgwood (dbridg15@gmail.com)

# clear the R workspace
rm(list = ls())
graphics.off()
# options(warn=2)  # warnings to errors

# require
require(ggplot2)       # for all plots
require(RColorBrewer)  # for colour gradient of seaons on combined plots
require(gridExtra)     # for putting together combined plot
require(lme4)          # for models
require(reshape2)      # for reshaping dataframes


###############################################################################
# setting values for run
###############################################################################

with_sensitivity <- FALSE  # set to TRUE to run with sensitivity analysis

# run with sensitivity analysis?
if (with_sensitivity == FALSE){
  X.vals <- 3
  N.vals <- 20

  } else if (with_sensitivity == TRUE){

  # values for sesitivity analysis ***MUST INCLUDE X=3 and N=20 TO RUN***
  X.vals <- c(1, 3, 5, 10, 15, 25)    # sightings for mean ffd day
  N.vals <- c(3, 5, 10, 15, 20, 25)   # minimum years to include

  }

# setting colour gradient for combined plots (from RColorBrewer)
colgrad <- "RdYlGn"

###############################################################################
# starting project
###############################################################################

message("\nReading in functions")
source("functions.R")
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
                                'N.vals',
                                'colgrad'))])
    message("-Done\n")
  }
}

message("\nsorting results")
source("6_sorting_results.R")
message("-Done\n")

message("\nexample species plots")
source("7_example_plots.R")
message("-Done\n")

cat("\n==================================================================")
message("\nFinished!!")
cat("==================================================================\n\n")

message("Warnings:\n")
warnings()
