#!usr/bin/env Rscript

# script: 7_example_plots.R
# Desc:   plots FFD and LFD response for example species
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# Example species - Silver-ground Carpet (ID = 42)
###############################################################################

# choose species
ID <- "42"

# load data from X = 3 and N = 20
load("../Results/r_data/3_20.rda")

# set up dataframe
exmp.spc <- data.frame("FFD"    = flight[ID, "FFD", ],
                       "LFD"    = flight[ID, "LFD", ],
                       "aa.FFD" = aa.ss.flight[ID, "FFD", ],
                       "aa.LFD" = aa.ss.flight[ID, "LFD", ],
                       "ytemp"  = temperatures$ytemp[31:55],
                       "stemp"  = temperatures[31:55, as.character(
                                           all.spc.df$season[as.numeric(ID)])],
                       "winter" = temperatures$winter[31:55])
