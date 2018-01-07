#!usr/bin/env Rscript

# script: 6_sorting_results.R
# Desc:   reads in and amalgamates results
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
#
###############################################################################

if (file.exists("../Results/Models_summarys.txt") == TRUE){
    file.remove("../Results/Models_summarys.txt")
}

all.chi.rslts <- data.frame("X" = NA, "N" = NA, "measure" = NA, "no.nve" = NA,
                            "no.pve" = NA, "chi.sqr" = NA, "p.val" = NA,
                            "no.sig.nve" = NA, "no.sig.pve" = NA,
                            "sig.chi.sqr" = NA, "sig.p.val" = NA)


for (X in c(1, 3, 5, 10, 15, 25)){  # sightings for mean ffd day
  for (N in c(3, 5, 10, 15, 20, 25)){  # min years
    filename <- paste0("../Results/r_data/", X, "_", N, ".rda")

    load(filename)


    # save summary of models to txt file
    sink("../Results/Models_summarys.txt", append = TRUE)
    print("==================================================================")
    print(paste0("Data for X: ", X, " and N: ", N))
    print("==================================================================")
    print("ytemp.models======================================================")
    print(ytemp.models)
    print("stemp.models======================================================")
    print(stemp.models)
    print("winter.models=====================================================")
    print(winter.models)
    sink()

    # get all chi.rslts together - with X and N
    chi.rslts$X <- X
    chi.rslts$N <- N
    all.chi.rslts <- rbind(all.chi.rslts, chi.rslts)

  }
}

all.chi.rslts <- all.chi.rslts[-1, ]  # remove top row of NAs
write.table(all.chi.rslts, file = "../Results/ChiSqr_Results.csv")
