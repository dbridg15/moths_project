#!usr/bin/env Rscript

# script: 6_sorting_results.R
# Desc:   reads in and amalgamates results
# Author: David Bridgwood (dbridg15@gmail.com)


###############################################################################
#
###############################################################################

# if Models summary exists delete it as code appends to file!
if (file.exists("../Results/Models_summarys.txt") == TRUE){
    file.remove("../Results/Models_summarys.txt")
}

# initialise dataframe
all.chi.rslts <- data.frame("X" = NA, "N" = NA, "measure" = NA, "no.nve" = NA,
                            "no.pve" = NA, "chi.sqr" = NA, "p.val" = NA,
                            "no.sig.nve" = NA, "no.sig.pve" = NA,
                            "sig.chi.sqr" = NA, "sig.p.val" = NA)


for (X in X.vals){  # sightings for mean ffd day
  for (N in N.vals){  # min years
    filename <- paste0("../Results/r_data/", X, "_", N, ".rda")

    load(filename)

    # save summary of models to txt file
    sink("../Results/Models_summarys.txt", append = TRUE)  # redirect output
    print("==================================================================")
    print(paste0("Data for X: ", X, " and N: ", N))
    print("==================================================================")
    print("ytemp.models======================================================")
    print(ytemp.models)
    print("stemp.models======================================================")
    print(stemp.models)
    print("winter.models=====================================================")
    print(winter.models)
    sink()  # output back to R-console

    # get all chi.rslts together - with X and N
    chi.rslts$X <- X
    chi.rslts$N <- N
    all.chi.rslts <- rbind(all.chi.rslts, chi.rslts)  # append to overall

  }
}

all.chi.rslts <- all.chi.rslts[-1, ]  # remove top row of NAs
write.csv(all.chi.rslts, file = "../Results/ChiSqr_Results.csv", row.names = F)  # save


###############################################################################
# table2
###############################################################################

# subset to only X = 3, N = 20
tbl2 <- subset(all.chi.rslts, X == 3 & N == 20)

# get explanatory and response variables
tbl2$explanatory_var <- unlist(strsplit(tbl2$measure, "[.]"))[ c(T,F)]
tbl2$response_var    <- unlist(strsplit(tbl2$measure, "[.]"))[ c(F,T)]


tbl2 <- tbl2[c("response_var", "explanatory_var", "no.pve", "no.nve", "chi.sqr",
               "p.val", "no.sig.pve", "no.sig.nve", "sig.chi.sqr", "sig.p.val")]

tbl2 <- tbl2[c(1, 5, 17, 13, 9, 2, 6, 18, 14, 10, 3, 7, 19, 15, 11, 4, 8, 20,
               16, 12), ]


write.csv(tbl2, file = "../Results/Table2.csv", row.names = F)  # save
