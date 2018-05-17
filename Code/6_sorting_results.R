#!usr/bin/env Rscript

# script: 6_sorting_results.R
# Desc:   reads in and amalgamates results
# Author: David Bridgwood (dbridg15@gmail.com)


###############################################################################
# combine results of sensitivity analysis
###############################################################################

# if Models summary exists delete it as code appends to file!
if  (file.exists("../Results/Models_summarys.txt") == TRUE){ 
    file.remove("../Results/Models_summarys.txt") 
}

# initialise dataframe
all.chi.rslts  <- data.frame("X" = NA, "N" = NA, "measure" = NA, "no.pve" = NA,
                             "no.nve" = NA, "chi.sqr" = NA, "p.val" = NA,
                             "no.sig.pve" = NA, "no.sig.nve" = NA,
                             "sig.chi.sqr" = NA, "sig.p.val" = NA)

all.wlcx.rslts <- data.frame("X" = NA, "N" = NA, "measure" = NA, "no.slopes" = NA,
                             "median" = NA, "q0.05" = NA, "q0.95" = NA, "wlcx.V" = NA,
                             "wlcx.p" = NA, "sig_no.slopes" = NA, "sig_median" = NA,
                             "sig_q0.05" = NA, "sig_q0.95" = NA, "sig_wlcx.V" = NA,
                             "sig_wlcx.p" = NA)


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

    # get all wlcx.rslts together - with X and N
    wlcx.rslts$X <- X
    wlcx.rslts$N <- N
    all.wlcx.rslts <- rbind(all.wlcx.rslts, wlcx.rslts)  # append to overall

  }
}

all.chi.rslts <- all.chi.rslts[-1, ]  # remove top row of NAs

all.chi.rslts$response_var    <- unlist(strsplit(all.chi.rslts$measure, "[.]"))[ c(T,F)]
all.chi.rslts$explanatory_var <- unlist(strsplit(all.chi.rslts$measure, "[.]"))[ c(F,T)]

all.chi.rslts <- all.chi.rslts[c("X", "N", "response_var", "explanatory_var",
                                 "no.pve", "no.nve", "chi.sqr", "p.val", "no.sig.pve",
                                 "no.sig.nve", "sig.chi.sqr", "sig.p.val")]

write.csv(all.chi.rslts, file <- "../Results/ChiSqr_Results.csv", row.names = F)  # save

all.wlcx.rslts <- all.wlcx.rslts[-1, ]  # remove top row of NAs

all.wlcx.rslts$response_var    <- unlist(strsplit(all.wlcx.rslts$measure, "[.]"))[ c(T,F)]
all.wlcx.rslts$explanatory_var <- unlist(strsplit(all.wlcx.rslts$measure, "[.]"))[ c(F,T)]

all.wlcx.rslts <- all.wlcx.rslts[c("X", "N", "response_var", "explanatory_var",
                                   "no.slopes", "median", "q0.05", "q0.95", "wlcx.V",
                                   "wlcx.p", "sig_no.slopes", "sig_median", "sig_q0.05",
                                   "sig_q0.95", "sig_wlcx.V", "sig_wlcx.p")]

write.csv(all.wlcx.rslts, file <- "../Results/Wilcox_Results.csv", row.names = F)  # save


###############################################################################
# table2
###############################################################################

# subset to only X = 3, N = 20
tbl2_chsqr <- subset(all.chi.rslts, X == 3 & N == 20,
                     select = c("response_var", "explanatory_var", "no.pve",
                                "no.nve", "chi.sqr", "p.val", "no.sig.pve",
                                "no.sig.nve", "sig.chi.sqr", "sig.p.val"))

write.csv(tbl2_chsqr, file = "../Results/Table2_chisqr.csv", row.names = F)  # save

# **Actually going with Wilcoxon signed ranks test**

tbl2_wlcx <- subset(all.wlcx.rslts, X == 3 & N == 20,
                    selct = c("response_var", "explanatory_var", "no.slopes",
                              "median", "q0.05", "q0.95", "wlcx.V", "wlcx.p",
                              "sig_no.slopes", "sig_median", "sig_q0.05",
                              "sig_q0.95", "sig_wlcx.V", "sig_wlcx.p"))

write.csv(tbl2_wlcx, file = "../Results/Table2_wlcx.csv", row.names = F)  # save
