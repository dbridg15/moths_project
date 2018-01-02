#!usr/bin/env Rscript

# script: 6_effect_on_moths.R
# Desc: are moths flying earlier/later/longer?
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# regression for each species, measure and possible explanatory variable
###############################################################################

# these are the thing i'll calcuate - setting them up as a list, they will be
# heading in the selspc_df
expl <- c("year", "ytemp", "stemp", "winter", "cons")
msr <- c("FFD", "LFD", "FP", "Fpos")
val <- c("slope", "p_val", "r_sqr")

tocalc <- c()
for(x in expl){ for(y in msr){ for(z in val){
    tocalc <- c(tocalc, paste0(x, "_", y, "_", z)) }}}

rm(x, y, z)

# set them as headings in my selected species data frame
for(i in tocalc){
    selspc_df[i] <- NA
}

# dataframe containg the explanatory variables
expl_df <- temperatures[31:55,c("year", "ytemp", "winter", "cons")]
expl_df$stemp <- NA  # will be specific for each species

# for the id of each selected species
for(id in as.character(selspc_df$id)){
    # set stemp for the given species use ytemp if season is "none"
    if(selspc_df[id, "season"] == "none"){
        expl_df$stemp <- expl_df$ytemp
    } else{
        expl_df$stemp <- unlist(temperatures[selspc_df[id, "season"]])[31:55]
    }

    # for each explanatory and measure
    for(a in expl){
        for(b in msr){
            # do regression and store coefs in selspc_df
            mdl <- lm(aaflight1[id, b, ] ~ unlist(expl_df[a]))
            selspc_df[id, paste0(a, "_", b, "_slope")] <-
                                as.numeric(coef(mdl)[2])  # slope
            selspc_df[id, paste0(a, "_", b, "_p_val")] <-
                                as.numeric(anova(mdl)$'Pr(>F)'[1]) # p-value
            selspc_df[id, paste0(a, "_", b, "_r_sqr")] <-
                                as.numeric(summary(mdl)[8])  # R squared
        }
    }

    # reset stemp to NA
    expl_df$stemp <- NA  # will be specific for each species
}


rm(expl_df, id, a, b)

###############################################################################
# are slopes in one direction more common than others (chi-squared)
###############################################################################


tocalc <- c()
for(x in expl){ for(y in msr){
    tocalc <- c(tocalc, paste0(x, "_", y)) }}

hdrs <- c("no_nve", "no_pve", "chi_sqr", "p_val", "no_sig_nve",
          "no_sig_pve","sig_chi_sqr", "sig_p_val")

chi_rslts <- data.frame('measure' = tocalc)

for(i in hdrs){
    chi_rslts[i] <- NA
}

rm(hdrs, tocalc)


for(i in 1:nrow(chi_rslts)){
    # for all slopes
    chi_rslts$no_nve[i] <- length(which(selspc_df[,paste0(chi_rslts$measure[i],
                                                           "_slope")] < 0))
    chi_rslts$no_pve[i] <- length(which(selspc_df[,paste0(chi_rslts$measure[i],
                                                           "_slope")] > 0))
    tmp <- chisq.test(c(chi_rslts$no_nve[i], chi_rslts$no_pve[i]))
    chi_rslts$chi_sqr[i] <- as.numeric(tmp[1])
    chi_rslts$p_val[i] <- as.numeric(tmp[3])

    # for only significant slopes
    chi_rslts$no_sig_nve[i] <- length(which(selspc_df[,paste0(chi_rslts$measure[i],"_slope")]
                            [which(selspc_df[,paste0(chi_rslts$measure[i],"_p_val")] < 0.05)] < 0))
    chi_rslts$no_sig_pve[i] <- length(which(selspc_df[,paste0(chi_rslts$measure[i],"_slope")]
                            [which(selspc_df[,paste0(chi_rslts$measure[i],"_p_val")] < 0.05)] > 0))
    tmp <- chisq.test(c(chi_rslts$no_sig_nve[i], chi_rslts$no_sig_pve[i]))
    chi_rslts$sig_chi_sqr[i] <- as.numeric(tmp[1])
    chi_rslts$sig_p_val[i] <- as.numeric(tmp[3])
}
