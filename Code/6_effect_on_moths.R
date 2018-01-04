#!usr/bin/env Rscript

# script: 6_effect_on_moths.R
# Desc: are moths flying earlier/later/longer?
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# regression for each species, measure and possible explanatory variable
###############################################################################

# these are the thing i'll calcuate - setting them up as a list, they will be
# heading in the ss.df
expl <- c("year", "ytemp", "stemp", "winter", "cons")
msr <- c("FFD", "LFD", "FP", "Fpos")
val <- c("slope", "p.val", "r.sqr")

tocalc <- c()
for (x in expl){ for (y in msr){ for (z in val){
  tocalc <- c(tocalc, paste0(x, ".", y, ".", z)) }}}

rm(x, y, z)

# set them as headings in my selected species data frame
for (i in tocalc){
  ss.df[i] <- NA
}

# dataframe containg the explanatory variables
expl.df <- temperatures[31:55,c("year", "ytemp", "winter", "cons")]
expl.df$stemp <- NA  # will be specific for each species

# for the id of each selected species
for (id in as.character(ss.df$id)){
  # set stemp for the given species use ytemp if season is "none"
  if (ss.df[id, "season"] == "none"){
    expl.df$stemp <- expl.df$ytemp
  } else{
    expl.df$stemp <- unlist(temperatures[ss.df[id, "season"]])[31:55]
  }

  # for each explanatory and measure
  for (a in expl){
    for (b in msr){
      # do regression and store coefs in ss.df
      mdl <- lm(aa.ss.flight[id, b, ] ~ unlist(expl.df[a]))
      ss.df[id, paste0(a, "_", b, "_slope")] <-
                as.numeric(coef(mdl)[2])  # slope
      ss.df[id, paste0(a, "_", b, "_p_val")] <-
                as.numeric(anova(mdl)$'Pr(>F)'[1]) # p-value
      ss.df[id, paste0(a, "_", b, "_r_sqr")] <-
                as.numeric(summary(mdl)[8])  # R squared
    }
  }
  # reset stemp to NA
  expl.df$stemp <- NA  # will be specific for each species
}


rm(expl.df, id, a, b, mdl, val)

###############################################################################
# are slopes in one direction more common than others (chi-squared)
###############################################################################

tocalc <- c()
for (x in expl){ for (y in msr){
  tocalc <- c(tocalc, paste0(x, "_", y)) }}

hdrs <- c("no_nve", "no_pve", "chi_sqr", "p_val", "no_sig_nve",
      "no_sig_pve","sig_chi_sqr", "sig_p_val")

chi.rslts <- data.frame('measure' = tocalc)

for (i in hdrs){
  chi.rslts[i] <- NA
}

rm(hdrs, tocalc)


for (i in 1:nrow(chi.rslts)){
  # for all slopes
  chi.rslts$no_nve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],
                               "_slope")] < 0))
  chi.rslts$no_pve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],
                               "_slope")] > 0))
  tmp <- chisq.test(c(chi.rslts$no_nve[i], chi.rslts$no_pve[i]))
  chi.rslts$chi_sqr[i] <- as.numeric(tmp[1])
  chi.rslts$p_val[i] <- as.numeric(tmp[3])

  # for only significant slopes
  chi.rslts$no_sig_nve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],"_slope")]
              [which(ss.df[,paste0(chi.rslts$measure[i],"_p_val")] < 0.05)] < 0))
  chi.rslts$no_sig_pve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],"_slope")]
              [which(ss.df[,paste0(chi.rslts$measure[i],"_p_val")] < 0.05)] > 0))
  tmp <- chisq.test(c(chi.rslts$no_sig_nve[i], chi.rslts$no_sig_pve[i]))
  chi.rslts$sig_chi_sqr[i] <- as.numeric(tmp[1])
  chi.rslts$sig_p_val[i] <- as.numeric(tmp[3])
}

rm(i, x, y, msr, expl)
