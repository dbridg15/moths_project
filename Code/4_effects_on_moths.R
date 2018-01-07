#!usr/bin/env Rscript

# script: 4_effects_on_moths.R
# Desc:   are moths flying earlier/later/longer?
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# regression for each species, measure and possible explanatory variable
###############################################################################

# these are the things to calcuate - setting them up as a list, they will be
# headings in the ss.df
expl <- c("year", "ytemp", "stemp", "winter", "cons")
msr  <- c("FFD", "LFD", "FP", "Fpos")
val  <- c("slope", "intercept", "p.val", "r.sqr")

tocalc <- c()
for (x in expl){ for (y in msr){ for (z in val){
  tocalc <- c(tocalc, paste0(x, ".", y, ".", z)) }}}

# cleanup
rm(x, y, z)

# set them as headings in selected species dataframe
for (i in tocalc){
  ss.df[i] <- NA
}

rm(i)

# dataframe containing the explanatory variables
expl.df <- temperatures[31:55,c("year", "ytemp", "winter", "cons")]
expl.df$stemp <- NA  # will be specific for each species

# for the id of each selected species
for (id in as.character(ss.df$id)){
  # get stemp for that species
  expl.df$stemp <- as.numeric(unlist(temperatures[as.character(ss.df[id,
                                                           "season"])])[31:55])

  # for each explanatory and measure
  for (a in expl){
    for (b in msr){
      p <- paste0(a, ".", b)  # prefix
      # do regression and store coefs in ss.df
      mdl <- lm(aa.ss.flight[id, b, ] ~ unlist(expl.df[a]))
      ss.df[id, paste0(p, ".slope")]     <- as.numeric(coef(mdl)[2])
      ss.df[id, paste0(p, ".intercept")] <- as.numeric(coef(mdl)[1])
      ss.df[id, paste0(p, ".p.val")]     <- as.numeric(anova(mdl)$'Pr(>F)'[1])
      ss.df[id, paste0(p, ".r.sqr")]     <- as.numeric(summary(mdl)[8])
    }
  }
  # reset stemp to NA
  expl.df$stemp <- NA
}


# cleanup
rm(expl.df, id, a, b, mdl, val, p)


###############################################################################
# are slopes in one direction more common than others (chi-squared)
###############################################################################

# new list of to-be-calculated, every combination of expl and msr
tocalc <- c()
for (x in expl){ for (y in msr){
  tocalc <- c(tocalc, paste0(x, ".", y)) }}

# headers of the chi.rslts dataframe
hdrs <- c("no.nve", "no.pve", "chi.sqr", "p.val", "no.sig.nve",
          "no.sig.pve","sig.chi.sqr", "sig.p.val")

# chi.rslts is a dataframe with those headers
chi.rslts <- data.frame('measure' = tocalc)
for (i in hdrs){
  chi.rslts[i] <- NA
}

# cleanup
rm(hdrs, tocalc)

# go throgh each row of chi.rslts and do the needed calculations
for (i in 1:nrow(chi.rslts)){
  # for all slopes
  chi.rslts$no.nve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],
                               ".slope")] < 0))  # number of negative slopes
  chi.rslts$no.pve[i] <- length(which(ss.df[,paste0(chi.rslts$measure[i],
                               ".slope")] > 0))  # number of positive slopes
  # chi-squared of +ve vs -ve, supress warnings when numbers are low
  tmp <- suppressWarnings(chisq.test(c(chi.rslts$no.nve[i],
                                       chi.rslts$no.pve[i])))
  chi.rslts$chi.sqr[i] <- as.numeric(tmp[1])
  chi.rslts$p.val[i] <- as.numeric(tmp[3])

  # for only significant slopes
  # list of ids with significant slopes
  sig.list <- which(ss.df[,paste0(chi.rslts$measure[i],".p.val")] < 0.05)
  chi.rslts$no.sig.nve[i] <-
      length(which(ss.df[,paste0(chi.rslts$measure[i], ".slope")]
            [which(ss.df[,paste0(chi.rslts$measure[i],".p.val")] < 0.05)] < 0))
  chi.rslts$no.sig.pve[i] <-
      length(which(ss.df[,paste0(chi.rslts$measure[i], ".slope")]
            [which(ss.df[,paste0(chi.rslts$measure[i],".p.val")] < 0.05)] > 0))
  if (length(sig.list) > 0){  # if there were significant slopes
    tmp <- suppressWarnings(chisq.test(c(chi.rslts$no.sig.nve[i],
                                         chi.rslts$no.sig.pve[i])))
    chi.rslts$sig.chi.sqr[i] <- as.numeric(tmp[1])
    chi.rslts$sig.p.val[i] <- as.numeric(tmp[3])
  }
}

# cleanup
rm(i, x, y, msr, expl, sig.list, tmp)


###############################################################################
# making fancy plots
###############################################################################

# open pdf for plots
pdf(paste0("../Results/plots/empirical/empirical_plots_", X, "_", N, ".pdf"))

# for each measure set up a datframe and put through PrettyPlots function
for (msr in c("ytemp", "stemp", "winter", "cons")){
  tmp.df <- ss.df[,c("id", "season",
                     paste0(msr, ".FFD.slope"), paste0(msr, ".FFD.intercept"),
                     paste0(msr, ".LFD.slope"), paste0(msr, ".LFD.intercept"))]
  # required colnames for function
  colnames(tmp.df) <- c("id", "season", "ffd.slope", "ffd.intercept",
                        "lfd.slope", "lfd.intercept")
  plt.title  <- paste(msr, "empirical")
  PrettyPlots(dat = tmp.df, plt.title = plt.title)
}

dev.off()  # close pdf

# cleanup
rm(msr, plt.title, tmp.df)