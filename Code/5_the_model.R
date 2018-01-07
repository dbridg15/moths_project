#!usr/bin/env Rscript

# script: 5_the_model.R
# Desc:   produces a general linear model from the data and plots for each spc
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# putting together the dataframe ready for it!
###############################################################################

# intialise empty dataframe with needed columns
mdl.df <- data.frame("id"    = NA, "season" = NA, "year" = NA, "ytemp" = NA,
                     "stemp" = NA, "winter" = NA, "cons" = NA,  "FFD"  = NA,
                     "LFD"   = NA, "FP"     = NA, "Fpos" = NA)

# temporary df with same column headings
tmp <- data.frame("id"    = rep(NA, ss.num), "season" = rep(NA, ss.num),
                  "year"  = rep(NA, ss.num), "ytemp"  = rep(NA, ss.num),
                  "stemp" = rep(NA, ss.num), "winter" = rep(NA, ss.num),
                  "cons"  = rep(NA, ss.num), "FFD"    = rep(NA, ss.num),
                  "LFD"   = rep(NA, ss.num), "FP"     = rep(NA, ss.num),
                  "Fpos"  = rep(NA, ss.num))

# for all years get the relevant data in tmp then append to mdl.df
for (yr in 1:25){
  tmp$id     <- ss.df$id
  tmp$season <- ss.df$season
  tmp$year   <- temperatures$year[yr + 30]
  tmp$ytemp  <- temperatures$ytemp[yr + 30]
  tmp$winter <- temperatures$winter[yr + 30]
  tmp$cons   <- temperatures$cons[yr + 30]
  tmp$FFD    <- aa.ss.flight[ , 1, yr]
  tmp$LFD    <- aa.ss.flight[ , 2, yr]
  tmp$FP     <- aa.ss.flight[ , 3, yr]
  tmp$Fpos   <- aa.ss.flight[ , 4, yr]

  # stemp is different for each species
  for (id in 1:ss.num){
    tmp$stemp[id] <- temperatures[yr + 30, as.character(tmp$season[id])]
  }

  mdl.df <- rbind(mdl.df, tmp)
}

mdl.df <- mdl.df[-1,]  # remove top row which is just NAs

# sort out seasons as factors
mdl.df$season <- factor(mdl.df$season, levels = seas.list)

# cleanup
rm(tmp, yr, id)


###############################################################################
# making the model
###############################################################################

# initialise pdf for plots
pdf(paste0("../Results/model_plots_", X, "_", N, ".pdf"))

# will do models for each of these explanatory variables
expl <- c("ytemp", "stemp", "winter")

for (i in expl){

  # FFD model
  ffd.model     <- lmer(FFD ~ (1|year) + (mdl.df[,i]|id), data = mdl.df)
  ffd.fixeff    <- fixef(ffd.model)
  ffd.randeff   <- ranef(ffd.model)
  ffd.slope     <- vector(length=ss.num)
  ffd.intercept <- vector(length=ss.num)

  # getting slope/intercept for each species
  temps <- 7:13
  for (s in 1:ss.num){
  ln <-  coef(lm(ffd.fixeff[1] + ffd.randeff$id[s,1] +
                  ffd.randeff$id[s,2]*temps~temps))
  ffd.slope[s]     <- as.numeric(ln[2])
  ffd.intercept[s] <- as.numeric(ln[1])
  rm(ln)
  }

  # LFD model
  lfd.model     <- lmer(LFD ~ (1|year) + (mdl.df[, i]|id), data = mdl.df)
  lfd.fixeff    <- fixef(lfd.model)
  lfd.randeff   <- ranef(lfd.model)
  lfd.slope     <- vector(length=ss.num)
  lfd.intercept <- vector(length=ss.num)

  # getting slope for each species
  temps <- 7:13
  for (s in 1:ss.num){
  ln <- coef(lm(lfd.fixeff[1] + lfd.randeff$id[s,1] +
                  lfd.randeff$id[s,2]*temps~temps))
  lfd.slope[s]     <- as.numeric(ln[2])
  lfd.intercept[s] <- as.numeric(ln[1])
  rm(ln)
  }

  # setting up dataframe for plots
  prty.plt.df <- cbind(ss.df[ , c(1,4)], ffd.slope, ffd.intercept,
                                         lfd.slope, lfd.intercept)

  plt.title <- paste0(i, " model")  # title for plots
  # PrettyPlots functions draws the plots from dataframe
  PrettyPlots(dat = prty.plt.df, plt.title = plt.title)

  # put ffd and lfd models in list to be saved
  nam = paste0(i, ".models")

  assign(nam, c(ffd.model, lfd.model))

  # cleanup
  rm(nam, ffd.model, lfd.model, plt.title)
}

dev.off()  # close pdf

# cleanup
rm(expl, s, temps, ffd.fixeff, ffd.intercept, ffd.randeff, ffd.slope, i,
   lfd.fixeff, lfd.randeff, lfd.intercept, lfd.slope)
