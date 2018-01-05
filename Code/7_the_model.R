#!usr/bin/env Rscript

# script: 7_the_model.R
# Desc:
# Author: David Bridgwood (dmb2417@ic.ac.uk)

# require
require(lme4)

###############################################################################
# putting together the dataframe ready for it!
###############################################################################

# intialise empty dataframe with needed columns
mdl.df <- data.frame("id"    = NA, "season" = NA, "year" = NA, "ytemp" = NA,
                     "stemp" = NA, "winter" = NA, "FFD"  = NA, "LFD"   = NA,
                     "FP"    = NA, "Fpos"   = NA)

# temporary df with same column headings
tmp <- data.frame("id"    = rep(NA, 110), "season" = rep(NA, 110),
                  "year"  = rep(NA, 110), "ytemp"  = rep(NA, 110),
                  "stemp" = rep(NA, 110), "winter" = rep(NA, 110),
                  "FFD"   = rep(NA, 110), "LFD"    = rep(NA, 110),
                  "FP"    = rep(NA, 110), "Fpos"   = rep(NA, 110))

for (yr in 1:25){
  tmp$id     <- ss.df$id
  tmp$season <- ss.df$season
  tmp$year   <- temperatures$year[yr+30]
  tmp$ytemp  <- temperatures$ytemp[yr+30]
  tmp$winter <- temperatures$winter[yr+30]
  tmp$FFD    <- aa.ss.flight[,1,yr]
  tmp$LFD    <- aa.ss.flight[,2,yr]
  tmp$FP     <- aa.ss.flight[,3,yr]
  tmp$Fpos   <- aa.ss.flight[,4,yr]

  for (id in 1:110){
    tmp$stemp[id] <- temperatures[yr+30, as.character(tmp$season[id])]
  }
  mdl.df <- rbind(mdl.df, tmp)
}

mdl.df <- mdl.df[-1,]

rm(tmp, yr)

mdl.df$season <- factor(mdl.df$season, levels = seas.list)


###############################################################################
# making the model
###############################################################################

pdf("../Results/model_plots.pdf")

expl <- c("ytemp", "stemp", "winter")

for (i in expl){

# FFD model
ffd.model   <- lmer(FFD ~ (1|year) + (mdl.df[,i]|id), data = mdl.df)
ffd.fixeff  <- fixef(ffd.model)
ffd.randeff <- ranef(ffd.model)
ffd.slope     <- vector(length=110)
ffd.intercept <- vector(length=110)

temps <- 7:13
for (s in 1:110){
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
lfd.slope      <- vector(length=110)
lfd.intercept <- vector(length=110)

temps <- 7:13
for (s in 1:110){
  ln <- coef(lm(lfd.fixeff[1] + lfd.randeff$id[s,1] +
                lfd.randeff$id[s,2]*temps~temps))

  lfd.slope[s]     <- as.numeric(ln[2])
  lfd.intercept[s] <- as.numeric(ln[1])
  rm(ln)
}

prty.plt.df <- cbind(ss.df[ , c(1,4)], ffd.slope, ffd.intercept,
                                       lfd.slope, lfd.intercept)
prty.plt.df$season <- factor(prty.plt.df$season, levels = rev(seas.list))
source("7.1_plot.R")

}

dev.off()

rm(expl, s, temps)
