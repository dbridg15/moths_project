#!usr/bin/env Rscript

# script: 7_the_model.R
# Desc:
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

  for (id in 1:ss.num){
    tmp$stemp[id] <- temperatures[yr + 30, as.character(tmp$season[id])]
  }
  mdl.df <- rbind(mdl.df, tmp)
}

mdl.df <- mdl.df[-1,]

rm(tmp, yr)

mdl.df$season <- factor(mdl.df$season, levels = seas.list)


###############################################################################
# making the model
###############################################################################

pdf(paste0("../Results/model_plots_", X, "_", N, ".pdf"))

expl <- c("ytemp", "stemp", "winter", "cons")

for (i in expl){

# FFD model
ffd.model     <- lmer(FFD ~ (1|year) + (mdl.df[,i]|id), data = mdl.df)
ffd.fixeff    <- fixef(ffd.model)
ffd.randeff   <- ranef(ffd.model)
ffd.slope     <- vector(length=ss.num)
ffd.intercept <- vector(length=ss.num)

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

temps <- 7:13
for (s in 1:ss.num){
  ln <- coef(lm(lfd.fixeff[1] + lfd.randeff$id[s,1] +
                lfd.randeff$id[s,2]*temps~temps))

  lfd.slope[s]     <- as.numeric(ln[2])
  lfd.intercept[s] <- as.numeric(ln[1])
  rm(ln)
}

prty.plt.df <- cbind(ss.df[ , c(1,4)], ffd.slope, ffd.intercept,
                                       lfd.slope, lfd.intercept)
prty.plt.df$season <- factor(prty.plt.df$season, levels = rev(seas.list))
source("5.1_plots.R")

nam = paste0(i, ".models")
assign(nam, c(ffd.model, lfd.model))

rm(nam,ffd.model, lfd.model)

}

dev.off()

rm(expl, s, temps)
