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
    tmp$stemp[id] <- temperatures[yr+30, tmp$season[id]]
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
ffd.model <- lmer(FFD ~ (1|year) + (mdl.df[,i]|id), data = mdl.df)
summary(ffd.model)

ffd.fixeff  <- fixef(ffd.model)
ffd.randeff <- ranef(ffd.model)

temps <- 7:13
ffd.resp <- vector(length=110)

plot(c(7,13),c(0, 365),type = 'n', main = paste("FFD: ", i))
for (s in 1:110){
  lines(temps, ffd.fixeff[1] + ffd.randeff$id[s,1] +
        ffd.randeff$id[s,2]*temps, type='l')
  ffd.resp[s] <- coef(lm(ffd.fixeff[1] + ffd.randeff$id[s,1] +
                         ffd.randeff$id[s,2]*temps~temps))[2]
}

# LFD model
lfd.model <- lmer(LFD ~ (1|year) + (mdl.df[, i]|id), data = mdl.df)
# summary(lfdmodel)

lfd.fixeff  <- fixef(lfd.model)
lfd.randeff <- ranef(lfd.model)

temps <- 7:13
lfd.resp <- vector(length=110)

plot(c(7,13),c(0, 365),type='n', main = paste("LFD: ", i))
for (s in 1:110){
  lines(temps, lfd.fixeff[1] + lfd.randeff$id[s,1] +
        lfd.randeff$id[s,2]*temps, type='l')
  lfd.resp[s] <- coef(lm(lfd.fixeff[1] + lfd.randeff$id[s,1] +
                         lfd.randeff$id[s,2]*temps~temps))[2]
}
plot(ffd.resp, lfd.resp, main = i)
}

dev.off()

rm(expl, s, temps)
