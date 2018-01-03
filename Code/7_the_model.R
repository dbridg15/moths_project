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
mdl_df <- data.frame("id" = NA, "season" = NA, "year" = NA, "ytemp" = NA,
                     "stemp" = NA, "winter" = NA, "FFD" = NA, "LFD" = NA,
                     "FP" = NA, "Fpos" = NA)

# temporary df with same column headings
tmp <- data.frame("id" = rep(NA, 110), "year" = rep(NA, 110),
                  "ytemp" = rep(NA, 110), "stemp" = rep(NA, 110),
                  "winter" = NA, "FFD" = NA, "LFD" = NA, "FP" = NA,
                  "Fpos" = NA)

for(yr in 1:25){
    tmp$id <- selspc_df$id
    tmp$season <- selspc_df$season
    tmp$year <- temperatures$year[yr+30]
    tmp$ytemp <- temperatures$ytemp[yr+30]
    tmp$winter <- temperatures$winter[yr+30]
    tmp$FFD <- aaflight1[,1,yr]
    tmp$LFD <- aaflight1[,2,yr]
    tmp$FP <- aaflight1[,3,yr]
    tmp$Fpos <- aaflight1[,4,yr]

    for(id in 1:110){
        if(tmp$season[id] == "none"){
            tmp$stemp[id] <- tmp$ytemp[id]
        } else {
            tmp$stemp <- temperatures[yr+30, tmp$season[id]]
        }
    }

    mdl_df <- rbind(mdl_df, tmp)
}

rm(tmp)
mdl_df <- mdl_df[-1,]


###############################################################################
# making the model
###############################################################################


expl <- c("ytemp", "stemp", "winter")

for(i in expl){

# FFD model
ffdmodel <- lmer(FFD ~ (1|year) + (mdl_df[,i]|id), data = mdl_df)
summary(ffdmodel)

FFD.fixeff <- fixef(ffdmodel)
FFD.randeff <- ranef(ffdmodel)

temps <- 7:13
ffdmr <- vector(length=110)

plot(c(7,13),c(0, 365),type='n', main = paste("FFD: ", i))
for (s in 1:110){
  lines(temps, FFD.fixeff[1] + FFD.randeff$id[s,1] +
        FFD.randeff$id[s,2]*temps, type='l')
  ffdmr[s] <- coef(lm(FFD.fixeff[1] + FFD.randeff$id[s,1] +
                      FFD.randeff$id[s,2]*temps~temps))[2]
}

readline(prompt="Press [enter] to see next plot")

# LFD model
lfdmodel <- lmer(LFD ~ (1|year) + (mdl_df[, i]|id), data = mdl_df)
summary(lfdmodel)

LFD.fixeff <- fixef(lfdmodel)
LFD.randeff <- ranef(lfdmodel)

temps <- 7:13
lfdmr <- vector(length=110)

plot(c(7,13),c(0, 365),type='n', main = paste("LFD: ", i))
for (s in 1:110){
  lines(temps, LFD.fixeff[1] + LFD.randeff$id[s,1] +
        LFD.randeff$id[s,2]*temps, type='l')
  lfdmr[s] <- coef(lm(LFD.fixeff[1] + LFD.randeff$id[s,1] +
                      LFD.randeff$id[s,2]*temps~temps))[2]
}

readline(prompt="Press [enter] to see next plot")

plot(ffdmr,lfdmr, main = i)

readline(prompt="Press [enter] to see next plot")
}
