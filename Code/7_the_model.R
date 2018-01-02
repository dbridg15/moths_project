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
