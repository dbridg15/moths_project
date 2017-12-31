#!usr/bin/env Rscript

# script: 4_species_classifications.R
# Desc: classifies species into seasons
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# defining the seasons
###############################################################################

seas_list <- c("early_spring","late_spring","early_summer","late_summer",
               "early_autumn","late_autumn","early_winter","late_winter",
               "spring","summer","autumn","winter","spring/summer",
               "summer/autumn","autumn/winter","winter/spring")
first <- c(60,107,152,199,244,290,335,1,60,152,244,335,60,152,244,1)
last <- c(106,151,198,243,289,334,365,59,151,243,334,59,243,334,365,151)

# pos is the order they should go in the year if i were to plot them...
pos <- c(1,3,5,7,9,11,13,15,2,6,10,14,4,8,12,16)

seasons <- data.frame(seas_list, first, last, pos)
colnames(seasons) <- c('season','first','last','pos')

rm(seas_list, first, last, pos)


###############################################################################
# classifiying species what does the middles 75% fly in?
###############################################################################

# list of vectors contain day all individuals were caught for given species
# i.e. if day 167 had 3 of that species lits would have ... 167, 167, 167, ...
individual_flights <- rep(list(c()), 110 )
i <- 1  # starting counter

for(id in as.character(selspc_df$id)){
  daily_spc_count <- as.numeric(moths2[id,])
  daily_spc_count[is.na(daily_spc_count)] <- 0
  tmp <- c()

  for(day in 1:365){
    tmp <- c(tmp, seq(from = day, by=0, length.out=daily_spc_count[day]))
  }

  individual_flights[[i]] <- tmp
  i  <- i + 1  # iterate counter
}

rm(i, tmp, day, id, daily_spc_count)

# class
class <- matrix(nrow=110,ncol=3)

colnames(class) <- c("12.5%", "87.5%", "season")
rownames(class) <- selspc_df$id

for(id in 1:110){
  class[id,1] <- quantile(individual_flights[[id]],c(.125,.875))[1]
  class[id,2] <- quantile(individual_flights[[id]],c(.125,.875))[2]
}

class <- as.data.frame(class)

for(id in 1:110){

    for(j in 1:8){
        if( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
           (class[id,2] <= seasons[j,3])){
                class[id,3] <- as.character(seasons$season[j])
        }
    }

    if(is.na(class[id,3])){
        for(j in 9:12){
            if( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
               (class[id,2] <= seasons[j,3])){
                    class[id,3] <- as.character(seasons$season[j])
            }
        }
    }

    if(is.na(class[id,3])){
        for(j in 13:16){
            if( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
               (class[id,2] <= seasons[j,3])){
                    class[id,3] <- as.character(seasons$season[j])
            }
        }
    }
}
class[which(is.na(class[,3])),3] <- "none"

selspc_df$season <- class$season

rm(class, id, j, individual_flights)
