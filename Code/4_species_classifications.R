#!usr.bin.env Rscript

# script: 4.species.classifications.R
# Desc: classifies species into seasons
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# defining the seasons
###############################################################################

seas.list <- c("early.spring", "spring", "late.spring", "spring.summer",
			   "early.summer", "summer", "late.summer", "summer.autumn",
			   "early.autumn", "autumn", "late.autumn", "autumn.winter",
			   "early.winter", "winter", "late.winter", "winter.spring",
               "none")

first.day <- c(60,60,107,60,152,152,199,152,244,244,290,244,335,335,1,1,1)
last.day <- c(106,151,151,243,198,243,243,334,289,334,334,365,365,59,59,151,365)

seasons <- data.frame(seas.list, first.day, last.day)
colnames(seasons) <- c('season', 'first.day', 'last.day')

# add colour
seasons$colour <- NA
colfunc <- colorRampPalette(c("green", "yellow", "blue"))

for (i in 1:(nrow(seasons)-1)){
    seasons$colour[i]  <- colfunc((nrow(seasons)-1))[i]
}

seasons$colour[17] <- "grey"

rm(seas.list, first.day, last.day)


###############################################################################
# classifiying species what does the middles 75% fly in?
###############################################################################

# list of vectors contain day all individuals were caught for given species
# i.e. if day 167 had 3 of that species lits would have ... 167, 167, 167, ...
individual.flights <- rep(list(c()), 110)
i <- 1  # starting counter

for (id in as.character(ss.df$id)){
  daily.spc.count <- as.numeric(ss.moths.yrsum[id, ])
  daily.spc.count[is.na(daily.spc.count)] <- 0
  tmp <- c()

  for (day in 1:365){
  tmp <- c(tmp, seq(from = day, by=0, length.out=daily.spc.count[day]))
  }

  individual.flights[[i]] <- tmp
  i  <- i + 1  # iterate counter
}

rm(i, tmp, day, id, daily.spc.count)

# class
class <- matrix(nrow=110,ncol=3)

colnames(class) <- c("12.5%", "87.5%", "season")
rownames(class) <- ss.df$id

for (id in 1:110){
  class[id,1] <- quantile(individual.flights[[id]],c(.125,.875))[1]
  class[id,2] <- quantile(individual.flights[[id]],c(.125,.875))[2]
}

class <- as.data.frame(class)

for (id in 1:110){

  for (j in c(1, 3, 5, 7, 9, 11, 13, 15)){  # top of hierarchy: early/late
    if ( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
       (class[id,2] <= seasons[j,3])){
        class[id,3] <- as.character(seasons$season[j])
    }
  }

  if (is.na(class[id,3])){
    for (j in c(2, 6, 10, 14)){  # second hierarchy: whole season
      if ( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
         (class[id,2] <= seasons[j,3])){
          class[id,3] <- as.character(seasons$season[j])
      }
    }
  }

  if (is.na(class[id,3])){
    for (j in c(4, 8, 12, 16)){  # third hierarchy: two seasons
      if ( (class[id,1] >= seasons[j,2]) & (class[id,1] <= seasons[j,3]) &
         (class[id,2] <= seasons[j,3])){
          class[id,3] <- as.character(seasons$season[j])
      }
    }
  }
}

class[which(is.na(class[,3])),3] <- "none"

ss.df$season <- class$season

# and add colours
ss.df$col <- NA
for (i in 1:nrow(ss.df)){
    ss.df$col[i] <- seasons$colour[which(seasons$season == ss.df$season[i])]
}

rm(class, id, j)
