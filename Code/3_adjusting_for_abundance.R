#!usr/bin/env Rscript

# script: 3_adjusting_for_abundance.R
# Desc: adjusts FFD/LFD by sampling to least abundant year
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# adjusted for abundance
###############################################################################

aa.ss.flight <- array(data = NA, dim = c(nrow(ss.flight),4,25),
                      dimnames = NULL)

colnames(aa.ss.flight) <- c("FFD", "LFD", "FP", "Fpos")
rownames(aa.ss.flight) <- ss.df$id

for (id in as.character(ss.df$id)){

  # list of empty vectors to put in flight days for all years
  FD.list <- rep(list(c()), 25)

  for (yr in 1:25){
    temp <- as.numeric(ss.moths[id,,yr])  # how many flew on each day
    temp[is.na(temp)] <- 0  # no flights (NA) is 0
    temp1 <- c()

    # put day number in list as many times as species was seen on day
    for (day in 1:365){
      temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
    }
    FD.list[[yr]] <- temp1
  }

  # making array to put the sampled data into
  # taking random samples to adjust each year down to the least abundant year
  # (that can be counted, i.e. has > X days of sightings)

  # row - 200 = number of samples
  # columns - as many as the lowest count year (above the threshold X)
  minsize <- sort(as.numeric(ss.msummary[id,])
                  )[which(sort(as.numeric(ss.msummary[id, ])) >X)[1]]
  # z - 25 = years
  FD.Rarf <- array(data = NA, dim = c(200, minsize, 25), dimnames = NULL)

  for (yr in 1:25){
    # if the species was seen enough in teh year
    if (as.numeric(ss.msummary[id,yr]) > X){
      for (i in 1:200){  # do 200 times
        # sample the year to the size of the least abundant
        FD.Rarf[i,,yr] <- sort(sample(FD.list[[yr]], size = minsize,
                                      replace = FALSE))
      }
    } else {
      FD.Rarf[ , ,yr] <- NA
    }
  }

  # calculte FFD, LFP, Flight period and Flight Position for all these 200
  # subsamples then take the mean and put in aa.ss.flight
  for (yr in 1:25){
    aa.FFD <- vector(mode='numeric', length=200)
    aa.LFD <- vector(mode='numeric', length=200)

    for (i in 1:200){
      aa.FFD[i] <- mean(unique(FD.Rarf[i,1:X,yr]))
      aa.LFD[i] <- mean(unique(FD.Rarf[i,(length(FD.Rarf[1,,1])-(X-1)):
                  length(FD.Rarf[1,,1]),yr]))
    }
    aa.ss.flight[id,1,yr] <- mean(aa.FFD)
    aa.ss.flight[id,2,yr] <- mean(aa.LFD)

    aa.ss.flight[id,3,yr] <- aa.ss.flight[id, 2, yr] - aa.ss.flight[id, 1, yr]
    aa.ss.flight[id,4,yr] <- mean(aa.ss.flight[id, 1:2, yr])
  }

  # put NAs in yrs and for species where they are NA in ss.flight i.e. that
  # year did not meet inclusion criteria
  for (yr in 1:25){
    aa.ss.flight[which(is.na(ss.flight[ , 1, yr])), , yr] <- NA
  }
}

rm(aa.FFD, aa.LFD, yr, i, temp, temp1, id, day, FD.Rarf, FD.list, minsize)
