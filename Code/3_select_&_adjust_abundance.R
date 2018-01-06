#!usr/bin/env Rscript

# script: 3_select_&_adjust_abundance.R
# Desc:   sorts out data ready for analysis
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# setting variables (if not already set)
###############################################################################

if (exists("X") == FALSE) { X <- 3  }  # no. sightings to take mean day from
if (exists("V") == FALSE) { V <- 1  }  # Minimum Flight Period
if (exists("N") == FALSE) { N <- 20 }  # Number of years with min flight period

###############################################################################
# flight - first flight day, last flight day, flight period for each species
###############################################################################

# set up array to hold flight data for all species
flight <- array(data = NA, dim = c(393,4,25), dimnames = NULL)
colnames(flight) <- c('FFD','LFD','FP','Fpos')
rownames(flight) <- all.spc.df$id

for (yr in 1:25){     # for 25 years of moth data
  for (id in 1:393){  # and all 393 species
    # list of days where species was seen
    FlightDays  <- which(moths[id,1:365,yr] > 0)
    if (length(FlightDays) > X){  # if there are enough to calculate from
      # FFD is mean of first X days
      flight[id,1,yr] <- mean(FlightDays[1:X])
      # LFD is mean of last X days
      flight[id,2,yr] <- (mean(FlightDays[(length(FlightDays)-(X-1)):
                               (length(FlightDays))]))
      # flight period is LFD - FFD
      flight[id,3,yr] <- (as.numeric(flight[id,2,yr]) -
                          as.numeric(flight[id,1,yr]))
      # flight position is middle of FFD and LFD
      flight[id,4,yr] <- mean(flight[id,1:2,yr])
    }
  }
}

# cleanup
rm(FlightDays,id,yr)


###############################################################################
# selecting species for analysis
###############################################################################

# matrix to hold flight period for all species in all years
FP <- matrix(nrow=393, ncol=25)

# Which species meet the minimum requirement in what years
for (yr in 1:25){
  for (id in 1:393){
    FP[id,yr] <- as.numeric(flight[id, 3, yr] >= V)
  }
}

FP[is.na(FP)] <- 0

#which species meet the miniumum requirment for the minimum number of years
selspc <- vector(length=393, mode='numeric')
for (id in 1:393){
  selspc[id] <- as.numeric(sum(FP[id,]) >= N)
}

selspc <- which(selspc > 0)

# ss.df is df with id, BF and common name of selected species
ss.df <- subset(all.spc.df, all.spc.df$id %in% selspc)

# ss.flight FFD, LFD, FP for selected speecies for all years
ss.flight <- array(data = NA, dim = c(length(selspc),4,25), dimnames = NULL)

colnames(ss.flight) <- c('FFD','LFD','FP','Fpos')
rownames(ss.flight) <- ss.df$id

# ss.flight is a subset of flight for the species in selspc
ss.flight <- flight[as.character(ss.df$id), , ]

# ss.moths same as moths but for selected species only
ss.moths <- moths[as.character(ss.df$id), , ]

# ss.moths.yrsum sum for each day over the 25 years
ss.moths.yrsum <- apply(ss.moths, c(1,2), sum, na.rm = TRUE)

# ss.msummary is msummary for selected species
ss.msummary <- msummary[as.character(ss.df$id),]

# number of selected species
ss.num <- nrow(ss.df)

# cleanup
rm(FP, id, selspc, flight)


###############################################################################
# adjusted for abundance
###############################################################################

# abundance adjusted flight for selected species
aa.ss.flight <- array(data = NA, dim = c(nrow(ss.flight),4,25),
                      dimnames = NULL)
colnames(aa.ss.flight) <- c("FFD", "LFD", "FP", "Fpos")
rownames(aa.ss.flight) <- ss.df$id

for (id in as.character(ss.df$id)){

  # list of empty vectors to put in flight days for all years
  FD.list <- rep(list(c()), 25)

  for (yr in 1:25){
    temp <- as.numeric(ss.moths[id,,yr])  # how many flew on each day
    temp[is.na(temp)] <- 0                # no flights (NA) is 0
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

# cleanup
rm(aa.FFD, aa.LFD, yr, i, temp, temp1, id, day, FD.Rarf, FD.list, minsize)
