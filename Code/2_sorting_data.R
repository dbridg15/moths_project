#!usr/bin/env Rscript

# script: 2_sorting_data.R
# Desc: sorts out data ready for analysis
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# setting variables (if not already set)
###############################################################################

if (exists("X") == FALSE) { X <- 3  }  # no. sightings to take mean day from
if (exists("V") == FALSE) { V <- 1  }  # Minimum Flight Period
if (exists("N") == FALSE) { N <- 20 }  # Number of years with min flight period
if (exists("Z") == FALSE) { Z <- 5  }  # Threshold Temperature (for cons)
if (exists("Q") == FALSE) { Q <- 10 }  # Threshold number of Days (for cons)

###############################################################################
# flight - first flight day, last flight day, flight period for each species
###############################################################################

flight <- array(data = NA, dim = c(393,4,25), dimnames = NULL)

colnames(flight) <- c('FFD','LFD','FP','Fpos')

for (yr in 1:25){  # for 25 years of moth data
  for (id in 1:393){  # and all 393 species
    # list of days where species was seen
    FlightDays  <- which(moths[id,1:365,yr] > 0)
    if (length(FlightDays) > X){  # if there are enough to calculate from
      flight[id,1,yr] <- mean(FlightDays[1:X])
      flight[id,2,yr] <- (mean(FlightDays[(length(FlightDays)-(X-1)):
                               (length(FlightDays))]))
      flight[id,3,yr] <- (as.numeric(flight[id,2,yr]) -
                          as.numeric(flight[id,1,yr]))
      flight[id,4,yr] <- mean(flight[id,1:2,yr])
    }
  }
}

rownames(flight) <- all.spc.df$id

rm(FlightDays,id,yr)


###############################################################################
# selecting species for analysis
###############################################################################

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

rm(FP, id, selspc)

###############################################################################
# consecutive days (cons)
###############################################################################


# it will just be a list of day numbers
cons <- vector(mode = "numeric", length = 55)

for (yr in 1:55){
  temp <- vector(mode = "numeric", length = 365)

  for (i in Q:365){
    # does Q consecutive days starting at i have a temp above Z?
    temp[i] <- sum(as.numeric(Daily.Temp[(i-(Q-1)):i,yr] >= Z)) >=Q
  }
  cons[yr] <- which((temp)>0)[1]  # first time its true
}

# cleanup
rm(temp,yr,i)
