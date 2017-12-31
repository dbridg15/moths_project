#!usr/bin/env Rscript

# script: 2_sorting_data.R
# Desc: sorts out data ready for analysis
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# setting variables (if not already set)
###############################################################################

if(exists("X") == FALSE) { X <- 3 }   # no. sightings to take mean day from
if(exists("V") == FALSE) { V <- 1 }   # Minimum Flight Period
if(exists("N") == FALSE) { N <- 20 }  # Number of years with min flight period
if(exists("Z") == FALSE) { Z <- 5 }   # Threshold Temperature (for cons)
if(exists("Q") == FALSE) { Q <- 10 }  # Threshold number of Days (for cons)


###############################################################################
# flight - first flight day, last flight day, flight period for each species
###############################################################################

flight <- array(data = NA, dim = c(393,4,25), dimnames = NULL)

colnames(flight) <- c('FFD','LFD','FP','Fpos')

for (yr in 1:25){  # for 25 years of moth data
    for (id in 1:393){  # and all 393 species
        # list of days where species was seen
        FlightDays  <- which(moths[id,1:365,yr] > 0)
        if(length(FlightDays) > X){  # if there are enough to calculate from
            flight[id,1,yr] <- mean(FlightDays[1:X])
            flight[id,2,yr] <- (mean(FlightDays[(length(FlightDays)-(X-1)):
                                     (length(FlightDays))]))
            flight[id,3,yr] <- (as.numeric(flight[id,2,yr]) -
                                as.numeric(flight[id,1,yr]))
            flight[id,4,yr] <- mean(flight[id,1:2,yr])
        }
    }
}

rownames(flight) <- species_df$id

rm(FlightDays,id,yr)


###############################################################################
# selecting species for analysis
###############################################################################

FP <- matrix(nrow=393, ncol=25)

# Which species meet the minimum requirement in what years
for (yr in 1:25){
    for (id in 1:393){
        FP[id,yr] <- as.numeric(flight[id,3,yr] >= V)
    }
}

FP[is.na(FP)] <- 0

#which species meet the miniumum requirment for the minimum number of years
selspc <- vector(length=393, mode='numeric')
for (id in 1:393){
    selspc[id] <- as.numeric(sum(FP[id,]) >= N)
}

selspc <- which(selspc > 0)

# selspc_df is df with id, BF and common name of selected species
selspc_df <- subset(species_df, species_df$id %in% selspc)

# flight1 FFD, LFD, FP for selected speecies for all years
flight1 <- array(data = NA, dim = c(length(selspc),4,25),
                 dimnames = NULL)

colnames(flight1) <- c('FFD','LFD','FP','Fpos')
rownames(flight1) <- selspc_df$id

# flight1 is a subset of flight for the species in selspc
flight1 <- flight[as.character(selspc_df$id),,]

# moths1 same as moths but for selected species only
moths1 <- moths[as.character(selspc_df$id),,]

# moths2 sum for each day over the 25 years
moths2 <- apply(moths1, c(1,2), sum, na.rm = TRUE)

# msummary1 is msummary for selected species
msummary1 <- msummary[as.character(selspc_df$id),]

rm(FP, id, selspc)

###############################################################################
# consecutive days (cons)
###############################################################################

# it will just be a list of day numbers
cons <- vector(mode = "numeric", length = 55)

for(yr in 1:55){
    temp <- vector(mode = "numeric", length = 365)

    for (i in Q:365){
        # does Q consecutive days starting at i have a temp above Z?
        temp[i] <- sum(as.numeric(dtemp[(i-(Q-1)):i,yr] >= Z)) >=Q
    }
    cons[yr] <- which((temp)>0)[1]  # first time its true
}

# cleanup
rm(temp,yr,i)
