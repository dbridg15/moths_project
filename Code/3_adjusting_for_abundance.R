#!usr/bin/env Rscript

# script: 3_adjusting_for_abundance.R
# Desc: adjusts FFD/LFD by sampling to least abundant year
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# adjusted for abundance
###############################################################################

aaflight1 <- array(data = NA, dim = c(nrow(flight1),4,25), dimnames = NULL)

colnames(aaflight1) <- c("aaFFD", "aaLFD", "aaFP", "aaFPos")
rownames(aaflight1) <- selspc_df$id

for(id in as.character(selspc_df$id)){

    # list of empty vectors to put in flight days for all years
    FDlist <- rep( list(c()), 25)

    for(yr in 1:25){
        temp <- as.numeric(moths1[id,,yr])  # how many flew on each day
        temp[is.na(temp)] <- 0  # no flights (NA) is 0
        temp1 <- c()

        # put day number in list as many times as species was seen on day
        for(day in 1:365){
            temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
        }
        FDlist[[yr]] <- temp1
    }

    # making array to put the sampled data into
    # taking random samples to adjust each year down to the least abundant year
    # (that can be counted, i.e. has > X days of sightings)

    # row - 200 = number of samples
    # columns - as many as the lowest count year (above the threshold X)
    minsize <- sort(as.numeric(msummary1[id,])
                    )[which(sort(as.numeric(msummary1[id, ])) >X)[1]]
    # z - 25 = years
    FDRarf <- array(data = NA, dim = c(200, minsize, 25), dimnames = NULL)

    for(yr in 1:25){
        # if the species was seen enough in teh year
        if(as.numeric(msummary1[id,yr]) > X){
            for (i in 1:200){  # do 200 times
                # sample the year to the size of the least abundant
                FDRarf[i,,yr] <- sort(sample(FDlist[[yr]], size = minsize,
                                             replace = FALSE))
            }
        } else {
            FDRarf[,,yr] <- NA
        }
    }

    # calculte FFD, LFP, Flight period and Flight Position for all these 200
    # subsamples then take the mean and put in aaflight1
    for(yr in 1:25){
        aaFFD <- vector(mode='numeric', length=200)
        aaLFD <- vector(mode='numeric', length=200)

        for(i in 1:200){
            aaFFD[i] <- mean(unique(FDRarf[i,1:X,yr]))
            aaLFD[i] <- mean(unique(FDRarf[i,(length(FDRarf[1,,1])-(X-1)):
                                    length(FDRarf[1,,1]),yr]))
        }
        aaflight1[id,1,yr] <- mean(aaFFD)
        aaflight1[id,2,yr] <- mean(aaLFD)

        aaflight1[id,3,yr] <- aaflight1[id,2,yr] - aaflight1[id,1,yr]
        aaflight1[id,4,yr] <- mean(aaflight1[id,1:2,yr])
    }

    # put NAs in yrs and for species where they are NA in flight1 i.e. that
    # year did not meet inclusion criteria
    for(yr in 1:25){
        aaflight1[which(is.na(flight1[,1,yr])),,yr] <- NA
    }
}

rm(aaFFD, aaLFD, yr, i, temp, temp1, id, day, FDRarf, FDlist, minsize)
