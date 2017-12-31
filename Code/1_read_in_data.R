#!usr/bin/env Rscript

# script: 1_read_in_data.R
# Desc: reads in moth and climate data
# Author: David Bridgwood (dmb2417@ic.ac.uk)

rm(list = ls())


###############################################################################
# Making moths - 3D array of all species counts everyday for all 25 years
###############################################################################

# make empty array to fill
moths <- array(data = NA, dim = c(393,368,25), dimnames = NULL)

# species list with (id,BF,Name)
indexlist <- read.csv("../Data/Moths/Speciesindexlist.csv", header=T)

id <- as.numeric(indexlist[,1])
BF <- as.character(indexlist[,2])
Species <- as.character(indexlist[,3])

for(yr in 1:25){

    # read in the .csv files for all years (1990-2014)
    dat <- read.csv(paste('../Data/Moths/Moths', yr+1989, '.csv', sep=''),
                    header=F)
    julianday <- as.numeric(dat[1,4:(dim(dat)[2])])
    dat[is.na(dat)] <- 0  # replace NAs with 0s
    dat <- dat[-1,]  # get rid of top row (column headings)

    for(s in 1:393){  # for all species populate Moths with index and name
        moths[s, 1, yr] <- id[s]
        moths[s, 2, yr] <- BF[s]
        moths[s, 3, yr] <- Species[s]
    }

    # now add the species counts to the correct days
    for(d in 1:length(julianday)){
        day <- julianday[d]  # the day
        moths[dat$V1, day+3, yr] <- dat[,d+3]  # put in moths
    }
}

# read in day numbers where 0 moths were seen
zero_days <- read.csv('../Data/Moths/zero_days.csv')

for (yr in 1:25){  # for 1990-2014
  for (d in 1:sum(!is.na(zero_days[,yr])))  # take the days that had 0 moths
    moths[,zero_days[d,yr]+3,yr] <- 0  # and put 0's in that day for Moths
}

# cleanup
rm(dat, day, zero_days, indexlist, BF, Species, d, id, julianday, s, yr)


###############################################################################
# moths Summary - yearly totals
###############################################################################

msummary <- matrix(nrow=393, ncol=28)

msummary[,1:3] <- moths[,1:3,1]

for(s in 1:393){
    for(yr in 1:25){
        # total of each species for each year
        msummary[s,yr+3] <- sum(as.numeric(moths[s,4:368,yr]),na.rm=TRUE)
    }
}

# cleanup
rm(s,yr)


###############################################################################
# dtemp (average daily temperature 1960 - 2014)
###############################################################################

DailyTemp <- array(data = NA, dim = c(365,55,9), dimnames = NULL)

for (cell in 1:9){  # for each of the 9 Grids

    # read in data
    MeanTemp <- read.csv(paste('../Data/Climate//MeanTemp_', cell, '.csv',
                            sep=''), header=F)

    for (yr in 1:55){  # from 1960-2014 (1:55)o
        tmp <- subset(MeanTemp, MeanTemp$V3 == (1959 + yr))
        tmp <- tmp$V6

        if(length(tmp) == 366){
            lpday  <- mean(tmp[59:60])
            tmp <- c(tmp[1:58], lpday, tmp[61:366])
        }

        DailyTemp[,yr,cell] <- tmp
I   }
}

# average across all 9 grid squares
dtemp <- rowMeans(DailyTemp, dims = 2)

# cleanup
rm(cell, DailyTemp, lpday, MeanTemp, tmp, yr)


###############################################################################
# mtemp (mean yearly and season temperatures)
###############################################################################

mtemp <- matrix(nrow=55,ncol=5)

colnames(mtemp) <- c('Year','Winter','Spring','Summer','Autumn')

# for every year take the mean temperature, overall and for each season
for (yr in 1:55){

# because winter spans the year the average includes the previous years
# december (except in 1960 as i dont have the data for 1959)

    mtemp[yr,1] <- mean(dtemp[,yr])
    if (yr > 1) {
        mtemp[yr,2] <- mean(mean(dtemp[335:365,yr-1])+mean(dtemp[1:59,yr]))
    } else{
        mtemp[yr,2] <- mean(dtemp[1:59,yr])
    }

    mtemp[yr,3] <- mean(dtemp[60:151,yr])
    mtemp[yr,4] <- mean(dtemp[152:243,yr])
    mtemp[yr,5] <- mean(dtemp[244:334,yr])
}

rm(yr)
