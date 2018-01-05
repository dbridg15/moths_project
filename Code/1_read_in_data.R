#!usr/bin/env Rscript

# script: 1_read_in_data.R
# Desc: reads in moth and climate data
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# Making moths - 3D array of all species counts everyday for all 25 years
###############################################################################

# make empty array to fill
moths <- array(data = NA, dim = c(393,365,25), dimnames = NULL)

# all.spc.df is list of all species with id BF and common name
all.spc.df  <- read.csv("../Data/Moths/Speciesindexlist.csv", header=T)
colnames(all.spc.df) <-  c('id', 'BF', 'common_name')

for (yr in 1:25){

  # read in the .csv files for all years (1990-2014)
  dat <- read.csv(paste('../Data/Moths/Moths', yr+1989, '.csv', sep=''),
				  header=F)
  # the days where traps were put out
  julianday <- as.numeric(dat[1,4:(dim(dat)[2])])
  dat[is.na(dat)] <- 0  # replace NAs with 0s
  dat <- dat[-1,]  # get rid of top row (column headings)

  # now add the species counts to the correct days
  for (d in 1:length(julianday)){
    day <- julianday[d]  # the day
    moths[dat$V1, day, yr] <- dat[,d+3]  # put in moths
  }
}

# read in day numbers where 0 moths were seen
zero.days <- read.csv('../Data/Moths/zero_days.csv')

for (yr in 1:25){  # for 1990-2014
  for (d in 1:sum(!is.na(zero.days[,yr])))  # take the days that had 0 moths
  moths[ ,zero.days[d, yr], yr] <- 0  # and put 0's in that day for Moths
}

rownames(moths) <- all.spc.df$id


# cleanup
rm(dat, day, zero.days, d, julianday, yr)


###############################################################################
# moths Summary - yearly totals
###############################################################################

msummary <- matrix(nrow=393, ncol=25)
rownames(msummary) <- all.spc.df$id

for (s in 1:393){
  for (yr in 1:25){
    # total of each species for each year
    msummary[s, yr] <- sum(as.numeric(moths[s, 1:365, yr]), na.rm = TRUE)
  }
}

# cleanup
rm(s, yr)


###############################################################################
# dtemp (average daily temperature 1960 - 2014)
###############################################################################

Daily.Temp <- array(data = NA, dim = c(365,55,9), dimnames = NULL)

for (cell in 1:9){  # for each of the 9 Grids

  # read in data
  MeanTemp <- read.csv(paste('../Data/Climate//MeanTemp_', cell, '.csv',
                             sep=''), header=F)

  for (yr in 1:55){  # from 1960-2014 (1:55)o
    tmp <- subset(MeanTemp, MeanTemp$V3 == (1959 + yr))
    tmp <- tmp$V6

    if (length(tmp) == 366){
      lpday  <- mean(tmp[59:60])
      tmp <- c(tmp[1:58], lpday, tmp[61:366])
    }

    Daily.Temp[,yr,cell] <- tmp
I   }
}

# average across all 9 grid squares
Daily.Temp <- rowMeans(Daily.Temp, dims = 2)

# cleanup
rm(cell, lpday, MeanTemp, tmp, yr)
