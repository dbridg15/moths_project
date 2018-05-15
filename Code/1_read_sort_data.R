#!usr/bin/env Rscript

# script: 1_read_sort_data.R
# Desc:   reads in moth and climate data and produces some useful summaries
# Author: David Bridgwood (dbridg15@gmail.com)


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
# msummary and moths.yrsum - yearly totals
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

# moths.yrsum sum for each day over the 25 years
moths.yrsum <- apply(moths, c(1,2), sum, na.rm = TRUE)


###############################################################################
# daily.temp (average daily temperature 1960 - 2014)
###############################################################################

daily.temp <- array(data = NA, dim = c(365,55,9), dimnames = NULL)

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

    daily.temp[,yr,cell] <- tmp
I   }
}

# average across all 9 grid squares
daily.temp <- rowMeans(daily.temp, dims = 2)

# cleanup
rm(cell, lpday, MeanTemp, tmp, yr)


###############################################################################
# consecutive days (cons)
###############################################################################


# it will just be a list of day numbers
cons <- vector(mode = "numeric", length = 55)

for (yr in 1:55){
  temp <- vector(mode = "numeric", length = 365)

  for (i in 10:365){
    # does 10 consecutive days starting at i have a temp above 5 degrees?
    temp[i] <- sum(as.numeric(daily.temp[(i-9):i,yr] >= 5)) >= 10
  }
  cons[yr] <- which((temp)>0)[1]  # first time its true
}

# cleanup
rm(temp,yr,i)


###############################################################################
# defining the seasons
###############################################################################

# list of seasons
seas.list <- c("early.spring", "spring", "late.spring", "spring.summer",
			   "early.summer", "summer", "late.summer", "summer.autumn",
			   "early.autumn", "autumn", "late.autumn", "autumn.winter",
			   "early.winter", "winter", "late.winter", "winter.spring",
               "none")

# list of first and last days
first.day <- c(60,60,107,60,152,152,199,152,244,244,290,244,335,335,1,1,1)
last.day  <- c(106,151,151,243,198,243,243,334,289,334,334,365,365,59,59,151,
               365)
# put into dataframe
seasons <- data.frame(seas.list, first.day, last.day)
colnames(seasons) <- c('season', 'first.day', 'last.day')
seasons$season <- factor(seasons$season, levels = seas.list)

# cleanup
rm(first.day, last.day)


###############################################################################
# classifiying species what does the middles 75% fly in?
###############################################################################

# list of vectors contain day all individuals were caught for given species
# i.e. if day 167 had 3 of that species lits would have ... 167, 167, 167, ...
individual.flights <- rep(list(c()), 393)

i <- 1  # starting counter
for (id in as.character(all.spc.df$id)){
  daily.spc.count <- as.numeric(moths.yrsum[id, ])  # list of daily counts
  daily.spc.count[is.na(daily.spc.count)] <- 0      # NAs to 0
  tmp <- c()

  for (day in 1:365){
  # tmp is a list with of days for each individual (i.e. 1, 2, 2, 2, 4, 5, 5)
  tmp <- c(tmp, seq(from = day, by=0, length.out = daily.spc.count[day]))
  }

  individual.flights[[i]] <- tmp
  i  <- i + 1  # iterate counter
}

# cleanup
rm(i, tmp, day, id, daily.spc.count)

# spc.class dataframe with lowerbound, upperbound and season
spc.class <- matrix(nrow = 393, ncol = 3)
colnames(spc.class) <- c("12.5%", "87.5%", "season")
rownames(spc.class) <- all.spc.df$id

for (id in 1:393){  # calculate the quantiles
  spc.class[id,1] <- quantile(individual.flights[[id]],c(.125,.875))[1]
  spc.class[id,2] <- quantile(individual.flights[[id]],c(.125,.875))[2]
}

spc.class <- as.data.frame(spc.class)

for (id in 1:393){
  for (j in c(1, 3, 5, 7, 9, 11, 13, 15)){  # top of hierarchy: early/late
    if ((spc.class[id,1] >= seasons[j,2]) &  # if lb is greater then fd of seas
        (spc.class[id,1] <= seasons[j,3]) &  # and lb is less than ld of seas
        (spc.class[id,2] <= seasons[j,3])){  # and ub is less than ld of seas
          # species season class is that season
          spc.class[id,3] <- as.character(seasons$season[j])
    }
  }
  if (is.na(spc.class[id,3])){
    for (j in c(2, 6, 10, 14)){  # second hierarchy: whole season
      if ((spc.class[id,1] >= seasons[j,2]) &
          (spc.class[id,1] <= seasons[j,3]) &
          (spc.class[id,2] <= seasons[j,3])){

            spc.class[id,3] <- as.character(seasons$season[j])
      }
    }
  }
  if (is.na(spc.class[id,3])){
    for (j in c(4, 8, 12, 16)){  # third hierarchy: two seasons
      if ((spc.class[id,1] >= seasons[j,2]) &
          (spc.class[id,1] <= seasons[j,3]) &
          (spc.class[id,2] <= seasons[j,3])){

            spc.class[id,3] <- as.character(seasons$season[j])
      }
    }
  }
}

# those with no class (NA) are given "none"
spc.class[which(is.na(spc.class[,3])),3] <- "none"

# put seasons into species dataframe
all.spc.df$season <- spc.class$season
all.spc.df$season <- factor(all.spc.df$season, levels = seas.list)

# cleanup
rm(spc.class, id, j, individual.flights)
