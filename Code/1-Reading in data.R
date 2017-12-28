#LoadPackages

library("lme4", lib.loc="~/R/win-library/3.1")
library("plyr", lib.loc="~/R/win-library/3.1")
library("scales", lib.loc="~/R/win-library/3.1")
library("sm", lib.loc="~/R/win-library/3.1")

# Making moths -------------------------------------------------------------------------------

moths <- array(data = NA, dim = c(393,368,25), dimnames = NULL) #make empty array to fill

indexlist <- read.csv("Data/CSVFiles/Speciesindexlist.csv", header=T) #species list with (id,BF,Name)

id <- as.numeric(indexlist[,1])
BF <- as.character(indexlist[,2])
Species <- as.character(indexlist[,3])

for(yr in 1:25){
  
  dat <- read.csv(paste('Data/CSVFiles/Moths', yr+1989, '.csv', sep=''), header=F) #read in the .csv files for all years (1990-2014)
  
  julianday <- as.numeric(dat[1,4:(dim(dat)[2])])
  dat[is.na(dat)] <- 0
  dat <- dat[-1,]
  
  
  for(s in 1:393){ #for all species populate Moths
    moths[s,1,yr] <- id[s]
    moths[s,2,yr] <- BF[s]
    moths[s,3,yr] <- Species[s]
  }
  
  for(d in 1:length(julianday)){ 
    day <- julianday[d]
    moths[dat$V1, day+3, yr] <- dat[,d+3]
  }
}

day <- read.csv('Data/CSVFiles/Days.csv') #read in day numbers where 0 moths were seen

for (yr in 1:25){ #for 1990-2014
  for (d in 1:sum(!is.na(day[,yr]))) #take the days that had 0 moths
    moths[,day[d,yr]+3,yr] <- 0 #and put 0's in that day for Moths
}


rm(dat,day,indexlist,BF,Species,d,id,julianday,s,yr)

# Making moths Summary ----------------------------------------------------------------------------

msummary <- matrix(nrow=393, ncol=28)

msummary[,1:3] <- moths[,1:3,1]

for(s in 1:393){
  
  for(yr in 1:25){
    msummary[s,yr+3] <- sum(as.numeric(moths[s,4:368,yr]),na.rm=TRUE) #Total of each species for each year
  }
}

rm(s,yr)

# Making dtemp (average daily temperature) pt.1. Reading in-------------------------------

DailyTemp <- array(data = NA, dim = c(3,365,52,9), dimnames = NULL)

for (cell in 1:9){ #For each of the 9 Grids
  MaxT <- read.csv(paste('Data/Climate/', cell, '/MaxTemp_', cell, '.csv', sep=''), header=F) #read in max temperature
  MinT <- read.csv(paste('Data/Climate/', cell, '/MinTemp_', cell, '.csv', sep=''), header=F) #min temperature
  MeanT <- read.csv(paste('Data/Climate/', cell, '/MeanTemp_', cell, '.csv', sep=''), header=F) #and mean temperature
  
  for (yr in 1:52){ #from 1960-2011
    
    for (d in ((yr*365)-364):(yr*365)){
      DailyTemp[1,(d-(365*(yr-1))),yr,cell] <- MaxT[d,6]
      DailyTemp[2,(d-(365*(yr-1))),yr,cell] <- MinT[d,6]
      DailyTemp[3,(d-(365*(yr-1))),yr,cell] <- MeanT[d,6]
    }
  }
}

DailyTempAvg <- array(data = NA, dim = c(3,365,52), dimnames = NULL) #Average across all 9 grid squares (15km^2)
for (x in 1:3){
  for (y in 1:365){
    for (z in 1:52){
      DailyTempAvg[x,y,z] <- mean(DailyTemp[x,y,z,1:9])
    }
  }
}

# Making dtemp (average daily temperature) pt.2. Correlate with CET-------------------------

CET <- read.csv("Data/Climate/Central England/CET.csv") #read in CET
CET <- CET[5828:7533,] #take only 1960 onwards 

CET1 <- matrix(nrow=55,ncol=365)

for (yr in 1:55){ # Getting it into a useful form (matrix with days and years)
  
  temp <- 1
  
  for(mth in 1:12){
    temp <- c(temp,CET[((yr*31)-30):(yr*31),mth+2])
  }
  
  temp <- temp[-1]
  temp <- temp[temp != -999]
  if (length(temp) > 365) temp <- temp[-60]
  
  CET1[yr,] <- temp
  
}
CET1 <- CET1/10 # convert to degrees not tenths of a degree

Temp <- matrix(nrow=18980,ncol=2)

for(yr in 1:52){
  Temp[((yr*365)-364):(yr*365),1] <- CET1[yr,]
  Temp[((yr*365)-364):(yr*365),2] <- DailyTempAvg[3,,yr]
}

colnames(Temp) <- c('CET','Grid')

Mod <- lm(Temp[,2]~Temp[,1]) # create model of CET with temperarture at haxby 

plot(Temp[,1],Temp[,2])
abline(Mod)

summary(Mod)


# Making dtemp (average daily temperature) pt.3. Get 2012;2014--------------------------------

Test <- matrix(nrow=1095,ncol=2)

for(yr in 1:3){
  Test[((yr*365)-364):(yr*365),1] <- CET1[yr+52,]
}

colnames(Test) <- c('CET','Grid')

Test <- as.data.frame(Test)

coef <- coef(Mod)

for(d in 1:1095){
  Test[d,2] <- ((coef[2]*(Test[d,1])) + coef[1]) #predict temperature in haxby based on CET 
}

dtemp <- matrix(nrow=365, ncol=55) #create a very nice matrix with temperature from 1960:2014

colnames(dtemp) <- c(1960:2014)

for(yr in 1:52){
  dtemp[,yr] <- DailyTempAvg[3,,yr]
}

dtemp[,53] <- Test[1:365,2]
dtemp[,54] <- Test[366:730,2]
dtemp[,55] <- Test[731:1095,2]

rm(CET,CET1,MaxT,MinT,MeanT,Temp,Test,DailyTemp,DailyTempAvg,Mod,cell,coef,d,mth,temp,x,y,z)

# Making mtemp (mean yearly and season temperatures)---------------------

mtemp <- matrix(nrow=55,ncol=5)

colnames(mtemp) <- c('Year','Winter','Spring','Summer','Autumn')

for (yr in 1:55){ #for every year take the mean temperature, overall and for each season
  mtemp[yr,1] <- mean(dtemp[,yr])
  if (yr > 1) { #because winter spans the year the average includes the previous years december (except in 1960 as i dont have the data for 1959)
    mtemp[yr,2] <- mean(mean(dtemp[335:365,yr-1])+mean(dtemp[1:59,yr]))} else{mtemp[yr,2] <- mean(dtemp[1:59,yr])}
  mtemp[yr,3] <- mean(dtemp[60:151,yr])
  mtemp[yr,4] <- mean(dtemp[152:243,yr])
  mtemp[yr,5] <- mean(dtemp[244:334,yr])
  
}

rm(yr)

#Making mrain---------------------------------

Rainfall <- array(data = NA, dim = c(102,12,9), dimnames = NULL)

#reading in the data for all 9 grid squares (monthly data from 1910:2011)

for (cell in 1:9){
  temprain <- read.csv(paste('Data/Climate/Rainfall/Rainfall_', cell, '.csv', sep=''), header=F)
  for (yr in 1:102){
    for (m in ((yr*12)-11):(yr*12)){
      Rainfall[yr,(m-(12*(yr-1))),cell] <- temprain[m,5]
    }
  }
}

#Getting the mean of all grid squares to give me the value for the 15km square...

rain <- matrix(nrow=12, ncol=102)

for (yr in 1:102){
  for (m in 1:12){
    rain[m,yr] <- mean(Rainfall[yr,m,1:9])
  }
}

colnames(rain) <- c(1910:2011)

mrain <- matrix(nrow=101,ncol=5)

colnames(mrain) <- c('Year','Winter','Spring','Summer','Autumn')

#Getting yearly and seasonal rainfall

for (yr in 2:102){
  mrain[yr-1,1] <- sum(rain[,yr])
  #because winter spans the year can only do 1911 onwards
  mrain[yr-1,2] <- sum(rain[1:2,yr]+rain[12,yr-1])
  mrain[yr-1,3] <- sum(rain[3:5,yr])
  mrain[yr-1,4] <- sum(rain[6:8,yr])
  mrain[yr-1,5] <- sum(rain[9:11,yr])
}



rm(Rainfall,yr,m,temprain,cell,rain)
