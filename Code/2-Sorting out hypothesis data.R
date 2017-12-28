
###########Sensitivity################-----------------------------------------------------------------------

X <- 3    # number of sightings to take mean day from
V <- 1    # Minimum Flight Period
N <- 20   # Number of years with minimum flight period
Z <- 5    # Threshold Temperature (for cons)
Q <- 10   # Threshold number of Days (for cons)


#Making flight-----------------------

flight <- array(data = NA, dim = c(393,3,25), dimnames = NULL)

colnames(flight) <- c('FFD','LFD','Flight Period')

for (yr in 1:25){
  
  for (id in 1:393){
    FlightDays  <- which(moths[id,4:368,yr] > 0)
    if(length(FlightDays) > X) flight[id,1,yr] <- (mean(FlightDays[1:X]))
    if(length(FlightDays) > X) flight[id,2,yr] <- (mean(FlightDays[(length(FlightDays)-(X-1)):(length(FlightDays))])) 
    if(length(FlightDays) > X) flight[id,3,yr] <- (as.numeric(flight[id,2,yr]) - as.numeric(flight[id,1,yr]))
  }
} 

rm(FlightDays,id,yr)

#selecting which species to use --> making flight1, moths1 and moths2 and msummary1 ---------------------

FP <- matrix(nrow=393, ncol=25)

#Which species meet the minimum requirement in what years 
for (yr in 1:25){
  for (id in 1:393){
    FP[id,yr] <- as.numeric(flight[id,3,yr] >= V)
  }
}

FP[is.na(FP)] <- 0

#which species meet the miniumum requirment for the minimum number of years and put these species into flight1 
# flight1 FFD, LFD, FP for selected speecies for all years 
temp <- vector(length=393, mode='numeric')

for (id in 1:393){
  temp[id] <- as.numeric(sum(FP[id,]) >= N)
}

flight1 <- array(data = NA, dim = c(length(which(temp > 0)),6,25), dimnames = NULL)

temp1 <- which(temp > 0)

flight1[,1,] <- temp1

for (i in temp1){
  flight1[which(temp1 == i),2,] <- msummary[i,2]
  flight1[which(temp1 == i),3,] <- msummary[i,3]
}

for (yr in 1:25){
  for (i in temp1){
    flight1[which(temp1 == i),4,yr] <- flight[i,1,yr]
    flight1[which(temp1 == i),5,yr] <- flight[i,2,yr]
    flight1[which(temp1 == i),6,yr] <- flight[i,3,yr]
  }
}

#moths1 same as moths but for selected species only 

moths1 <- array(data = NA, dim = c(length(flight1[,1,1]),368,25), dimnames = NULL)

for (i in as.numeric(flight1[,1,1])){
  moths1[which(flight1[,1,1]==i),,] <- moths[i,,]
}

rm(temp,temp1,yr,FP,id,i)

#moths2 total of sum of each day over 25 years

moths2 <- matrix(nrow=length(flight1[,1,1]),ncol=368)

for(id in 1:length(flight1[,1,1])){
  for( day in 4:368){
    moths2[id,1] <- moths1[id,1,1]
    moths2[id,2] <- moths1[id,2,1]
    moths2[id,3] <- moths1[id,3,1]
    moths2[id,day] <- sum(as.numeric(moths1[id,day,which(!is.na(moths1[id,day,]))]))
  }
}

rm(id,day)

#msummary1 --> msummary for selecyed species 

msummary1 <- matrix(nrow=length(flight1[,1,1]),ncol=28)

msummary1[,1:3] <- moths2[,1:3]

for (i in as.numeric(flight1[,1,1])){
  msummary1[which(flight1[,1,1]==i),4:28] <- msummary[i,4:28]
}



#Making Consecutive Days --> cons-----------------------------

cons <- vector(mode = "numeric", length = 55)

for(yr in 1:55){
  temp <- vector(mode = "numeric", length = 365)
  
  for (i in Q:365){
    if(sum(as.numeric(dtemp[(i-(Q-1)):i,yr] >= Z))>=10) temp[i] <- sum(as.numeric(dtemp[(i-(Q-1)):i,yr] >= Z))
    cons[yr] <- which((temp)>=10)[1]
  }
}

rm(temp,yr,i)


# adjusted for abundance 

aaflight1 <- array(data = NA, dim = c(110,4,25), dimnames = NULL)

colnames(aaflight1) <- c("aaFFD", "aaLFD", "aaFP", "aaFPos")

for(id in 1:110){ 
  
  ron <- rep( list(list()), 25 ) 					#list of lists to put in flight days for all years
  
  for(yr in 1:25){								#for 25 years 
    temp <- as.numeric(moths1[id,4:368,yr])		#how many flew on each day 
    temp[is.na(temp)] <- 0						#no flights is 0 
    temp1 <- 1									#just to star off list, deleted later	
    
    for(day in 1:365){							#which Julian days had a flight ion them  
      temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
    }
    
    temp1 <- temp1[2:length(temp1)]
    
    ron[[yr]] <- temp1							#put that into list of lists 
    
  }
  
  #making array to put the sampled data into  
  #taking random samples to adjust each year down to the least abundant year 
  #(that can be counted, i.e. has > X days of sightings)
  
  ronRarf <- array(data = NA, dim = c(200,sort(as.numeric(msummary1[id,4:28]))[which(sort(as.numeric(msummary1[id,4:28])) >X)[1]],25), dimnames = NULL)
  
  for(yr in 1:25){
    if(as.numeric(msummary1[id,yr+3]) >X){
      for (i in 1:200){
        ronRarf[i,,yr] <- sample(ron[[yr]], size=sort(as.numeric(msummary1[id,4:28]))[which(sort(as.numeric(msummary1[id,4:28])) >X)[1]])
        
        ronRarf[i,,yr] <- sort(ronRarf[i,,yr])
      }} else ronRarf[,,yr] <- NA
  }
  
  for(yr in 1:25){
    aaFFD <- vector(mode='numeric', length=200)
    
    for(i in 1:200){
      aaFFD[i] <- mean(unique(ronRarf[i,1:X,yr]))
    }
    
    aaflight1[id,1,yr] <- mean(aaFFD)
    
  }
  
  for(yr in 1:25){
    aaLFD <- vector(mode='numeric', length=200)
    
    for(i in 1:200){
      aaLFD[i] <- mean(unique(ronRarf[i,(length(ronRarf[1,,1])-(X-1)):length(ronRarf[1,,1]),yr]))
    }
    
    aaflight1[id,2,yr] <- mean(aaLFD)
    
  
  aaflight1[id,3,yr] <- aaflight1[id,2,yr] - aaflight1[id,1,yr]
  aaflight1[id,4,yr] <- mean(aaflight1[id,1:2,yr])
  }
 
  for(yr in 1:25){
    
    aaflight1[which(is.na(flight1[,4,yr])),,yr] <- NA
    
  }
  
}




#Cleanup------------

rm(N,Q,V,X,Z,aaFFD,aaLFD,i,id,ron,ronRarf,temp,temp1,yr)
