sensitivity <- data.frame(matrix(ncol=18,nrow=1))

colnames(sensitivity) <- c("X","Z","FFDPos","FFDNeg","FFDchi","FFDP","FFDSPos","FFDSNeg","FFDSchi","FFDSP","LFDPos","LFDNeg","LFDchi","LFDP","LFDSPos","LFDSNeg","LFDSchi","LFDSP")


X <- 3
Z <- 20

sensummary <- matrix(nrow = 393, ncol = 25) #summary of how many days a species was seen in each year

for(id in 1:393){
  for(yr in 1:25){
      sensummary[id,yr] <- length(which(moths[id,4:368,yr] >= 1))
}}

for(X in c(1,3,5,10,15,25)){
  for(Z in c(1,10,20,25)){

inc <- vector(length = 393, mode = "numeric")

for(id in 1:393){
  if(length(which(sensummary[id,] > X)) >= Z) inc[id] <- 1 #only include species seen on more than x days in Z or more years 
}

inc <- which(inc == 1)


senflight <- array(data = NA, dim = c(length(inc),2,25), dimnames = NULL)


# adjusted for abundance -----------------------------------------------


for(id in inc){ 
  
  ron <- rep( list(list()), 25 )   				#list of lists to put in flight days for all years
  
  for(yr in 1:25){								#for 25 years 
    temp <- as.numeric(moths[id,4:368,yr])		#how many flew on each day 
    temp[is.na(temp)] <- 0						#no flights is 0 
    temp1 <- 1									#just to star off list, deleted later	
    
    for(day in 1:365){							#which Julian days had a flight in them  
      temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
    }
    
    temp1 <- temp1[2:length(temp1)]
    
    ron[[yr]] <- temp1							#put that into list of lists 
    
  }
  
  #making array to put the sampled data into  
  #taking random samples to adjust each year down to the least abundant year 
  #(that can be counted, i.e. has > X days of sightings)
  
  ronRarf <- array(data = NA, dim = c(200,sort(as.numeric(msummary[id,4:28]))[which(sort(as.numeric(msummary[id,4:28])) >X)[1]],25), dimnames = NULL)
  
  for(yr in 1:25){
    if(as.numeric(msummary[id,yr+3]) >X){
      for (i in 1:200){
        ronRarf[i,,yr] <- sample(ron[[yr]], size=sort(as.numeric(msummary[id,4:28]))[which(sort(as.numeric(msummary[id,4:28])) >X)[1]])
        
        ronRarf[i,,yr] <- sort(ronRarf[i,,yr])
      }} else ronRarf[,,yr] <- NA
  }
  
  for(yr in 1:25){
    aaFFD <- vector(mode='numeric', length=200)
    
    for(i in 1:200){
      aaFFD[i] <- mean(unique(ronRarf[i,1:X,yr]))
    }
    
    senflight[which(inc == id),1,yr] <- mean(aaFFD)
    
  }
  
  for(yr in 1:25){
    aaLFD <- vector(mode='numeric', length=200)
    
    for(i in 1:200){
      aaLFD[i] <- mean(unique(ronRarf[i,(length(ronRarf[1,,1])-(X-1)):length(ronRarf[1,,1]),yr]))
    }
    
    senflight[which(inc == id),2,yr] <- mean(aaLFD)
    
    
  }
  
}

#regressions-------------------------------------------------------------


lupin <- data.frame(inc)

temp <- mtemp[31:55,1]

#FFD-

lupin$FFDslope <- 0
lupin$FFDsig <- 0

for(id in inc){
  #plot(temp, aaflight1[id,1,])
  
  a <- lm(senflight[which(inc == id),1,]~temp)
  #abline(coef(a)) 
  
  lupin$FFDslope[which(inc == id)] <- as.numeric(coef(a)[2])
  lupin$FFDsig[which(inc == id)] <- anova(a)$'Pr(>F)'[1]
  
}

#LFD-

lupin$LFDslope <- 0
lupin$LFDsig <- 0

for(id in inc){
  #plot(temp, senflight[which(inc == id),2,])
  
  a <- lm(senflight[which(inc == id),2,]~temp)
  #abline(coef(a)) 
  
  lupin$LFDslope[which(inc == id)] <- as.numeric(coef(a)[2])
  lupin$LFDsig[which(inc == id)] <- anova(a)$'Pr(>F)'[1]
  
}

length(which(lupin$FFDslope > 0))
length(which(lupin$FFDslope < 0))


test <- vector(length=18)

test[1] <- X
test[2] <- Z

test[3] <- length(which(lupin$FFDslope > 0))
test[4] <- length(which(lupin$FFDslope < 0))
test[5] <-  as.numeric(chisq.test(c(length(which(lupin$FFDslope > 0)),length(which(lupin$FFDslope < 0))))[1])
test[6] <-  as.numeric(chisq.test(c(length(which(lupin$FFDslope > 0)),length(which(lupin$FFDslope < 0))))[3])

test[7] <- length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] > 0))
test[8] <- length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] < 0))
test[9] <-  as.numeric(chisq.test(c(length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] > 0)),length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] < 0))))[1])
test[10] <- as.numeric(chisq.test(c(length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] > 0)),length(which(lupin$FFDslope[which(lupin$FFDsig < 0.05)] < 0))))[3])

test[11] <- length(which(lupin$LFDslope > 0))
test[12] <- length(which(lupin$LFDslope < 0))
test[13] <-  as.numeric(chisq.test(c(length(which(lupin$LFDslope > 0)),length(which(lupin$LFDslope < 0))))[1])
test[14] <-  as.numeric(chisq.test(c(length(which(lupin$LFDslope > 0)),length(which(lupin$LFDslope < 0))))[3])

test[15] <- length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] > 0))
test[16] <- length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] < 0))
test[17] <-  as.numeric(chisq.test(c(length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] > 0)),length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] < 0))))[1])
test[18] <- as.numeric(chisq.test(c(length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] > 0)),length(which(lupin$LFDslope[which(lupin$LFDsig < 0.05)] < 0))))[3])

sensitivity <- rbind(sensitivity, test)

}}

View(sensitivity)
