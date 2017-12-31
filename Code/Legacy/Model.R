id <- vector(length = 2750)
FFD <- vector(length = 2750)
LFD <- vector(length = 2750)
FP <- vector(length = 2750)
FPos <- vector(length = 2750)
year <- vector(length = 2750)
temp <- vector(length = 2750)

for(yr in 1:25){
  id[((yr*110)-109):(yr*110)] <- luna$id
  season[((yr*110)-109):(yr*110)] <- as.character(luna$season)
  year[((yr*110)-109):(yr*110)] <- yr+1989
  temp[((yr*110)-109):(yr*110)] <- mtemp[30 +yr,1]
  FFD[((yr*110)-109):(yr*110)] <- aaflight1[,1,yr]
  LFD[((yr*110)-109):(yr*110)] <- aaflight1[,2,yr]
  FP[((yr*110)-109):(yr*110)] <- aaflight1[,3,yr]
  FPos[((yr*110)-109):(yr*110)] <- aaflight1[,4,yr]
  
}


dat <- data.frame(id, season, year, temp, FFD, LFD, FP, FPos)

color <- as.character(luna$col)
color


View(dat)

plot(dat$temp, dat$FFD)
abline(coef(lm(FFD~temp, data=dat)))


plot(dat$temp, dat$LFD)
abline(coef(lm(FFD~temp, data=dat)))


#FFD model---------------------------------------

ffdmodel <- lmer(FFD ~ (1|year) + (temp|id), data=dat)
summary(ffdmodel)

fixef(ffdmodel)
ffdrandeff <- ranef(ffdmodel)

plot(ffdrandeff$id[,1], ffdrandeff$id[,2])
ffdrandeff$year


temps <- 7:13

ffdmr <- vector(length=110)


plot(c(7,13),c(50,300),type='n')


for (s in 1:110){
  lines(temps, fixef(ffdmodel)[1] + ffdrandeff$id[s,1] + ffdrandeff$id[s,2]*temps, type='l', col = alpha(col=color[s], 0.8)) #average year
  #lines(temps, fixef(model)[1] + randeff$id[s,1] + randeff$id[s,2]*temps + randeff$year[22,1],type='l', col = color[s])
  ffdmr[s] <- coef(lm(fixef(ffdmodel)[1] + ffdrandeff$id[s,1] + ffdrandeff$id[s,2]*temps~temps))[2]
  
}



#LFD model------------------------------------------------

lfdmodel <- lmer(LFD ~ (1|year) + (temp|id), data=dat)
summary(lfdmodel)

fixef(lfdmodel)
lfdrandeff <- ranef(lfdmodel)

plot(lfdrandeff$id[,1], lfdrandeff$id[,2])
lfdrandeff$year


temps <- 7:13

lfdmr <- vector(length=110)

plot(c(7,13),c(0,365),type='n')


for (s in 1:110){
  lines(temps, fixef(lfdmodel)[1] + lfdrandeff$id[s,1] + lfdrandeff$id[s,2]*temps, type='l', col = alpha(col=color[s], 0.8)) #average year
  #lines(temps, fixef(model)[1] + randeff$id[s,1] + randeff$id[s,2]*temps + randeff$year[22,1],type='l', col = color[s])
  lfdmr[s] <- coef(lm(fixef(lfdmodel)[1] + lfdrandeff$id[s,1] + lfdrandeff$id[s,2]*temps~temps))[2]
  
}


plot(ffdmr, lfdmr)
abline(h=0,v=0)

range(ffdmr)
range(lfdmr)


hist(ffdmr, breaks = 18)
hist(lfdmr, breaks = 20)

#FFDmodel season hist!!!!---------------------------------------------

seasons <- c("autumn/winter", "late_autumn", "autumn", "early_autumn", "summer/autumn", "late_summer", "summer", "early_summer", "spring/summer", "late_spring", "spring", "early_spring", "late_winter", "winter", "early_winter", "winter/spring" ,"none")

mffdbins <- seq(-4.5, 3.5, 0.5)

mffdresponse <- matrix(nrow=17, ncol=17)

colnames(mffdresponse) <- seasons
rownames(mffdresponse) <- mffdbins


for(s in 1:17){
  
  for(i in 1:17){
    
    a <- which(ffdmr[which(luna$season == seasons[s])] >= mffdbins[i])
    b <- which(ffdmr[which(luna$season == seasons[s])] < (mffdbins[i]+0.5))
    
    mffdresponse[i,s] <- length(intersect(a,b))
  }
}


temp <- hist(ffdmr, breaks = 18, xlim = c(-4.5,4))#, plot = F)

plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-4.5,4) , xaxt='n', ann=FALSE, bty = "n")
  rect(mffdbins, 0, mffdbins +1, mffdresponse[,1], col = colfunc(16)[12], border = NA)
  rect(mffdbins, mffdresponse[,1], mffdbins +1, rowSums(mffdresponse[,1:2]), col = colfunc(16)[11], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:2]), mffdbins +0.5, rowSums(mffdresponse[,1:3]), col = colfunc(16)[10], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:3]), mffdbins +0.5, rowSums(mffdresponse[,1:4]), col = colfunc(16)[9], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:4]), mffdbins +0.5, rowSums(mffdresponse[,1:5]), col = colfunc(16)[8], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:5]), mffdbins +0.5, rowSums(mffdresponse[,1:6]), col = colfunc(16)[7], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:6]), mffdbins +0.5, rowSums(mffdresponse[,1:7]), col = colfunc(16)[6], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:7]), mffdbins +0.5, rowSums(mffdresponse[,1:8]), col = colfunc(16)[5], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:8]), mffdbins +0.5, rowSums(mffdresponse[,1:9]), col = colfunc(16)[4], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:9]), mffdbins +0.5, rowSums(mffdresponse[,1:10]), col = colfunc(16)[3], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:10]), mffdbins +0.5, rowSums(mffdresponse[,1:11]), col = colfunc(16)[2], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:11]), mffdbins +0.5, rowSums(mffdresponse[,1:12]), col = colfunc(16)[1], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:12]), mffdbins +0.5, rowSums(mffdresponse[,1:13]), col = colfunc(16)[16], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:13]), mffdbins +0.5, rowSums(mffdresponse[,1:14]), col = colfunc(16)[15], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:14]), mffdbins +0.5, rowSums(mffdresponse[,1:15]), col = colfunc(16)[14], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:15]), mffdbins +0.5, rowSums(mffdresponse[,1:16]), col = colfunc(16)[13], border = NA)
  rect(mffdbins, rowSums(mffdresponse[,1:16]), mffdbins +0.5, rowSums(mffdresponse[,1:17]), col = "grey", border = NA)


#LFDmodel season hist!!!!---------------------------------------------

hist(lfdmr, breaks = 20)

mlfdbins <- seq(-5.0, 4.5, 0.5)

mlfdresponse <- matrix(nrow=20, ncol=17)

colnames(mlfdresponse) <- seasons
rownames(mlfdresponse) <- mlfdbins


for(s in 1:17){
  
  for(i in 1:20){
    
    a <- which(lfdmr[which(luna$season == seasons[s])] >= mlfdbins[i])
    b <- which(lfdmr[which(luna$season == seasons[s])] < (mlfdbins[i]+0.5))
    
    mlfdresponse[i,s] <- length(intersect(a,b))
  }
}


temp <- hist(lfdmr, breaks = 18, xlim = c(-5.0,5.0))#, plot = F)

plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-5.0,5.0))#, yaxt='n', ann=FALSE, bty = "n")
  rect(0, mlfdbins, mlfdresponse[,1], mlfdbins +1, col = colfunc(16)[12], border = NA)
  rect(mlfdresponse[,1], mlfdbins, rowSums(mlfdresponse[,1:2]), mlfdbins +1,  col = colfunc(16)[11], border = NA)
  rect(rowSums(mlfdresponse[,1:2]), mlfdbins, rowSums(mlfdresponse[,1:3]), mlfdbins +0.5,  col = colfunc(16)[10], border = NA)
  rect(rowSums(mlfdresponse[,1:3]), mlfdbins, rowSums(mlfdresponse[,1:4]), mlfdbins +0.5,  col = colfunc(16)[9], border = NA)
  rect(rowSums(mlfdresponse[,1:4]), mlfdbins, rowSums(mlfdresponse[,1:5]), mlfdbins +0.5,  col = colfunc(16)[8], border = NA)
  rect(rowSums(mlfdresponse[,1:5]), mlfdbins, rowSums(mlfdresponse[,1:6]), mlfdbins +0.5,  col = colfunc(16)[7], border = NA)
  rect(rowSums(mlfdresponse[,1:6]), mlfdbins, rowSums(mlfdresponse[,1:7]), mlfdbins +0.5,  col = colfunc(16)[6], border = NA)
  rect(rowSums(mlfdresponse[,1:7]), mlfdbins, rowSums(mlfdresponse[,1:8]), mlfdbins +0.5,  col = colfunc(16)[5], border = NA)
  rect(rowSums(mlfdresponse[,1:8]), mlfdbins, rowSums(mlfdresponse[,1:9]), mlfdbins +0.5,  col = colfunc(16)[4], border = NA)
  rect(rowSums(mlfdresponse[,1:9]), mlfdbins, rowSums(mlfdresponse[,1:10]), mlfdbins +0.5,  col = colfunc(16)[3], border = NA)
  rect(rowSums(mlfdresponse[,1:10]), mlfdbins, rowSums(mlfdresponse[,1:11]), mlfdbins +0.5,  col = colfunc(16)[2], border = NA)
  rect(rowSums(mlfdresponse[,1:11]), mlfdbins, rowSums(mlfdresponse[,1:12]), mlfdbins +0.5,  col = colfunc(16)[1], border = NA)
  rect(rowSums(mlfdresponse[,1:12]), mlfdbins, rowSums(mlfdresponse[,1:13]), mlfdbins +0.5,  col = colfunc(16)[16], border = NA)
  rect(rowSums(mlfdresponse[,1:13]), mlfdbins, rowSums(mlfdresponse[,1:14]), mlfdbins +0.5,  col = colfunc(16)[15], border = NA)
  rect(rowSums(mlfdresponse[,1:14]), mlfdbins, rowSums(mlfdresponse[,1:15]), mlfdbins +0.5,  col = colfunc(16)[14], border = NA)
  rect(rowSums(mlfdresponse[,1:15]), mlfdbins, rowSums(mlfdresponse[,1:16]), mlfdbins +0.5,  col = colfunc(16)[13], border = NA)
  rect(rowSums(mlfdresponse[,1:16]), mlfdbins, rowSums(mlfdresponse[,1:17]), mlfdbins +0.5,  col = "grey", border = NA)

#Actual plot-------------------------------------------------------

par(fig=c(0,0.8,0,0.8))
 
  plot(ffdmr,lfdmr, xlim = c(-4.5,4), ylim = c(-5,5))
  abline(h=0,v=0)

par(fig=c(0,0.8,0.5,1), new=TRUE)

  temp <- hist(ffdmr, breaks = 18, xlim = c(-4.5,4), plot = F)
  
  plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-4.5,4) , xaxt='n', ann=FALSE, bty = "n")
    rect(mffdbins, 0, mffdbins +1, mffdresponse[,1], col = colfunc(16)[12], border = NA)
    rect(mffdbins, mffdresponse[,1], mffdbins +1, rowSums(mffdresponse[,1:2]), col = colfunc(16)[11], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:2]), mffdbins +0.5, rowSums(mffdresponse[,1:3]), col = colfunc(16)[10], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:3]), mffdbins +0.5, rowSums(mffdresponse[,1:4]), col = colfunc(16)[9], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:4]), mffdbins +0.5, rowSums(mffdresponse[,1:5]), col = colfunc(16)[8], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:5]), mffdbins +0.5, rowSums(mffdresponse[,1:6]), col = colfunc(16)[7], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:6]), mffdbins +0.5, rowSums(mffdresponse[,1:7]), col = colfunc(16)[6], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:7]), mffdbins +0.5, rowSums(mffdresponse[,1:8]), col = colfunc(16)[5], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:8]), mffdbins +0.5, rowSums(mffdresponse[,1:9]), col = colfunc(16)[4], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:9]), mffdbins +0.5, rowSums(mffdresponse[,1:10]), col = colfunc(16)[3], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:10]), mffdbins +0.5, rowSums(mffdresponse[,1:11]), col = colfunc(16)[2], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:11]), mffdbins +0.5, rowSums(mffdresponse[,1:12]), col = colfunc(16)[1], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:12]), mffdbins +0.5, rowSums(mffdresponse[,1:13]), col = colfunc(16)[16], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:13]), mffdbins +0.5, rowSums(mffdresponse[,1:14]), col = colfunc(16)[15], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:14]), mffdbins +0.5, rowSums(mffdresponse[,1:15]), col = colfunc(16)[14], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:15]), mffdbins +0.5, rowSums(mffdresponse[,1:16]), col = colfunc(16)[13], border = NA)
    rect(mffdbins, rowSums(mffdresponse[,1:16]), mffdbins +0.5, rowSums(mffdresponse[,1:17]), col = "grey", border = NA)

par(fig=c(0.63,1,0,0.8), new=TRUE)

  temp <- hist(lfdmr, breaks = 18, xlim = c(-5.0,5.0), plot = F)

  plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-5.0,5.0), yaxt='n', ann=FALSE, bty = "n")
    rect(0, mlfdbins, mlfdresponse[,1], mlfdbins +1, col = colfunc(16)[12], border = NA)
    rect(mlfdresponse[,1], mlfdbins, rowSums(mlfdresponse[,1:2]), mlfdbins +1,  col = colfunc(16)[11], border = NA)
    rect(rowSums(mlfdresponse[,1:2]), mlfdbins, rowSums(mlfdresponse[,1:3]), mlfdbins +0.5,  col = colfunc(16)[10], border = NA)
    rect(rowSums(mlfdresponse[,1:3]), mlfdbins, rowSums(mlfdresponse[,1:4]), mlfdbins +0.5,  col = colfunc(16)[9], border = NA)
    rect(rowSums(mlfdresponse[,1:4]), mlfdbins, rowSums(mlfdresponse[,1:5]), mlfdbins +0.5,  col = colfunc(16)[8], border = NA)
    rect(rowSums(mlfdresponse[,1:5]), mlfdbins, rowSums(mlfdresponse[,1:6]), mlfdbins +0.5,  col = colfunc(16)[7], border = NA)
    rect(rowSums(mlfdresponse[,1:6]), mlfdbins, rowSums(mlfdresponse[,1:7]), mlfdbins +0.5,  col = colfunc(16)[6], border = NA)
    rect(rowSums(mlfdresponse[,1:7]), mlfdbins, rowSums(mlfdresponse[,1:8]), mlfdbins +0.5,  col = colfunc(16)[5], border = NA)
    rect(rowSums(mlfdresponse[,1:8]), mlfdbins, rowSums(mlfdresponse[,1:9]), mlfdbins +0.5,  col = colfunc(16)[4], border = NA)
    rect(rowSums(mlfdresponse[,1:9]), mlfdbins, rowSums(mlfdresponse[,1:10]), mlfdbins +0.5,  col = colfunc(16)[3], border = NA)
    rect(rowSums(mlfdresponse[,1:10]), mlfdbins, rowSums(mlfdresponse[,1:11]), mlfdbins +0.5,  col = colfunc(16)[2], border = NA)
    rect(rowSums(mlfdresponse[,1:11]), mlfdbins, rowSums(mlfdresponse[,1:12]), mlfdbins +0.5,  col = colfunc(16)[1], border = NA)
    rect(rowSums(mlfdresponse[,1:12]), mlfdbins, rowSums(mlfdresponse[,1:13]), mlfdbins +0.5,  col = colfunc(16)[16], border = NA)
    rect(rowSums(mlfdresponse[,1:13]), mlfdbins, rowSums(mlfdresponse[,1:14]), mlfdbins +0.5,  col = colfunc(16)[15], border = NA)
    rect(rowSums(mlfdresponse[,1:14]), mlfdbins, rowSums(mlfdresponse[,1:15]), mlfdbins +0.5,  col = colfunc(16)[14], border = NA)
    rect(rowSums(mlfdresponse[,1:15]), mlfdbins, rowSums(mlfdresponse[,1:16]), mlfdbins +0.5,  col = colfunc(16)[13], border = NA)
    rect(rowSums(mlfdresponse[,1:16]), mlfdbins, rowSums(mlfdresponse[,1:17]), mlfdbins +0.5,  col = "grey", border = NA)






#FP---------------------
#LFD model------------------------------------------------

fpmodel <- lmer(FP ~ (1|year) + (temp|id), data=dat)
summary(fpmodel)

fixef(fpmodel)
fprandeff <- ranef(fpmodel)

plot(fprandeff$id[,1], fprandeff$id[,2])
fprandeff$year


temps <- 7:13

fpmr <- vector(length=110)

plot(c(7,13),c(0,365),type='n')


for (s in 1:110){
  lines(temps, fixef(fpmodel)[1] + fprandeff$id[s,1] + fprandeff$id[s,2]*temps, type='l', col = alpha(col=color[s], 0.8)) #average year
  #lines(temps, fixef(model)[1] + randeff$id[s,1] + randeff$id[s,2]*temps + randeff$year[22,1],type='l', col = color[s])
  fpmr[s] <- coef(lm(fixef(fpmodel)[1] + fprandeff$id[s,1] + fprandeff$id[s,2]*temps~temps))[2]
  
}


plot(ffdmr, fpmr)
abline(h=0,v=0)

hist(ffdmr, breaks = 18)
hist(fpmr, breaks = 18)

#FFDmodel season hist!!!!---------------------------------------------

mffdbins <- seq(-4.5, 3.5, 0.5)

mffdresponse <- matrix(nrow=17, ncol=17)

colnames(mffdresponse) <- seasons
rownames(mffdresponse) <- mffdbins


for(s in 1:17){
  
  for(i in 1:17){
    
    a <- which(ffdmr[which(luna$season == seasons[s])] >= mffdbins[i])
    b <- which(ffdmr[which(luna$season == seasons[s])] < (mffdbins[i]+0.5))
    
    mffdresponse[i,s] <- length(intersect(a,b))
  }
}


temp <- hist(ffdmr, breaks = 18, xlim = c(-4.5,4))#, plot = F)

plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-4.5,4) , xaxt='n', ann=FALSE, bty = "n")
rect(mffdbins, 0, mffdbins +1, mffdresponse[,1], col = colfunc(16)[12], border = NA)
rect(mffdbins, mffdresponse[,1], mffdbins +1, rowSums(mffdresponse[,1:2]), col = colfunc(16)[11], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:2]), mffdbins +0.5, rowSums(mffdresponse[,1:3]), col = colfunc(16)[10], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:3]), mffdbins +0.5, rowSums(mffdresponse[,1:4]), col = colfunc(16)[9], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:4]), mffdbins +0.5, rowSums(mffdresponse[,1:5]), col = colfunc(16)[8], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:5]), mffdbins +0.5, rowSums(mffdresponse[,1:6]), col = colfunc(16)[7], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:6]), mffdbins +0.5, rowSums(mffdresponse[,1:7]), col = colfunc(16)[6], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:7]), mffdbins +0.5, rowSums(mffdresponse[,1:8]), col = colfunc(16)[5], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:8]), mffdbins +0.5, rowSums(mffdresponse[,1:9]), col = colfunc(16)[4], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:9]), mffdbins +0.5, rowSums(mffdresponse[,1:10]), col = colfunc(16)[3], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:10]), mffdbins +0.5, rowSums(mffdresponse[,1:11]), col = colfunc(16)[2], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:11]), mffdbins +0.5, rowSums(mffdresponse[,1:12]), col = colfunc(16)[1], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:12]), mffdbins +0.5, rowSums(mffdresponse[,1:13]), col = colfunc(16)[16], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:13]), mffdbins +0.5, rowSums(mffdresponse[,1:14]), col = colfunc(16)[15], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:14]), mffdbins +0.5, rowSums(mffdresponse[,1:15]), col = colfunc(16)[14], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:15]), mffdbins +0.5, rowSums(mffdresponse[,1:16]), col = colfunc(16)[13], border = NA)
rect(mffdbins, rowSums(mffdresponse[,1:16]), mffdbins +0.5, rowSums(mffdresponse[,1:17]), col = "grey", border = NA)


#fpmodel season hist!!!!---------------------------------------------

hist(fpmr, breaks = 21)

mfpbins <- seq(-5.0, 5.0, 0.5)

mfpresponse <- matrix(nrow=21, ncol=17)

colnames(mfpresponse) <- seasons
rownames(mfpresponse) <- mfpbins


for(s in 1:17){
  
  for(i in 1:20){
    
    a <- which(fpmr[which(luna$season == seasons[s])] >= mfpbins[i])
    b <- which(fpmr[which(luna$season == seasons[s])] < (mfpbins[i]+0.5))
    
    mfpresponse[i,s] <- length(intersect(a,b))
  }
}


temp <- hist(fpmr, breaks = 18, xlim = c(-5.0,5.0))#, plot = F)

plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-5.0,5.0))#, yaxt='n', ann=FALSE, bty = "n")
rect(0, mfpbins, mfpresponse[,1], mfpbins +1, col = colfunc(16)[12], border = NA)
rect(mfpresponse[,1], mfpbins, rowSums(mfpresponse[,1:2]), mfpbins +1,  col = colfunc(16)[11], border = NA)
rect(rowSums(mfpresponse[,1:2]), mfpbins, rowSums(mfpresponse[,1:3]), mfpbins +0.5,  col = colfunc(16)[10], border = NA)
rect(rowSums(mfpresponse[,1:3]), mfpbins, rowSums(mfpresponse[,1:4]), mfpbins +0.5,  col = colfunc(16)[9], border = NA)
rect(rowSums(mfpresponse[,1:4]), mfpbins, rowSums(mfpresponse[,1:5]), mfpbins +0.5,  col = colfunc(16)[8], border = NA)
rect(rowSums(mfpresponse[,1:5]), mfpbins, rowSums(mfpresponse[,1:6]), mfpbins +0.5,  col = colfunc(16)[7], border = NA)
rect(rowSums(mfpresponse[,1:6]), mfpbins, rowSums(mfpresponse[,1:7]), mfpbins +0.5,  col = colfunc(16)[6], border = NA)
rect(rowSums(mfpresponse[,1:7]), mfpbins, rowSums(mfpresponse[,1:8]), mfpbins +0.5,  col = colfunc(16)[5], border = NA)
rect(rowSums(mfpresponse[,1:8]), mfpbins, rowSums(mfpresponse[,1:9]), mfpbins +0.5,  col = colfunc(16)[4], border = NA)
rect(rowSums(mfpresponse[,1:9]), mfpbins, rowSums(mfpresponse[,1:10]), mfpbins +0.5,  col = colfunc(16)[3], border = NA)
rect(rowSums(mfpresponse[,1:10]), mfpbins, rowSums(mfpresponse[,1:11]), mfpbins +0.5,  col = colfunc(16)[2], border = NA)
rect(rowSums(mfpresponse[,1:11]), mfpbins, rowSums(mfpresponse[,1:12]), mfpbins +0.5,  col = colfunc(16)[1], border = NA)
rect(rowSums(mfpresponse[,1:12]), mfpbins, rowSums(mfpresponse[,1:13]), mfpbins +0.5,  col = colfunc(16)[16], border = NA)
rect(rowSums(mfpresponse[,1:13]), mfpbins, rowSums(mfpresponse[,1:14]), mfpbins +0.5,  col = colfunc(16)[15], border = NA)
rect(rowSums(mfpresponse[,1:14]), mfpbins, rowSums(mfpresponse[,1:15]), mfpbins +0.5,  col = colfunc(16)[14], border = NA)
rect(rowSums(mfpresponse[,1:15]), mfpbins, rowSums(mfpresponse[,1:16]), mfpbins +0.5,  col = colfunc(16)[13], border = NA)
rect(rowSums(mfpresponse[,1:16]), mfpbins, rowSums(mfpresponse[,1:17]), mfpbins +0.5,  col = "grey", border = NA)