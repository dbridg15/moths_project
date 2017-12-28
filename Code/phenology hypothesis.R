id <- as.numeric(msummary1[,1])
bf <- msummary1[,2]
name <- msummary1[,3] 
season <- class[,3]



#yeartrend---------------------------------------------------

yeartrend <- data.frame(id,bf,name,season)

years <- 1990:2014


#FFD

for(s in 1:110){
  
a <- lm(aaflight1[s,1,]~years)

yeartrend$FFDslope[s] <- as.numeric(coef(a)[2])
yeartrend$FFDsig[s] <- anova(a)$'Pr(>F)'[1]

}

#LF

for(s in 1:110){
  
  a <- lm(aaflight1[s,2,]~years)
  
  yeartrend$LFDslope[s] <- as.numeric(coef(a)[2])
  yeartrend$LFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FP


for(s in 1:110){
  
  a <- lm(aaflight1[s,3,]~years)
  
  yeartrend$FPslope[s] <- as.numeric(coef(a)[2])
  yeartrend$FPsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FPos

for(s in 1:110){
  
  a <- lm(aaflight1[s,4,]~years)
  
  yeartrend$FPoslope[s] <- as.numeric(coef(a)[2])
  yeartrend$Fposig[s] <- anova(a)$'Pr(>F)'[1]
  
}


#ytemptrend---------------------------------------------------

ytemptrend <- data.frame(id,bf,name,season)

ytemp <- mtemp[31:55,1]


#FFD

for(s in 1:110){
  
  a <- lm(aaflight1[s,1,]~ytemp)
  
  ytemptrend$FFDslope[s] <- as.numeric(coef(a)[2])
  ytemptrend$FFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#LF

for(s in 1:110){
  
  a <- lm(aaflight1[s,2,]~ytemp)
  
  ytemptrend$LFDslope[s] <- as.numeric(coef(a)[2])
  ytemptrend$LFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FP


for(s in 1:110){
  
  a <- lm(aaflight1[s,3,]~ytemp)
  
  ytemptrend$FPslope[s] <- as.numeric(coef(a)[2])
  ytemptrend$FPsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FPos

for(s in 1:110){
  
  a <- lm(aaflight1[s,4,]~ytemp)
  
  ytemptrend$FPoslope[s] <- as.numeric(coef(a)[2])
  ytemptrend$Fposig[s] <- anova(a)$'Pr(>F)'[1]
  
}
ytemptrend


#constrend---------------------------------------------------

constrend <- data.frame(id,bf,name,season)

cons1 <- cons[31:55]


#FFD

for(s in 1:110){
  
  a <- lm(aaflight1[s,1,]~cons1)
  
  constrend$FFDslope[s] <- as.numeric(coef(a)[2])
  constrend$FFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#LF

for(s in 1:110){
  
  a <- lm(aaflight1[s,2,]~cons1)
  
  constrend$LFDslope[s] <- as.numeric(coef(a)[2])
  constrend$LFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FP


for(s in 1:110){
  
  a <- lm(aaflight1[s,3,]~cons1)
  
  constrend$FPslope[s] <- as.numeric(coef(a)[2])
  constrend$FPsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FPos

for(s in 1:110){
  
  a <- lm(aaflight1[s,4,]~cons1)
  
  constrend$FPoslope[s] <- as.numeric(coef(a)[2])
  constrend$Fposig[s] <- anova(a)$'Pr(>F)'[1]
  
}
constrend


#pwtrend---------------------------------------------------

pwtrend <- data.frame(id,bf,name,season)

pw <- mtemp[30:54,1]


#FFD

for(s in 1:110){
  
  a <- lm(aaflight1[s,1,]~pw)
  
  pwtrend$FFDslope[s] <- as.numeric(coef(a)[2])
  pwtrend$FFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#LF

for(s in 1:110){
  
  a <- lm(aaflight1[s,2,]~pw)
  
  pwtrend$LFDslope[s] <- as.numeric(coef(a)[2])
  pwtrend$LFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FP


for(s in 1:110){
  
  a <- lm(aaflight1[s,3,]~pw)
  
  pwtrend$FPslope[s] <- as.numeric(coef(a)[2])
  pwtrend$FPsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FPos

for(s in 1:110){
  
  a <- lm(aaflight1[s,4,]~pw)
  
  pwtrend$FPoslope[s] <- as.numeric(coef(a)[2])
  pwtrend$Fposig[s] <- anova(a)$'Pr(>F)'[1]
  
}

pwtrend


#sttrend -------------------------------------------

sttrend <- data.frame(id,bf,name,season,snum)



#FFD

for(s in 1:110){
  
  a <- lm(aaflight1[s,1,]~stemp[31:55,sttrend$snum[s]])
  
  sttrend$FFDslope[s] <- as.numeric(coef(a)[2])
  sttrend$FFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#LF

for(s in 1:110){
  
  a <- lm(aaflight1[s,2,]~stemp[31:55,sttrend$snum[s]])
  
  sttrend$LFDslope[s] <- as.numeric(coef(a)[2])
  sttrend$LFDsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FP


for(s in 1:110){
  
  a <- lm(aaflight1[s,3,]~stemp[31:55,sttrend$snum[s]])
  
  sttrend$FPslope[s] <- as.numeric(coef(a)[2])
  sttrend$FPsig[s] <- anova(a)$'Pr(>F)'[1]
  
}

#FPos

for(s in 1:110){
  
  a <- lm(aaflight1[s,4,]~stemp[31:55,sttrend$snum[s]])
  
  sttrend$FPoslope[s] <- as.numeric(coef(a)[2])
  sttrend$Fposig[s] <- anova(a)$'Pr(>F)'[1]
  
}

sttrend


plot(ytemptrend$FFDslope, constrend$FFDslope)

cor.test(ytemptrend$FFDslope, constrend$FFDslope)

plot(ytemptrend$LFDslope, constrend$LFDslope)

cor.test(ytemptrend$LFDslope, constrend$LFDslope)


plot(ytemptrend$FFDslope, pwtrend$FFDslope)

cor.test(ytemptrend$FFDslope, pwtrend$FFDslope)

plot(ytemptrend$LFDslope, pwtrend$LFDslope)

cor.test(ytemptrend$LFDslope, pwtrend$LFDslope)


plot(ytemptrend$FFDslope, sttrend$FFDslope)

cor.test(ytemptrend$FFDslope, sttrend$FFDslope)

plot(ytemptrend$LFDslope, sttrend$LFDslope)

cor.test(ytemptrend$LFDslope, sttrend$LFDslope)







#ALL SLOPES-FFD----------------------------------------------------

FFDyear <- vector(length=2, mode='numeric')

FFDyear[1] <- length(which(yeartrend$FFDslope < 0))
FFDyear[2] <- length(which(yeartrend$FFDslope > 0))

chisq.test(FFDyear)


FFDytemp <- vector(length=2, mode='numeric')

FFDytemp[1] <- length(which(ytemptrend$FFDslope < 0))
FFDytemp[2] <- length(which(ytemptrend$FFDslope > 0))

chisq.test(FFDytemp)


FFDcons <- vector(length=2, mode='numeric')

FFDcons[1] <- length(which(constrend$FFDslope < 0))
FFDcons[2] <- length(which(constrend$FFDslope > 0))

chisq.test(FFDcons)


FFDpw <- vector(length=2, mode='numeric')

FFDpw[1] <- length(which(pwtrend$FFDslope < 0))
FFDpw[2] <- length(which(pwtrend$FFDslope > 0))

chisq.test(FFDpw)


FFDst <- vector(length=2, mode='numeric')

FFDst[1] <- length(which(sttrend$FFDslope < 0))
FFDst[2] <- length(which(sttrend$FFDslope > 0))

chisq.test(FFDst)

#Significant only-FFD---------------------------------

FFDyearsig <- vector(length=2, mode='numeric')

FFDyearsig[1] <- length(which(yeartrend$FFDslope[which(yeartrend$FFDsig < 0.05)] < 0))
FFDyearsig[2] <- length(which(yeartrend$FFDslope[which(yeartrend$FFDsig < 0.05)] > 0))

chisq.test(FFDyearsig)


FFDytempsig <- vector(length=2, mode='numeric')

FFDytempsig[1] <- length(which(ytemptrend$FFDslope[which(ytemptrend$FFDsig< 0.05)] < 0))
FFDytempsig[2] <- length(which(ytemptrend$FFDslope[which(ytemptrend$FFDsig < 0.05)] > 0))

chisq.test(FFDytempsig)


FFDconssig <- vector(length=2, mode='numeric')

FFDconssig[1] <- length(which(constrend$FFDslope[which(constrend$FFDsig < 0.05)] < 0))
FFDconssig[2] <- length(which(constrend$FFDslope[which(constrend$FFDsig < 0.05)] > 0))

chisq.test(FFDconssig)


FFDpwsig <- vector(length=2, mode='numeric')

FFDpwsig[1] <- length(which(pwtrend$FFDslope[which(pwtrend$FFDsig < 0.05)] < 0))
FFDpwsig[2] <- length(which(pwtrend$FFDslope[which(pwtrend$FFDsig < 0.05)] > 0))

chisq.test(FFDpwsig)


FFDstsig <- vector(length=2, mode='numeric')

FFDstsig[1] <- length(which(sttrend$FFDslope[which(sttrend$FFDsig < 0.05)] < 0))
FFDstsig[2] <- length(which(sttrend$FFDslope[which(sttrend$FFDsig < 0.05)] > 0))

chisq.test(FFDstsig)

#ALL SLOPES-LFD----------------------------------------------------

LFDyear <- vector(length=2, mode='numeric')

LFDyear[1] <- length(which(yeartrend$LFDslope < 0))
LFDyear[2] <- length(which(yeartrend$LFDslope > 0))

chisq.test(LFDyear)


LFDytemp <- vector(length=2, mode='numeric')

LFDytemp[1] <- length(which(ytemptrend$LFDslope < 0))
LFDytemp[2] <- length(which(ytemptrend$LFDslope > 0))

chisq.test(LFDytemp)


LFDcons <- vector(length=2, mode='numeric')

LFDcons[1] <- length(which(constrend$LFDslope < 0))
LFDcons[2] <- length(which(constrend$LFDslope > 0))

chisq.test(LFDcons)


LFDpw <- vector(length=2, mode='numeric')

LFDpw[1] <- length(which(pwtrend$LFDslope < 0))
LFDpw[2] <- length(which(pwtrend$LFDslope > 0))

chisq.test(LFDpw)


LFDst <- vector(length=2, mode='numeric')

LFDst[1] <- length(which(sttrend$LFDslope < 0))
LFDst[2] <- length(which(sttrend$LFDslope > 0))

chisq.test(LFDst)

#Significant only-LFD---------------------------------

LFDyearsig <- vector(length=2, mode='numeric')

LFDyearsig[1] <- length(which(yeartrend$LFDslope[which(yeartrend$LFDsig < 0.05)] < 0))
LFDyearsig[2] <- length(which(yeartrend$LFDslope[which(yeartrend$LFDsig < 0.05)] > 0))

chisq.test(LFDyearsig)


LFDytempsig <- vector(length=2, mode='numeric')

LFDytempsig[1] <- length(which(ytemptrend$LFDslope[which(ytemptrend$LFDsig< 0.05)] < 0))
LFDytempsig[2] <- length(which(ytemptrend$LFDslope[which(ytemptrend$LFDsig < 0.05)] > 0))

chisq.test(LFDytempsig)


LFDconssig <- vector(length=2, mode='numeric')

LFDconssig[1] <- length(which(constrend$LFDslope[which(constrend$LFDsig < 0.05)] < 0))
LFDconssig[2] <- length(which(constrend$LFDslope[which(constrend$LFDsig < 0.05)] > 0))

chisq.test(LFDconssig)


LFDpwsig <- vector(length=2, mode='numeric')

LFDpwsig[1] <- length(which(pwtrend$LFDslope[which(pwtrend$LFDsig < 0.05)] < 0))
LFDpwsig[2] <- length(which(pwtrend$LFDslope[which(pwtrend$LFDsig < 0.05)] > 0))

chisq.test(LFDpwsig)


LFDstsig <- vector(length=2, mode='numeric')

LFDstsig[1] <- length(which(sttrend$LFDslope[which(sttrend$LFDsig < 0.05)] < 0))
LFDstsig[2] <- length(which(sttrend$LFDslope[which(sttrend$LFDsig < 0.05)] > 0))

chisq.test(LFDstsig)


#ALL SLOPES-FP----------------------------------------------------

FPyear <- vector(length=2, mode='numeric')

FPyear[1] <- length(which(yeartrend$FPslope < 0))
FPyear[2] <- length(which(yeartrend$FPslope > 0))

chisq.test(FPyear)


FPytemp <- vector(length=2, mode='numeric')

FPytemp[1] <- length(which(ytemptrend$FPslope < 0))
FPytemp[2] <- length(which(ytemptrend$FPslope > 0))

chisq.test(FPytemp)


FPcons <- vector(length=2, mode='numeric')

FPcons[1] <- length(which(constrend$FPslope < 0))
FPcons[2] <- length(which(constrend$FPslope > 0))

chisq.test(FPcons)


FPpw <- vector(length=2, mode='numeric')

FPpw[1] <- length(which(pwtrend$FPslope < 0))
FPpw[2] <- length(which(pwtrend$FPslope > 0))

chisq.test(FPpw)


FPst <- vector(length=2, mode='numeric')

FPst[1] <- length(which(sttrend$FPslope < 0))
FPst[2] <- length(which(sttrend$FPslope > 0))

chisq.test(FPst)

#Significant only-FP---------------------------------

FPyearsig <- vector(length=2, mode='numeric')

FPyearsig[1] <- length(which(yeartrend$FPslope[which(yeartrend$FPsig < 0.05)] < 0))
FPyearsig[2] <- length(which(yeartrend$FPslope[which(yeartrend$FPsig < 0.05)] > 0))

chisq.test(FPyearsig)


FPytempsig <- vector(length=2, mode='numeric')

FPytempsig[1] <- length(which(ytemptrend$FPslope[which(ytemptrend$FPsig< 0.05)] < 0))
FPytempsig[2] <- length(which(ytemptrend$FPslope[which(ytemptrend$FPsig < 0.05)] > 0))

chisq.test(FPytempsig)


FPconssig <- vector(length=2, mode='numeric')

FPconssig[1] <- length(which(constrend$FPslope[which(constrend$FPsig < 0.05)] < 0))
FPconssig[2] <- length(which(constrend$FPslope[which(constrend$FPsig < 0.05)] > 0))

chisq.test(FPconssig)


FPpwsig <- vector(length=2, mode='numeric')

FPpwsig[1] <- length(which(pwtrend$FPslope[which(pwtrend$FPsig < 0.05)] < 0))
FPpwsig[2] <- length(which(pwtrend$FPslope[which(pwtrend$FPsig < 0.05)] > 0))

chisq.test(FPpwsig)


FPstsig <- vector(length=2, mode='numeric')

FPstsig[1] <- length(which(sttrend$FPslope[which(sttrend$FPsig < 0.05)] < 0))
FPstsig[2] <- length(which(sttrend$FPslope[which(sttrend$FPsig < 0.05)] > 0))

chisq.test(FPstsig)
