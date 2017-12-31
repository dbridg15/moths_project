id <- 56

FFD <- aaflight1[id,1,]
LFD <- aaflight1[id,2,]

yt <- mtemp[31:55,1]
spt <- mtemp[30:54,2]
st <- stemp$SumAut[31:55]
spt <- cons[31:55]


svg("shc.svg")

par(mfrow=c(2,2))


#yt--------------------------------------------

for(s in 56){
  
  plot(c(8.5,11), c(round_any(min(aaflight1[s,1,], na.rm = T), 10, floor),round_any(max(aaflight1[s,2,], na.rm = T), 10, ceiling)), type = 'n',  xlab = "Mean Year Temperature (°C)", ylab = "Days")
  
  points(yt, aaflight1[s,1,], col = "red", pch = 19, type = 'p')
  points(yt, aaflight1[s,2,], col = "blue", pch = 19)
  
  
  a <- lm(aaflight1[s,1,]~yt)
  b <- lm(aaflight1[s,2,]~yt)
          
  abline(coef(a), col = "red")
  abline(coef(b), col = "blue")
  
  
}


#wt------------------------------------

for(s in 56){
  
  plot(c(5,11), c(round_any(min(aaflight1[s,1,], na.rm = T), 10, floor),round_any(max(aaflight1[s,2,], na.rm = T), 10, ceiling)), type = 'n',  xlab = "Previous Winter Temperature (°C)", ylab = "Days")
  
  points(wt, aaflight1[s,1,], col = "red", pch = 19, type = 'p')
  points(wt, aaflight1[s,2,], col = "blue", pch = 19)
  
  
  a <- lm(aaflight1[s,1,]~wt)
  b <- lm(aaflight1[s,2,]~wt)
  
  abline(coef(a), col = "red")
  abline(coef(b), col = "blue")
  
  
}

#spt------------------------------------

for(s in 56){
  
  plot(c(0,120), c(round_any(min(aaflight1[s,1,], na.rm = T), 10, floor),round_any(max(aaflight1[s,2,], na.rm = T), 10, ceiling)), type = 'n',  xlab = "Spring Threshold Day", ylab = "Days")
  
  points(spt, aaflight1[s,1,], col = "red", pch = 19, type = 'p')
  points(spt, aaflight1[s,2,], col = "blue", pch = 19)
  
  
  a <- lm(aaflight1[s,1,]~spt)
  b <- lm(aaflight1[s,2,]~spt)
  
  abline(coef(a), col = "red")
  abline(coef(b), col = "blue")
  
  
}


#st

s <- 56

plot(c(11,15), c(round_any(min(aaflight1[s,1,], na.rm = T), 10, floor),round_any(max(aaflight1[s,2,], na.rm = T), 10, ceiling)), type = 'n',  xlab = "Season of Flight Temperature (°C)", ylab = "Days")



points(st, aaflight1[s,1,], col = "red", pch = 19, type = 'p')
points(st, aaflight1[s,2,], col = "blue", pch = 19)


a <- lm(aaflight1[s,1,]~st)
b <- lm(aaflight1[s,2,]~st)

abline(coef(a), col = "red")
abline(coef(b), col = "blue")

dev.off()

