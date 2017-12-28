
fpt <- aaflight1[,1:3,1]
fpt <- as.data.frame(fpt)
fpt[,4:7] <- NA
fpt[,8] <- class[,3]
fpt[,9] <- NA
fpt[,10] <- "black"
fpt[,11] <- "black"

#row.names(fp) <- ("id" "BF" "Name" "FFD Slope" "FFD Sig" "LFD Slope" "LFD Sig" "Season" "season col"  "ffdcol" "lfdcol"")


fpt[which(fpt[,8] == "Winter/Spring"),9] <- 15
fpt[which(fpt[,8] == "Spring"),9] <- 15
fpt[which(fpt[,8] == "Spring/Summer"),9] <- 15
fpt[which(fpt[,8] == "Early_Summer"),9] <- 15

fpt[which(fpt[,8] == "Summer"),9] <- 19
fpt[which(is.na(fpt[,8])),9] <- 19

fpt[which(fpt[,8] == "Late_Summer"),9] <- 17
fpt[which(fpt[,8] == "Summer/Autumn"),9] <- 17
fpt[which(fpt[,8] == "Autumn"),9] <- 17
fpt[which(fpt[,8] == "Autumn/Winter"),9] <- 17


for(id in 1:110){
  
  plot(1, type="n", xlab="", ylab="", xlim=c(8.5,11), ylim=c(0, 365), main = flight1[id,3,1])
  
  f <- as.numeric(flight1[id,4,])
  l <- as.numeric(flight1[id,5,])
  t <- mtemp[31:55,1]
  
  
  fpt[id,4] <- coef(lm(f~t))[2]
  fpt[id,5] <- anova(lm(f~t))$'Pr(>F)'[1]
  fpt[id,6] <- coef(lm(l~t))[2]
  fpt[id,7] <- anova(lm(l~t))$'Pr(>F)'[1]
  
  if(fpt[id,5] < 0.05){
    fpt[id,10] <- "red"
  }
  
  if(fpt[id,7] < 0.05){
    fpt[id,11] <- "red"
  }
  
  abline(coef(lm(f~t)), col = fpt[id,10])
  abline(coef(lm(l~t)), col = fpt[id,11])
  
  for(temp in t){
    segments(y0 = as.numeric(flight1[id,4,which(mtemp[,1] == temp) -30]), x0 =temp, y1= as.numeric(flight1[id,5,which(mtemp[,1] == temp) -30]))
  }
  
  
}


plot(fpt[,4], fpt[,6], pch = fpt[,9], cex = 0.7)
abline(v=0)
abline(h=0)

sigFFD <- which(fpt[,5] <= 0.05)
sigLFD <- which(fpt[,7] <= 0.05)

t <-sigLFD[match(sigFFD, sigLFD)]
sigboth <- t[which(is.na(t) == F)]


sigFFD <- sigFFD[which(is.na(match(sigFFD, sigboth)) == T)]
sigLFD <- sigLFD[which(is.na(match(sigLFD, sigboth)) == T)]

sigFFD
sigLFD
sigboth

sigall <- c(sigFFD, sigLFD, sigboth)
t<- 1:110
sigall
which


plot(100,xlim = c(-30,30), ylim = c(-30,30), xlab = "FFD Slope", ylab = "LFD Slope")
abline(v=0)
abline(h=0)
points(fpt[sigFFD,4], fpt[sigFFD,6], col = "red", pch = fpt[,9], cex = 0.7)
points(fpt[sigLFD,4], fpt[sigLFD,6], col = "blue", pch = fpt[,9], cex = 0.7)
points(fpt[sigboth,4], fpt[sigboth,6], col = "green", pch = fpt[,9], cex = 0.7)


e <- which(fpt[,9] == 15)
m <- which(fpt[,9] == 19)
l <- which(fpt[,9] == 17)


plot(100,xlim = c(-30,30), ylim = c(-30,30), xlab = "FFD Slope", ylab = "LFD Slope")
abline(v=0)
abline(h=0)

points(fpt[e,4], fpt[e,6], col = "yellow", pch = fpt[,9], cex = 0.7)
points(fpt[m,4], fpt[m,6], col = "red", pch = fpt[,9], cex = 0.7)
points(fpt[l,4], fpt[l,6], col = "blue", pch = fpt[,9], cex = 0.7)


#Flight legnth against temperature ----------------------------------------

temp <- vector(length=110)

for(id in 1:110){
  plot(mtemp[31:55,1],aaflight1[id,3,])
  fl <- lm(aaflight1[id,3,]~mtemp[31:55,1])
  abline(coef(fl))
  temp[id] <- as.numeric(coef(fl)[2])
}

temp

hist(temp)

?hist
