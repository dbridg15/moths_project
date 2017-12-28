in2cm <- function(x) return(x * 2.54)
cm2in <- function(x) return(x * 0.393700787)


#Season classifications---------------------------

pdf("Seasonsclass.pdf")

par(fig = c(0.1,1,0,1))

plot(c(0,400),c(0,17),type='n', yaxt='n', bty = "n", xlab = "Days", xaxt = 'n', ylab = "")
axis(1, at = c(0,ron[,3]))
axis(2, at = 1:17, labels = c("Early Spring", "Spring", "Late Spring", "Spring/Summer", "Early Summer", " Summer", "Late Summer", "Summer/Autumn", "Early Autumn", " Autumn", " Late Autumn", " Autumn/Winter", "Early Winter", " Winter", "Late Winter", "Winter/Spring", "None"), las = 2) 


for(i in 1:11){
  segments(ron[i,2], ron[i,5], ron[i,3], ron[i,5], col = as.character(ron[i,4]), lwd = 15)
}


for(i in 13:16){
  segments(ron[i,2], ron[i,5], ron[i,3], ron[i,5], col = as.character(ron[i,4]), lwd = 15)
}

segments(ron[12,2],ron[12,5],365,ron[12,5], col = as.character(ron[12,4]), lwd = 15)
segments(0,ron[12,5],ron[12,3],ron[12,5], col = as.character(ron[12,4]), lwd = 15)
segments(0,17,365,17, col = "grey", lwd = 15)

dev.off()


#example species graphs----------------------------------------


pdf("Pale Mottled Willow.pdf")

for(s in 1:110){
  
  plot(c(8.5,11), c(round_any(min(aaflight1[s,1,], na.rm = T), 10, floor),round_any(max(aaflight1[s,2,], na.rm = T), 10, ceiling)), type = 'n', main = luna$name[s])

points(mtemp[31:55,1], aaflight1[s,1,], col = "red", pch = 19, type = 'p')
points(mtemp[31:55,1], aaflight1[s,2,], col = "blue", pch = 19)

abline(luna$FFDintercept[s], luna$FFDslope[s], col = "red")
abline(luna$LFDintercept[s], luna$LFDslope[s], col = "blue")

for(yr in 1:25){
  segments(mtemp[30+yr,1],aaflight1[s,1,yr],mtemp[30+yr,1],aaflight1[s,2,yr])
}
}

dev.off()

#significant and insignificant---------------------------------------


B <-  hist(luna$FFDslope, breaks = 35)
Bs <- hist(luna$FFDslope[which(luna$FFDsig < 0.05)], xlim = c(-25,10), breaks = 24)

A <-  hist(luna$LFDslope, breaks = 35)
As <- hist(luna$LFDslope[which(luna$LFDsig < 0.05)] , xlim = c(-15,20), breaks = 30)

pdf("significant.pdf")


par(fig=c(0,0.8,0,0.8))
plot(luna$FFDslope, luna$LFDslope, xlim= c(-25,10), ylim = c(-15,20))
abline(v=0,h=0)

par(fig=c(0,0.8,0.55,1), new=TRUE)
plot(NULL, type = "n", ylim = c(0,max(B$counts)), xlim = c(-25,10) , xaxt='n', ann=FALSE, bty = "n")
rect(B$breaks[1:(length(B$breaks) - 1)], 0, B$breaks[2:length(B$breaks)], B$counts, col = "grey50", border = NA)
rect(Bs$breaks[1:(length(Bs$breaks) - 1)], 0, Bs$breaks[2:length(Bs$breaks)], Bs$counts, col = "grey10", border = NA)

par(fig=c(0.65,1,0,0.8), new=TRUE)
plot(NULL, type = "n", xlim = c(0, max(A$counts)), ylim = c(-15,20), yaxt='n', ann=FALSE, bty = "n")
rect(0, A$breaks[1:(length(A$breaks) - 1)], A$counts, A$breaks[2:length(A$breaks)],  col = "grey50", border = NA)
rect(0, As$breaks[1:(length(As$breaks) - 1)], As$counts, As$breaks[2:length(As$breaks)],  col = "grey10", border = NA) 


dev.off()

#Seasons-------------------------------------------

seasons <- c("autumn/winter", "late_autumn", "autumn", "early_autumn", "summer/autumn", "late_summer", "summer", "early_summer", "spring/summer", "late_spring", "spring", "early_spring", "late_winter", "winter", "early_winter", "winter/spring" ,"none")

pdf("Seasons.pdf")


par(fig=c(0,0.8,0,0.8))

plot(luna$FFDslope, luna$LFDslope, xlim= c(-25,10), ylim = c(-16,20), col = as.character(luna$col), pch = luna$pch, ylab = "Last Flight Day Slope", xlab = "First Flight Day Slope")
abline(v=0,h=0)
abline(0,1)

par(fig=c(0,0.8,0.55,1), new=TRUE)

ffdbins <- -25:9

ffdresponse <- matrix(nrow=35, ncol=17)

colnames(ffdresponse) <- seasons
rownames(ffdresponse) <- ffdbins



for(s in 1:17){
  
  for(i in -25:9){
    
    a <- which(luna$FFDslope[which(luna$season == seasons[s])] >= i)
    b <- which(luna$FFDslope[which(luna$season == seasons[s])] < (i+1))
    
    ffdresponse[i+26,s] <- length(intersect(a,b))
  }
}

rm(a,b)

temp <- hist(luna$FFDslope, breaks = 35, plot = F)

plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-25,10) , xaxt='n', ann=FALSE, bty = "n")
rect(ffdbins, 0, ffdbins +1, ffdresponse[,1], col = colfunc(16)[12], border = NA)
rect(ffdbins, ffdresponse[,1], ffdbins +1, rowSums(ffdresponse[,1:2]), col = colfunc(16)[11], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:2]), ffdbins +1, rowSums(ffdresponse[,1:3]), col = colfunc(16)[10], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:3]), ffdbins +1, rowSums(ffdresponse[,1:4]), col = colfunc(16)[9], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:4]), ffdbins +1, rowSums(ffdresponse[,1:5]), col = colfunc(16)[8], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:5]), ffdbins +1, rowSums(ffdresponse[,1:6]), col = colfunc(16)[7], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:6]), ffdbins +1, rowSums(ffdresponse[,1:7]), col = colfunc(16)[6], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:7]), ffdbins +1, rowSums(ffdresponse[,1:8]), col = colfunc(16)[5], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:8]), ffdbins +1, rowSums(ffdresponse[,1:9]), col = colfunc(16)[4], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:9]), ffdbins +1, rowSums(ffdresponse[,1:10]), col = colfunc(16)[3], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:10]), ffdbins +1, rowSums(ffdresponse[,1:11]), col = colfunc(16)[2], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:11]), ffdbins +1, rowSums(ffdresponse[,1:12]), col = colfunc(16)[1], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:12]), ffdbins +1, rowSums(ffdresponse[,1:13]), col = colfunc(16)[16], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:13]), ffdbins +1, rowSums(ffdresponse[,1:14]), col = colfunc(16)[15], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:14]), ffdbins +1, rowSums(ffdresponse[,1:15]), col = colfunc(16)[14], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:15]), ffdbins +1, rowSums(ffdresponse[,1:16]), col = colfunc(16)[13], border = NA)
rect(ffdbins, rowSums(ffdresponse[,1:16]), ffdbins +1, rowSums(ffdresponse[,1:17]), col = "grey", border = NA)

par(fig=c(0.65,1,0,0.8), new=TRUE)

lfdbins <- -16:19

lfdresponse <- matrix(nrow=36, ncol=17)

colnames(lfdresponse) <- seasons
rownames(lfdresponse) <- lfdbins



for(s in 1:17){
  
  for(i in -16:19){
    
    a <- which(luna$LFDslope[which(luna$season == seasons[s])] >= i)
    b <- which(luna$LFDslope[which(luna$season == seasons[s])] < (i+1))
    
    lfdresponse[i+17,s] <- length(intersect(a,b))
  }
}

rm(a,b)

temp <- hist(luna$LFDslope, breaks = 36, plot = F)


plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-16,20), yaxt='n', ann=FALSE, bty = "n")
rect(0, lfdbins, lfdresponse[,1], lfdbins +1, col = colfunc(16)[12], border = NA)
rect(lfdresponse[,1], lfdbins, rowSums(lfdresponse[,1:2]), lfdbins +1,  col = colfunc(16)[11], border = NA)
rect(rowSums(lfdresponse[,1:2]), lfdbins, rowSums(lfdresponse[,1:3]), lfdbins +1,  col = colfunc(16)[10], border = NA)
rect(rowSums(lfdresponse[,1:3]), lfdbins, rowSums(lfdresponse[,1:4]), lfdbins +1,  col = colfunc(16)[9], border = NA)
rect(rowSums(lfdresponse[,1:4]), lfdbins, rowSums(lfdresponse[,1:5]), lfdbins +1,  col = colfunc(16)[8], border = NA)
rect(rowSums(lfdresponse[,1:5]), lfdbins, rowSums(lfdresponse[,1:6]), lfdbins +1,  col = colfunc(16)[7], border = NA)
rect(rowSums(lfdresponse[,1:6]), lfdbins, rowSums(lfdresponse[,1:7]), lfdbins +1,  col = colfunc(16)[6], border = NA)
rect(rowSums(lfdresponse[,1:7]), lfdbins, rowSums(lfdresponse[,1:8]), lfdbins +1,  col = colfunc(16)[5], border = NA)
rect(rowSums(lfdresponse[,1:8]), lfdbins, rowSums(lfdresponse[,1:9]), lfdbins +1,  col = colfunc(16)[4], border = NA)
rect(rowSums(lfdresponse[,1:9]), lfdbins, rowSums(lfdresponse[,1:10]), lfdbins +1,  col = colfunc(16)[3], border = NA)
rect(rowSums(lfdresponse[,1:10]), lfdbins, rowSums(lfdresponse[,1:11]), lfdbins +1,  col = colfunc(16)[2], border = NA)
rect(rowSums(lfdresponse[,1:11]), lfdbins, rowSums(lfdresponse[,1:12]), lfdbins +1,  col = colfunc(16)[1], border = NA)
rect(rowSums(lfdresponse[,1:12]), lfdbins, rowSums(lfdresponse[,1:13]), lfdbins +1,  col = colfunc(16)[16], border = NA)
rect(rowSums(lfdresponse[,1:13]), lfdbins, rowSums(lfdresponse[,1:14]), lfdbins +1,  col = colfunc(16)[15], border = NA)
rect(rowSums(lfdresponse[,1:14]), lfdbins, rowSums(lfdresponse[,1:15]), lfdbins +1,  col = colfunc(16)[14], border = NA)
rect(rowSums(lfdresponse[,1:15]), lfdbins, rowSums(lfdresponse[,1:16]), lfdbins +1,  col = colfunc(16)[13], border = NA)
rect(rowSums(lfdresponse[,1:16]), lfdbins, rowSums(lfdresponse[,1:17]), lfdbins +1,  col = "grey", border = NA)

par(fig=c(0.65,1,0.55,1), new=TRUE)

plot(NULL, type = "n", ylim = c(-4.5,4.5), xlim = c(-4.5,4.5) , xaxt='n', ann=FALSE,  yaxt='n')
abline(h=0,v=0, lwd = 2)
segments(-4,3,-1,4)
segments(-4,2,-1,1)
segments(-4,-1,-1,-2)
segments(-4,-3,-1,-4)
segments(1,3,4,4)
segments(1,1,4,2)
segments(1,-1,4,-2)
segments(1,-4,4,-3)

dev.off()


#model predictions ------------------------------------------------

pdf("FFDmodel.pdf")

plot(c(7,13),c(50,300),type='n', main = "First flight day")


for (s in 1:110){
  lines(temps, fixef(ffdmodel)[1] + ffdrandeff$id[s,1] + ffdrandeff$id[s,2]*temps, type='l', col = alpha(col=color[s], 0.8)) #average year 
}

dev.off()

pdf("LFDmodel.pdf")

plot(c(7,13),c(50,300),type='n', main = "last flight day")


for (s in 1:110){
  lines(temps, fixef(lfdmodel)[1] + lfdrandeff$id[s,1] + lfdrandeff$id[s,2]*temps, type='l', col = alpha(col=color[s], 0.8)) #average year
}

dev.off()


pdf("modelresponses.pdf")

par(fig=c(0,0.8,0,0.8))

plot(ffdmr,lfdmr, xlim = c(-4.5,4), ylim = c(-5,5), xlab = "First Flight Day Slope", ylab = "Last Flight Day Slope",  col = as.character(luna$col))
abline(h=0,v=0)

par(fig=c(0,0.8,0.55,1), new=TRUE)

temp <- hist(ffdmr, breaks = 18, plot = F)

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

par(fig=c(0.65,1,0,0.8), new=TRUE)

temp <- hist(lfdmr, breaks = 18, plot = F)

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

dev.off()






#CLIMATE GRAPHS--------------------------------------------------------

pdf("Climate.pdf", width = 8, height = 11)

years <- 1960:2014
years1 <- 1990:2014

# par(fig=c(0,1,0.4,1))
par(mfrow=c(2,1))

yearmod <- lm(mtemp[,1]~years)
plot(years, mtemp[,1], ylab="Mean Daily Temperature (°C)", xlab="Year", col=alpha(col="blue",alpha=0.7), pch=16)
segments(1960, fitted(yearmod)[1], 2014, fitted(yearmod)[55], col="blue", lwd=1)
nyt <- lm(mtemp[31:55,1]~years1)
segments(1990, fitted(nyt)[1], 2014, fitted(nyt)[25], col="red", lwd=1.5) 

# par(fig=c(0,0.5,0,0.48), new=TRUE)
# 
# wintermod <- lm(mtemp[,2]~years)
# plot(years, mtemp[,2], ylab="Mean Daily Temperature (°C)", xlab="Year",col=alpha(col="blue",alpha=0.7), pch=16)
# segments(1960, fitted(wintermod)[1], 2014, fitted(wintermod)[55], col="blue", lwd=1)

# par(fig=c(0.5,1,0,0.48), new=TRUE)

consmod <- lm(cons~years)
plot(years, cons, xlab="Year", ylab="Spring Threshold Day",col=alpha(col="blue",alpha=0.7), pch=16)
segments(1960, fitted(consmod)[1], 2014, fitted(consmod)[55], col="blue", lwd=1)

dev.off()

#Adjusting for abundance----------------------------------------------

svg("AA.svg", width = 7.5, height = 8.5)


id <- 56

a <- log10(as.numeric(msummary1[8,4:28]))
b <- 1990:2014

a[is.infinite(a)] <- NA

par(fig=c(0,1,0.4,1))

plot(b,a, xlab="Year", ylab="log(Abundance)", col=alpha(col="green",0.7), pch=16)
abline(coef(lm(a~b)),col="green")

par(fig=c(0,0.5,0,0.48), new=TRUE)


plot(years1, flight1[id,4,], col = "blue", pch = 16, xlab = "Year", ylab = "Day of First Emergence")
mod <- lm(flight1[id,4,]~years1)
abline(coef(mod), col = "blue")

points(years1, aaflight1[id,1,], col = "red", pch = 18)
aamod <- lm(aaflight1[id,1,]~years1)
abline(coef(aamod), col = "red")

par(fig=c(0.5,1,0,0.48), new=TRUE)

plot(years1, flight1[id,5,], col = "blue", pch = 16, ylim= c(240,310), xlab = "Year", ylab = "Last Flight Day")
mod <- lm(flight1[id,5,]~years1)
abline(coef(mod), col = "blue")

points(years1, aaflight1[id,2,], col = "red", pch = 18)
aamod <- lm(aaflight1[id,2,]~years1)
abline(coef(aamod), col = "red")

dev.off()

