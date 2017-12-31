spring <- vector(length = 35)
summer <- vector(length = 35)
autumn <- vector(length = 35)
na <- vector(length = 35)



#FFD Season hist-----------------------------------------------------------------------      
    
for(i in -25:9){
  
  a <- which(luna$FFDslope[which(luna$season == "Spring")] >= i)
  b <- which(luna$FFDslope[which(luna$season == "Spring")] < (i+1))
  
  spring[i+26] <- length(intersect(a,b))
  
}


for(i in -25:9){
 
  a <- which(luna$FFDslope[which(luna$season == "Summer")] >= i)
  b <- which(luna$FFDslope[which(luna$season == "Summer")] < (i+1))

  summer[i+26] <- length(intersect(a,b))
  
}

for(i in -25:9){
  
  a <- which(luna$FFDslope[which(luna$season == "Autumn")] >= i)
  b <- which(luna$FFDslope[which(luna$season == "Autumn")] < (i+1))
  
  autumn[i+26] <- length(intersect(a,b))
  
}

for(i in -25:9){
  
  a <- which(luna$FFDslope[which(is.na(luna$season))] >= i)
  b <- which(luna$FFDslope[which(is.na(luna$season))] < (i+1))
  
  na[i+26] <- length(intersect(a,b))
  
}

bins <- -25:9

ffdseasonhist <- data.frame( bins, spring, summer, autumn, na)

ffdseasonhist


temp <- hist(luna$FFDslope, breaks = 35, plot = F)

plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-25,10) , xaxt='n', ann=FALSE, bty = "n")
  rect(ffdseasonhist$bins, 0, ffdseasonhist$bins +1, ffdseasonhist$autumn, col = "orange", border = NA)
  rect(ffdseasonhist$bins, ffdseasonhist$autumn, ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring, col = "green", border = NA)
  rect(ffdseasonhist$bins, ffdseasonhist$autumn + ffdseasonhist$spring, ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer, col = "yellow", border = NA)
  rect(ffdseasonhist$bins, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer , ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer + ffdseasonhist$na, col = "grey", border = NA)


#LFD Season hist-----------------------------------------------------------------------      

for(i in -15:19){
  
  a <- which(luna$LFDslope[which(luna$season == "Spring")] >= i)
  b <- which(luna$LFDslope[which(luna$season == "Spring")] < (i+1))
  
  spring[i+16] <- length(intersect(a,b))
  
}


for(i in -15:19){
  
  a <- which(luna$LFDslope[which(luna$season == "Summer")] >= i)
  b <- which(luna$LFDslope[which(luna$season == "Summer")] < (i+1))
  
  summer[i+16] <- length(intersect(a,b))
  
}

for(i in -15:19){
  
  a <- which(luna$LFDslope[which(luna$season == "Autumn")] >= i)
  b <- which(luna$LFDslope[which(luna$season == "Autumn")] < (i+1))
  
  autumn[i+16] <- length(intersect(a,b))
  
}

for(i in -15:19){
  
  a <- which(luna$LFDslope[which(is.na(luna$season))] >= i)
  b <- which(luna$LFDslope[which(is.na(luna$season))] < (i+1))
  
  na[i+16] <- length(intersect(a,b))
  
}

bins <- -15:19

lfdseasonhist <- data.frame( bins, spring, summer, autumn, na)

lfdseasonhist

temp <- hist(luna$LFDslope, breaks = 35, plot = F)

plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-15,20), yaxt='n', ann=FALSE, bty = "n")
  rect(0, lfdseasonhist$bins, lfdseasonhist$autumn, lfdseasonhist$bins +1,  col = "orange", border = NA)
  rect(lfdseasonhist$autumn, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring, lfdseasonhist$bins +1,  col = "green", border = NA)
  rect(lfdseasonhist$autumn + lfdseasonhist$spring, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer, lfdseasonhist$bins +1,  col = "yellow", border = NA)
  rect(lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer + lfdseasonhist$na, lfdseasonhist$bins +1,  col = "grey", border = NA)


#Actual plot!!!!!----------------------------------

pdf("C:/Users/David/OneDrive/Moths/test1.pdf")


par(fig=c(0,0.8,0,0.8))

  plot(luna$FFDslope, luna$LFDslope, xlim= c(-25,10), ylim = c(-15,20))
  abline(v=0,h=0)

par(fig=c(0,0.8,0.56,1), new=TRUE)
  
  temp <- hist(luna$FFDslope, breaks = 35, plot = F)

  plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-25,10) , xaxt='n', ann=FALSE, bty = "n")
    rect(ffdseasonhist$bins, 0, ffdseasonhist$bins +1, ffdseasonhist$autumn, col = "orange", border = NA)
    rect(ffdseasonhist$bins, ffdseasonhist$autumn, ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring, col = "green", border = NA)
    rect(ffdseasonhist$bins, ffdseasonhist$autumn + ffdseasonhist$spring, ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer, col = "yellow", border = NA)
    rect(ffdseasonhist$bins, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer , ffdseasonhist$bins +1, ffdseasonhist$autumn + ffdseasonhist$spring + ffdseasonhist$summer + ffdseasonhist$na, col = "grey", border = NA)


par(fig=c(0.63,1,0,0.8), new=TRUE)

  temp <- hist(luna$LFDslope, breaks = 35, plot = F)

  plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-15,20), yaxt='n', ann=FALSE, bty = "n")
  rect(0, lfdseasonhist$bins, lfdseasonhist$autumn, lfdseasonhist$bins +1,  col = "orange", border = NA)
  rect(lfdseasonhist$autumn, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring, lfdseasonhist$bins +1,  col = "green", border = NA)
  rect(lfdseasonhist$autumn + lfdseasonhist$spring, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer, lfdseasonhist$bins +1,  col = "yellow", border = NA)
  rect(lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer, lfdseasonhist$bins, lfdseasonhist$autumn + lfdseasonhist$spring + lfdseasonhist$summer + lfdseasonhist$na, lfdseasonhist$bins +1,  col = "grey", border = NA)


dev.off()
