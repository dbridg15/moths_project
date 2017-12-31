luna 
ron 
seasons <- c("autumn/winter", "late_autumn", "autumn", "early_autumn", "summer/autumn", "late_summer", "summer", "early_summer", "spring/summer", "late_spring", "spring", "early_spring", "late_winter", "winter", "early_winter", "winter/spring" ,"none")
colfunc <- colorRampPalette(c("green", "yellow", "red", "blue"))


#FFD season hist-----------------------------------------------------------

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

temp <- hist(luna$FFDslope, breaks = 35, xlim = c(-25,10))#, plot = F)

plot(NULL, type = "n", ylim = c(0,max(temp$counts)), xlim = c(-25,10))# , xaxt='n', ann=FALSE, bty = "n")
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


#LFDseasonhist

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

temp <- hist(luna$LFDslope, breaks = 36, xlim = c(-16,20))#, plot = F)

plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-16,20))#, yaxt='n', ann=FALSE, bty = "n")
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




#Actual plot!!!!!!!!--------------------------

par(fig=c(0,0.8,0,0.8))

  plot(luna$FFDslope, luna$LFDslope, xlim= c(-25,10), ylim = c(-15,20))
  abline(v=0,h=0)

par(fig=c(0,0.8,0.5,1), new=TRUE)

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

par(fig=c(0.63,1,0,0.8), new=TRUE)

temp <- hist(luna$LFDslope, breaks = 35, plot = F)

plot(NULL, type = "n", xlim = c(0, max(temp$counts)), ylim = c(-15,20), yaxt='n', ann=FALSE, bty = "n")
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
    
    
    

