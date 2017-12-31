
pdf("Example Species.pdf", width= 14, height= 10)

s <- 13 #Silver ground carpet


  par(mfrow=c(2,2))
  
mar = c(0,0,0,0)

  plot(years1, log10(as.numeric(msummary1[s,4:28])), xlab = "Year", ylab = "Log10(Abundance)", col = 'green', pch = 15)
  abline(coef(lm(log10(as.numeric(msummary1[s,4:28]))~years1)), col = 'green')


mar = c(0,0,0,0)
  
  plot(c(8.5,10.6), c(round_any(min(as.numeric(flight1[s,4,]), na.rm = T), 10, floor),round_any(max(as.numeric(flight1[s,5,]), na.rm = T), 10, ceiling)), type = 'n', xlab = "Year Temperature", ylab = "Day")
  
  points(mtemp[31:55,1], as.numeric(flight1[s,4,]), pch = 18, col = alpha("blue", 0.6))
  points(mtemp[31:55,1], as.numeric(flight1[s,5,]),  col = alpha("blue", 0.6), pch = 5)
  
  abline(coef(lm(as.numeric(flight1[s,4,])~mtemp[31:55,1])), col = "blue")
  abline(coef(lm(as.numeric(flight1[s,5,])~mtemp[31:55,1])), col = "blue")


  points(mtemp[31:55,1], aaflight1[s,1,], pch = 16, col = alpha("red", 0.6))
  points(mtemp[31:55,1], aaflight1[s,2,],  col = alpha("red", 0.6), pch = 1)
  
  abline(coef(lm(aaflight1[s,1,]~mtemp[31:55,1])), col = "red", lty = 2)
  abline(coef(lm(aaflight1[s,2,]~mtemp[31:55,1])), col = "red", , lty = 2)

  
mar = c(0,0,0,0)

  
  plot(c(range(cons[31:55])), c(round_any(min(as.numeric(flight1[s,4,]), na.rm = T), 10, floor),round_any(max(as.numeric(flight1[s,5,]), na.rm = T), 10, ceiling)), type = 'n', xlab = "Spring Threshold Day", ylab = "Day")
  
  points(cons[31:55], as.numeric(flight1[s,4,]), pch = 18, col = alpha("blue", 0.6))
  points(cons[31:55], as.numeric(flight1[s,5,]),  col = alpha("blue", 0.6), pch = 5)
  
  abline(coef(lm(as.numeric(flight1[s,4,])~cons[31:55])), col = "blue")
  abline(coef(lm(as.numeric(flight1[s,5,])~cons[31:55])), col = "blue")
  
  
  points(cons[31:55], aaflight1[s,1,], pch = 16, col = alpha("red", 0.6))
  points(cons[31:55], aaflight1[s,2,],  col = alpha("red", 0.6), pch = 1)
  
  abline(coef(lm(aaflight1[s,1,]~cons[31:55])), col = "red", lty = 2)
  abline(coef(lm(aaflight1[s,2,]~cons[31:55])), col = "red", lty = 2)


mar = c(0,0,0,0)

  
  plot(c(range(mtemp[30:54,2])), c(round_any(min(as.numeric(flight1[s,4,]), na.rm = T), 10, floor),round_any(max(as.numeric(flight1[s,5,]), na.rm = T), 10, ceiling)), type = 'n', xlab = "Previous Winter Temperature", ylab = "Day")
  
  points(mtemp[30:54,2], as.numeric(flight1[s,4,]), pch = 18, col = alpha("blue", 0.6))
  points(mtemp[30:54,2], as.numeric(flight1[s,5,]),  col = alpha("blue", 0.6), pch = 5)
  
  abline(coef(lm(as.numeric(flight1[s,4,])~mtemp[30:54,2])), col = "blue")
  abline(coef(lm(as.numeric(flight1[s,5,])~mtemp[30:54,2])), col = "blue")
  
  
  points(mtemp[30:54,2], aaflight1[s,1,], pch = 16, col = alpha("red", 0.6))
  points(mtemp[30:54,2], aaflight1[s,2,],  col = alpha("red", 0.6), pch = 1)
  
  abline(coef(lm(aaflight1[s,1,]~mtemp[30:54,2])), col = "red", lty = 2)
  abline(coef(lm(aaflight1[s,2,]~mtemp[30:54,2])), col = "red", lty = 2)

dev.off()


