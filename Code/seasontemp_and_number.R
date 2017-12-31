t <- 1:55
stemp <- data.frame(t)



stemp$ESp <- colMeans(dtemp[60:106,])
stemp$Spr <- colMeans(dtemp[60:151,])
stemp$LSpr <- colMeans(dtemp[107:151,])
stemp$SprSum <- colMeans(dtemp[60:243,])
stemp$ESum <- colMeans(dtemp[152:198,])
stemp$Sum <- colMeans(dtemp[152:243,])
stemp$LSum <- colMeans(dtemp[199:243,])
stemp$SumAut <- colMeans(dtemp[152:334,])
stemp$EAut <- colMeans(dtemp[244:289,])
stemp$Aut <- colMeans(dtemp[244:334,])
stemp$LAut <- colMeans(dtemp[290:334,])
stemp$AutWin <- colMeans(dtemp[244:365,])
stemp$EWin <- colMeans(dtemp[335:365,])
stemp$Win <- colMeans(dtemp[335:365,])
stemp$LWin <- colMeans(dtemp[1:59,])
stemp$WinSpr <- colMeans(dtemp[1:151,])
stemp$none <- colMeans(dtemp[1:365,])

stemp <- stemp[,-1]
stemp


snum <- vector(length = 110, mode = "numeric")

snum[which(luna$season == "early_spring")] <- 1
snum[which(luna$season == "spring")] <- 2
snum[which(luna$season == "late_spring")] <- 3
snum[which(luna$season == "spring/summer")] <- 4
snum[which(luna$season == "early_summer")] <- 5
snum[which(luna$season == "summer")] <- 6
snum[which(luna$season == "late_summer")] <- 7
snum[which(luna$season == "summer/autumn")] <- 8
snum[which(luna$season == "early_autumn")] <- 9
snum[which(luna$season == "autumn")] <- 10
snum[which(luna$season == "late_autumn")] <- 11
snum[which(luna$season == "autumn/winter")] <- 12
snum[which(luna$season == "early_winter")] <- 13
snum[which(luna$season == "winter")] <- 14
snum[which(luna$season == "late_winter")] <- 15
snum[which(luna$season == "winter/spring")] <- 16
snum[which(luna$season == "none")] <- 17

snum
