color <- as.character(luna$season)

color[which(color == "Summer")] <- "yellow"
color[which(color == "Spring")] <- "green"
color[which(color == "Autumn")] <- "orange"
color[which(is.na(color))] <- "grey"

color


FFD <- vector(length = 2750)
LFD <- vector(length = 2750)
FP <- vector(length = 2750)
FPos <- vector(length = 2750)
year <- vector(length = 2750)
temp <- vector(length = 2750)

for(yr in 1:25){
  id[((yr*110)-109):(yr*110)] <- as.character(luna$id)
  season[((yr*110)-109):(yr*110)] <- as.character(luna$season)
  year[((yr*110)-109):(yr*110)] <- yr+1989
  temp[((yr*110)-109):(yr*110)] <- mtemp[30 +yr,1]
  FFD[((yr*110)-109):(yr*110)] <- aaflight1[,1,yr]
  LFD[((yr*110)-109):(yr*110)] <- aaflight1[,2,yr]
  FP[((yr*110)-109):(yr*110)] <- aaflight1[,3,yr]
  FPos[((yr*110)-109):(yr*110)] <- aaflight1[,4,yr]
  
}


dat <- data.frame(id, season, year, temp, FFD, LFD, FP, FPos)



plot(dat$temp, dat$FFD)
abline(coef(lm(FFD~temp, data=dat)))

model <- lmer(FFD ~ (1|year) + (temp|id), data=dat)
summary(model)

fixef(model)
randeff <- ranef(model)

plot(randeff$id[,1], randeff$id[,2])
randeff$year


temps <- 5:15

plot(c(5,15),c(0,300),type='n')

for (s in 1:110){
  #lines(temps, fixef(model)[1] + randeff$Species[s,1] + randeff$Species[s,2]*temps,type='l')#average year
  lines(temps, fixef(model)[1] + randeff$id[s,1] + randeff$id[s,2]*temps + randeff$year[22,1],type='l', col = color[s])
}


