#Pretty Graphs---------------------------------
colfunc <- colorRampPalette(c("red", "yellow"))


toby <- rep( list(list()), 110 )

for(id in 1:110){ #Overall
  
  temp <- as.numeric(moths2[id,4:368])
  temp[is.na(temp)] <- 0
  temp1 <- 1
  
  for(day in 1:365){
    temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
  }
  
  Overall <- temp1[2:length(temp1)]
  
  ron <- rep( list(list()), 25 )
  
  for(yr in 1:25){
    temp <- as.numeric(moths1[id,4:368,yr])
    temp[is.na(temp)] <- 0
    temp1 <- 1
    
    for(day in 1:365){
      temp1 <- c(temp1, seq(from = day, by=0, length.out=temp[day]))
    }
    
    temp1 <- temp1[2:length(temp1)]
    
    ron[[yr]] <- vector(mode='numeric',length=length(temp1))
    ron[[yr]] <- temp1
    
  }
  
  test4 <- vector(length=25)
  for(yr in 1:25){
    if(msummary[which(msummary[,1] == moths1[id,1,1]),yr+3] > 2){
      test4[yr] <-  max(density(ron[[yr]])$y)
    }
  }
#   
#   plot(1,1,xlim=c(((which(moths2[id,4:368] > 0)[1])-7),(tail(which(moths2[id,4:368] > 0),n='1')+7)),ylim=c(0,round_any(max(test4),0.1,ceiling)),main= moths2[id,3])
#   abline(v=c(31,59,90,120,151,181,212,243,273,304,334,365),col=alpha("black",0.5))
#   
#   for (yr in 1:25){
#     if(msummary[which(msummary[,1] == moths1[id,1,1]),yr+3] > 2){
#       lines(density(ron[[yr]]),col=alpha(colfunc(25)[yr],0.5))}
#   }
#   lines(density(Overall),col="blue")
  
  toby[[id]] <- Overall
  
}

# plot(1,1,xlim=c(1,365),ylim=c(0,0.1),xaxs="i",yaxs="i")
# abline(v=c(31,59,90,120,151,181,212,243,273,304,334,365),col=alpha("black",0.5))
# abline(v=c(59,151,243,334),col=alpha("black",0.5),lwd=2)
# 
# for (id in 1:110){
#   lines(density(as.numeric(toby[[id]])),col=alpha("blue",0.5))
#   
# }

toby

#Classifications------------------------------------------------


# colfunc <- colorRampPalette(c("green", "yellow", "red", "blue"))

seas_list <- c("early_spring","late_spring","early_summer","late_summer",
               "early_autumn","late_autumn","early_winter","late_winter",
               "spring","summer","autumn","winter","spring/summer",
               "summer/autumn","autumn/winter","winter/spring")
first <- c(60,107,152,199,244,290,335,1,60,152,244,335,60,152,244,1)
last <- c(106,151,198,243,289,334,365,59,151,243,334,59,243,334,365,151)
# colour <- c(colfunc(16)[1], colfunc(16)[3], colfunc(16)[5], colfunc(16)[7],
#             colfunc(16)[9], colfunc(16)[11], colfunc(16)[13], colfunc(16)[15],
#             colfunc(16)[2], colfunc(16)[6], colfunc(16)[10], colfunc(16)[14],
#             colfunc(16)[4], colfunc(16)[8], colfunc(16)[12], colfunc(16)[16])
pos <- c(1,3,5,7,9,11,13,15,2,6,10,14,4,8,12,16)

seasons <- data.frame(seas_list, first, last, pos)

seasons


class <- matrix(nrow=110,ncol=4)

colnames(class) <- c("12.5%", "87.5%", "Season 75%")

for(id in 1:110){
  class[id,1] <- quantile(toby[[id]],c(.125,.875))[1]
  class[id,2] <- quantile(toby[[id]],c(.125,.875))[2]
}

class <- as.data.frame(class)



for(id in 1:110){
  
  for(j in 1:8){
    if(class[id,1] >= ron[j,2]){
      if(class[id,1] <= ron[j,3])
        if(class[id,2] <= ron[j,3]){
          class[id,3] <- as.character(ron$season[j])
          class[id,4] <- as.character(ron$colour[j])
        }
    }
  }
 
  if(is.na(class[id,3])){
    
    for(j in 9:12){
      if(class[id,1] >= ron[j,2]){
        if(class[id,1] <= ron[j,3])
          if(class[id,2] <= ron[j,3]){
            class[id,3] <- as.character(ron$season[j])
            class[id,4] <- as.character(ron$colour[j])
          }
      }
    }
  }
  
  if(is.na(class[id,3])){
    
  
    for(j in 13:16){
      if(class[id,1] >= ron[j,2]){
        if(class[id,1] <= ron[j,3])
          if(class[id,2] <= ron[j,3]){
            class[id,3] <- as.character(ron$season[j])
            class[id,4] <- as.character(ron$colour[j])
          }
      }
    }
  }
    
}

class$Colour[which(is.na(class$Colour))] <- "grey"

class[which(is.na(class[,3])),3] <- "none"
class

