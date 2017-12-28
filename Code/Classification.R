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
  
  plot(1,1,xlim=c(((which(moths2[id,4:368] > 0)[1])-7),(tail(which(moths2[id,4:368] > 0),n='1')+7)),ylim=c(0,round_any(max(test4),0.1,ceiling)),main= moths2[id,3])
  abline(v=c(31,59,90,120,151,181,212,243,273,304,334,365),col=alpha("black",0.5))
  
  for (yr in 1:25){
     if(msummary[which(msummary[,1] == moths1[id,1,1]),yr+3] > 2){
        lines(density(ron[[yr]]),col=alpha(colfunc(25)[yr],0.5))}
  }
 lines(density(Overall),col="blue")
  
  toby[[id]] <- Overall
  
}

 plot(1,1,xlim=c(1,365),ylim=c(0,0.1),xaxs="i",yaxs="i")
 abline(v=c(31,59,90,120,151,181,212,243,273,304,334,365),col=alpha("black",0.5))
 abline(v=c(59,151,243,334),col=alpha("black",0.5),lwd=2)
 
 for (id in 1:110){
   lines(density(as.numeric(toby[[id]])),col=alpha("blue",0.5))
 
 }



#CLASSIFICATIONS------------------------------------------------------------

class <- matrix(nrow=110,ncol=4)

colnames(class) <- c("12.5%", "87.5%", "Season 75%","Colour")

for(id in 1:110){
  class[id,1] <- quantile(toby[[id]],c(.125,.875))[1]
  class[id,2] <- quantile(toby[[id]],c(.125,.875))[2]
}

class <- as.data.frame(class)

View(class)

df <- read.table(text="152  198   Early_Summer  yellow2
199   243   Late_Summer   yellow3
1   59   Winter    blue
335   365   Winter    blue
60   151   Spring     green
152    243    Summer    yellow
244   334   Autumn    Red
1   151   Winter/Spring     turquoise
60    243   Spring/Summer   palegreen
152   334   Summer/Autumn   orange
244   365   Autumn/Winter   purple", stringsAsFactors=F)


for(id in 1:110){
 
  for(j in 1:2){
    
    if(class[id,1] >= df[j,1]){
      if(class[id,1] <= df[j,2]){
        if(class[id,2] <= df[j,2]){
          class[id,3] <- df[j,3]
          class[id,4] <- df[j,4]
        }}}
    
  }
  
  if(is.na(class[id,3])){
  
    for(j in 3:7){
   
      if(class[id,1] >= df[j,1]){
        if(class[id,1] <= df[j,2]){
          if(class[id,2] <= df[j,2]){
            class[id,3] <- df[j,3]
            class[id,4] <- df[j,4]
        }}}
      
    }
  }

  if(is.na(class[id,3])){
    
    for(j in 8:11){
      
      if(class[id,1] >= df[j,1]){
        if(class[id,1] <= df[j,2]){
          if(class[id,2] <= df[j,2]){
            class[id,3] <- df[j,3]
            class[id,4] <- df[j,4]
          }}}
    }
  }  
  
  if(is.na(class[id,3])){
    class[id,4] <- "black"
    
  }
}

View(class)

rm(day,id,j,temp,temp1,test4,yr)

colfunc <- colorRampPalette(c("lightblue", "yellow", "red"))


#plot(rep(1,12),col=colfunc(12),pch=19,cex=3)


