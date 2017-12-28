#Winter 335-59
#EarlySpring 60-106
#LateSpring 107-151
#EarlySummer 152-198
#LateSummer 199-243
#EarlyAutumn 244-289
#LateAutumn 290-334

K <- 0.5 # percentage of population within season
Y <- 15 # no.years that X is met for season

#Making Seasons---------------------------------------------------------------------------

Seasons <- array(data = NA, dim = c(110,10,25), dimnames = NULL)

for (yr in 1:25){
  
  for(c in 1:3){
  Seasons[,c,yr] <-msummary1[,c] 
  }

for (id in 1:110){
  Seasons[id,4,yr] <- sum(as.numeric(moths1[id,4:62,yr]), na.rm = TRUE)+sum(as.numeric(moths1[id,338:368,yr-1]), na.rm = TRUE)
  Seasons[id,5,yr] <- sum(as.numeric(moths1[id,63:109,yr]), na.rm = TRUE)
  Seasons[id,6,yr] <- sum(as.numeric(moths1[id,110:154,yr]), na.rm = TRUE)
  Seasons[id,7,yr] <- sum(as.numeric(moths1[id,155:201,yr]), na.rm = TRUE)
  Seasons[id,8,yr] <- sum(as.numeric(moths1[id,202:246,yr]), na.rm = TRUE)
  Seasons[id,9,yr] <- sum(as.numeric(moths1[id,247:292,yr]), na.rm = TRUE)
  Seasons[id,10,yr] <- sum(as.numeric(moths1[id,293:337,yr]), na.rm = TRUE)
  }
}

View(Seasons[,,1])

#Making Classifications------------------------------------------------

Classifications <- matrix(nrow=110, ncol=29)
  #columns(id,bf,species,years 1:25, overall)
for(c in 1:3){
  Classifications[,c] <-msummary1[,c] 
  }
  
for (yr in 1:25){
  
  for (id in 1:110){
    if(as.numeric(Seasons[id,4,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Winter"
    if(as.numeric(Seasons[id,5,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Early Spring"
    if(as.numeric(Seasons[id,6,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Late Spring"
    if(as.numeric(Seasons[id,7,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Early Summer"
    if(as.numeric(Seasons[id,8,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Late Summer"
    if(as.numeric(Seasons[id,9,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Early Autumn"
    if(as.numeric(Seasons[id,10,yr]) > K*sum(as.numeric(Seasons[id,4:10,yr]))) Classifications[id,yr+3] <- "Late Autumn"
    }
}

for (id in 1:110){
if(length(which(Classifications[id,4:28] == "Winter")) >Y) Classifications[id,29] <- "Winter" 
if(length(which(Classifications[id,4:28] == "Early Spring")) + length(which(Classifications[id,4:28] == "Late Spring")) >Y) Classifications[id,29] <- "Spring"
if(length(which(Classifications[id,4:28] == "Early Summer")) + length(which(Classifications[id,4:28] == "Late Summer")) >Y) Classifications[id,29] <- "Summer"
if(length(which(Classifications[id,4:28] == "Early Autumn")) + length(which(Classifications[id,4:28] == "Late Autumn")) >Y) Classifications[id,29] <- "Autumn"
}


View(Classifications)

#Cleanup-----------------------------------------------------#####
rm(M,K,Y,c,id,yr)

