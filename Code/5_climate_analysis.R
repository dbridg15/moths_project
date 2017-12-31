#!usr/bin/env Rscript

# script: 5_climate_analysis.R
# Desc: analysis of climate data
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# making temperatures data frame
###############################################################################

# start off data frame with mean yearly temperature
temperatures <- data.frame('year' = c(1960:2014),
                           'ytemp' = apply(dtemp, 2, mean))

# add the seasons
for(i in 1:nrow(seasons)){
    temperatures[as.character(seasons$season[i])] <-
        colMeans(dtemp[seasons$first[i]:seasons$last[i],])
}

# winter is weired! (goes across 2 years)
#335-59 (335-365 of yr-1 and 1:59 of yr)

# only have the second half of winter for 1960 (no 1959 data)
temperatures$winter[1] <- mean(dtemp[1:59,1])

for(yr in 2:55){
    temperatures$winter[yr] <- mean(c(dtemp[335:356,yr-1], dtemp[1:59, yr]))
}

rm(i, yr)


###############################################################################
# is temperature changing?
###############################################################################

# get some numbers

temperature_analysis <- data.frame('time_period' =colnames(temperatures)[2:18])
temperature_analysis$slope <- NA
temperature_analysis$p_val <- NA
temperature_analysis$r_sqr <- NA

for(i in 2:18){
    mdl <- lm(as.numeric(unlist(temperatures[i])) ~ temperatures$year)
    temperature_analysis$slope[i-1] <- as.numeric(coef(mdl)[2])  # slope
    temperature_analysis$p_val[i-1] <- as.numeric(anova(mdl)$'Pr(>F)'[1]) # p-value
    temperature_analysis$r_sqr[i-1] <- as.numeric(summary(mdl)[8])  # R squared
}

# plots
if(PLOTS){

pdf("../Results/temperature_plots.pdf", width = 10, height = 7.5)

for(i in 2:18){
    print(qplot(temperatures$year, temperatures[i], xlab = "Years",
            ylab = paste("Mean Temperature", colnames(temperatures)[i])) +
            theme_classic() +
            geom_smooth(method = 'lm', col = 'black') +
            ggtitle(paste("slope:", temperature_analysis$slope[i-1], "\n",
                          "P Value:", temperature_analysis$p_val[i-1], "\n",
                          "R Squared:", temperature_analysis$r_sqr[i-1])))
}

dev.off()

}

rm(i)
