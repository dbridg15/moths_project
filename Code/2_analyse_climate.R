#!usr/bin/env Rscript

# script: 3_analyse_climate.R
# Desc:   analysis of climate data
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# making temperatures data frame
###############################################################################

# start off data frame with mean yearly temperature
temperatures <- data.frame('year' = c(1960:2014),
               'ytemp' = apply(daily.temp, 2, mean))

# add the seasons
for (i in 1:nrow(seasons)){
  temperatures[as.character(seasons$season[i])] <-
    colMeans(daily.temp[seasons$first.day[i]:seasons$last.day[i],])
}

# winter is weired! (goes across 2 years)
#335-59 (335-365 of yr-1 and 1:59 of yr)

# only have the second half of winter for 1960 (no 1959 data)
temperatures$winter[1] <- mean(daily.temp[1:59,1])

for (yr in 2:55){
  temperatures$winter[yr] <- mean(c(daily.temp[335:356,yr-1],
                                    daily.temp[1:59, yr]))
}

#add cons
temperatures$cons <- cons

rm(i, yr, cons)


###############################################################################
# is temperature changing?
###############################################################################

# ****if you get more time do for only 1990-2014****

# initialise dataframe to put results in
temperature.analysis <- data.frame('time_period' =colnames(temperatures)[2:19])
temperature.analysis$slope <- NA
temperature.analysis$p.val <- NA
temperature.analysis$r.sqr <- NA


for (i in 2:19){  #2-19 bc 1 is year (not a temperature measure)
  mdl <- lm(as.numeric(unlist(temperatures[i])) ~ temperatures$year)
  temperature.analysis$slope[i-1] <- as.numeric(coef(mdl)[2])  # slope
  temperature.analysis$p.val[i-1] <- as.numeric(anova(mdl)$'Pr(>F)'[1]) # p
  temperature.analysis$r.sqr[i-1] <- as.numeric(summary(mdl)[8])  # R squared
}


###############################################################################
# the plots (one for each season)
###############################################################################

# setting up dataframe for plot
plt.df <- temperatures
plt.df$mothyear <- "no"  # will have seperate regression line for 1990:2014

tmp <- subset(temperatures, year > 1989)
tmp$mothyear <- "yes"
plt.df <- rbind(plt.df, tmp)

# plots
pdf("../Results/plots/climate/temperature_plots.pdf", width = 10, height = 7.5)

for (i in 2:19){
  temp.plt <- ggplot(plt.df, aes(year, plt.df[i], color = mothyear))
  temp.plt <- temp.plt + labs(x = "Years",
                              y = paste("Mean Temp.", colnames(plt.df)[i]))
  temp.plt <- temp.plt + theme_classic()
  temp.plt <- temp.plt + geom_point()
  temp.plt <- temp.plt + geom_smooth(method = 'lm')
  temp.plt <- temp.plt + ggtitle(
                       paste("slope:", temperature.analysis$slope[i-1], "\n",
                             "P Value:", temperature.analysis$p.val[i-1], "\n",
                             "R Squared:", temperature.analysis$r.sqr[i-1]))

  suppressMessages(print(temp.plt))
}

dev.off()  # close pdf

# cleanup
rm(i, mdl, temp.plt, plt.df, tmp)
