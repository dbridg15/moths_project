#!usr/bin/env Rscript

# script: 7_the_model.R
# Desc:
# Author: David Bridgwood (dmb2417@ic.ac.uk)

# require
require(ggplot2)
require(RColorBrewer)

###############################################################################
#
###############################################################################


dat <- data.frame(x=runif(10),y=runif(10),
        grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)



dns.plt.df <- data.frame('day' = NA, 'id' = NA, 'season' = NA, 'colour' = NA)


for (id in 1:110){
    len <- length(individual.flights[[id]])

    tmp <- data.frame('day'    = individual.flights[[id]],
                      'id'     = rep(ss.df$id[id], len),
                      'season' = rep(ss.df$season[id], len),
                      'colour' = rep(ss.df$col[id], len))

    dns.plt.df <- rbind(dns.plt.df, tmp)
    rm(tmp)
}

dns.plt.df <- dns.plt.df[-1,]

View(dns.plt.df)

p <- ggplot(dns.plt.df, aes(day))
p <- p + geom_histogram()


print(p)
