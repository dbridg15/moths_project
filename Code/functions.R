#!usr/bin/env Rscript

# script: 0.1_functions.R
# Desc:   contains code for functions used in moths_project
# Author: David Bridgwood (dbridg15@gmail.com)

# functions

# PrettyPlots: produces line graphs for ffd and lfd, and a combined
#              scatterplot/summary histograms from slope data. Either empirical
#              or from model.


# define function!
# takes in dat: data.frame with headings (id, season, ffd.slope, ffd.intercept,
#                                         lfd.slope, lfd.intercept)
# and plt.title which will be the title of the plots!
PrettyPlots <- function(dat, plt.title){

###############################################################################
# line charts from the model
###############################################################################

# levels go backwards so the histograms plot with levels in the right order...
dat$season <- factor(dat$season, levels = rev(seas.list))

# add dudd data so ggplot will work (for geom_abline)
dat$x <- rep(-10000, nrow(dat))
dat$y <- rep(-10000, nrow(dat))

# ffd line plot
ffd.plt <-  ggplot(data=dat, aes(x,y))                        # plot dudd data
ffd.plt <-  ffd.plt + geom_point()                            # as points...
ffd.plt <-  ffd.plt + theme_bw()                              # nice theme
ffd.plt <-  ffd.plt + scale_y_continuous(limits = c(0, 365))  # whole year
ffd.plt <-  ffd.plt + scale_x_continuous(limits = c(5, 15))   # 5-15 celcius
ffd.plt <-  ffd.plt +                                         # plot line from
    geom_abline(data = dat,aes(slope     = ffd.slope,         # slope and
                               intercept = ffd.intercept,     # intercept
                               color     = season))           # color on season
ffd.plt <- ffd.plt + scale_color_brewer(palette = colgrad)   # color pallete
ffd.plt <- ffd.plt + theme(legend.key.size = unit(.3, "cm"),  # key size
                           legend.position = "bottom")        # and position
ffd.plt <- ffd.plt + labs(x = "Temperatures",                 # axis labels
                          y = "Days")
ffd.plt <- ffd.plt + ggtitle(paste("FFD: ", plt.title))       # title!

suppressWarnings(print(ffd.plt))  # print plot and suppress warning about
                                  # data out of range

# lfd line plot (comments as above)
lfd.plt <- ggplot(data=dat, aes(x,y))
lfd.plt <-  lfd.plt + geom_point()
lfd.plt <-  lfd.plt + theme_bw()
lfd.plt <-  lfd.plt + scale_y_continuous(limits = c(0, 365))
lfd.plt <-  lfd.plt + scale_x_continuous(limits = c(5, 15))
lfd.plt <-  lfd.plt + geom_abline(data = dat,aes(slope     = lfd.slope,
                                                 intercept = lfd.intercept,
                                                 color     = season))
lfd.plt <- lfd.plt + scale_color_brewer(palette = colgrad)
lfd.plt <- lfd.plt + theme(legend.key.size = unit(.3, "cm"),
                           legend.position = "bottom")
lfd.plt <- lfd.plt + labs(x = "Temperatures",
                          y = "Days")
lfd.plt <- lfd.plt + ggtitle(paste("LFD: ", plt.title))

suppressWarnings(print(lfd.plt))


###############################################################################
# big combined plots
###############################################################################

# ffd histogram of slope coloured by season of species
ffd.hist <- ggplot(dat, aes(ffd.slope, fill = season))
ffd.hist <- ffd.hist + geom_histogram(binwidth = 0.1)
ffd.hist <- ffd.hist + scale_fill_brewer(palette = colgrad)
ffd.hist <- ffd.hist + guides(fill = FALSE)
ffd.hist <- ffd.hist + theme_bw()
ffd.hist <- ffd.hist + theme(axis.line.x      = element_blank(),
                             axis.title.x     = element_blank(),
                             axis.text.x      = element_blank(),
                             axis.ticks.x     = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border     = element_blank())


# ldf histogram
lfd.hist <- ggplot(dat, aes(lfd.slope, fill = season))
lfd.hist <- lfd.hist + geom_histogram(binwidth = 0.1)
lfd.hist <- lfd.hist + scale_fill_brewer(palette = colgrad)
lfd.hist <- lfd.hist + guides(fill = FALSE)
lfd.hist <- lfd.hist + coord_flip()  # rotate 90 degrees
lfd.hist <- lfd.hist + theme_bw()
lfd.hist <- lfd.hist + theme(axis.line.y      = element_blank(),
                             axis.title.y     = element_blank(),
                             axis.text.y      = element_blank(),
                             axis.ticks.y     = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border     = element_blank())

# scatter plot of lfd.slope by ffd.slope, points coloured by season of species
a <- ggplot(dat, aes(ffd.slope, lfd.slope, color = season))
a <- a + geom_hline(yintercept = 0, lwd = 0.2)  # add horizontal
a <- a + geom_vline(xintercept = 0, lwd = 0.2)  # and vertical lines
a <- a + geom_point(size = 2)
a <- a + theme_bw()
a <- a + scale_color_brewer(palette = colgrad)
a <- a + guides(color = guide_legend(reverse=T))
a <- a + theme(legend.key.size  = unit(.3, "cm"),
               legend.position  = c(1.15, 1.13),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank())
a <- a + labs(x = paste("FFD Response to ", plt.title),
              y = paste("LFD Response to ", plt.title))

# empty plot for top right corner
empty <- ggplot() + geom_point(aes(1,1), colour="white") +
         theme(axis.ticks       = element_blank(),
               panel.background = element_blank(),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.title.x     = element_blank(),
               axis.title.y     = element_blank())

# now put them in a grid again suppressing warnings
suppressWarnings(grid.arrange(ffd.hist, empty, a, lfd.hist, ncol = 2, nrow = 2,
             widths = c(4, 1), heights = c(1, 4)))

}
