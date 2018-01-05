#!usr/bin/env Rscript

# script: plot.R
# Desc: makes the pretty plot
# Author: David Bridgwood (dmb2417@ic.ac.uk)

# require
require(ggplot2)
require(RColorBrewer)
require(gridExtra)

###############################################################################
#
###############################################################################

prty.plt.df <- cbind(ss.df[ , c(1,4)], ffd.resp, lfd.resp)
prty.plt.df$season <- factor(prty.plt.df$season, levels = rev(seas.list))


# ffd histogram
ffd.hist <- ggplot(prty.plt.df, aes(ffd.resp, fill = season))
ffd.hist <- ffd.hist + geom_histogram(binwidth = 0.1)
ffd.hist <- ffd.hist + scale_fill_brewer(palette = 'RdYlGn')
ffd.hist <- ffd.hist + guides(fill = FALSE)
ffd.hist <- ffd.hist + scale_x_continuous(limits = c(-2.5, 2.5))
ffd.hist <- ffd.hist + theme_bw()
ffd.hist <- ffd.hist + theme(axis.line.x      = element_blank(),
                             axis.title.x     = element_blank(),
                             axis.text.x      = element_blank(),
                             axis.ticks.x     = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border     = element_blank())


# ldf histogram
lfd.hist <- ggplot(prty.plt.df, aes(lfd.resp, fill = season))
lfd.hist <- lfd.hist + geom_histogram(binwidth = 0.1)
lfd.hist <- lfd.hist + scale_fill_brewer(palette = 'RdYlGn')
lfd.hist <- lfd.hist + guides(fill = FALSE)
lfd.hist <- lfd.hist + scale_x_continuous(limits = c(-2.5, 2.5))
lfd.hist <- lfd.hist + coord_flip()
lfd.hist <- lfd.hist + theme_bw()
lfd.hist <- lfd.hist + theme(axis.line.y      = element_blank(),
                             axis.title.y     = element_blank(),
                             axis.text.y      = element_blank(),
                             axis.ticks.y     = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border     = element_blank())

# scatter plot
a <- ggplot(prty.plt.df, aes(ffd.resp, lfd.resp, color = season))
a <- a + geom_hline(yintercept = 0, lwd = 0.2)
a <- a + geom_vline(xintercept = 0, lwd = 0.2)
a <- a + geom_point(size = 2)
a <- a + theme_bw()
a <- a + scale_color_brewer(palette = 'RdYlGn')
a <- a + guides(color = guide_legend(reverse=T))
a <- a + scale_x_continuous(limits = c(-2.5, 2.5))
a <- a + scale_y_continuous(limits = c(-2.5, 2.5))
a <- a + theme(legend.key.size  = unit(.3, "cm"),
               legend.position  = c(1.15, 1.13),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank())

# empty plot for top right corner
empty <- ggplot() + geom_point(aes(1,1), colour="white") +
         theme(axis.ticks       = element_blank(),
               panel.background = element_blank(),
               axis.text.x      = element_blank(),
               axis.text.y      = element_blank(),
               axis.title.x     = element_blank(),
               axis.title.y     = element_blank())

# now put them in a grid
grid.arrange(ffd.hist, empty, a, lfd.hist, ncol=2, nrow=2,
             widths = c(4, 1), heights = c(1, 4))



