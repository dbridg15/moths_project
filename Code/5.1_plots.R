#!usr/bin/env Rscript

# script: plot.R
# Desc: makes the pretty plot
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# line charts from the model
###############################################################################

prty.plt.df$x <- rep(-10000, nrow(prty.plt.df))
prty.plt.df$y <- rep(-10000, nrow(prty.plt.df))

# ffd line plot
ffd.plt <- ggplot(data=prty.plt.df, aes(x,y))
ffd.plt <-  ffd.plt + geom_point()
ffd.plt <-  ffd.plt + theme_bw()
ffd.plt <-  ffd.plt + scale_y_continuous(limits = c(0, 365))
ffd.plt <-  ffd.plt + scale_x_continuous(limits = c(5, 15))
ffd.plt <-  ffd.plt + geom_abline(data = prty.plt.df,aes(slope     = ffd.slope,
                                            intercept = ffd.intercept,
                                            color     = season))
ffd.plt <- ffd.plt + scale_color_brewer(palette = 'RdYlGn')
ffd.plt <- ffd.plt + theme(legend.key.size = unit(.3, "cm"),
                           legend.position = "bottom")
ffd.plt <- ffd.plt + labs(x = "Temperatures",
                          y = "Days")

print(ffd.plt)

# lfd line plot
lfd.plt <- ggplot(data=prty.plt.df, aes(x,y))
lfd.plt <-  lfd.plt + geom_point()
lfd.plt <-  lfd.plt + theme_bw()
lfd.plt <-  lfd.plt + scale_y_continuous(limits = c(0, 365))
lfd.plt <-  lfd.plt + scale_x_continuous(limits = c(5, 15))
lfd.plt <-  lfd.plt + geom_abline(data = prty.plt.df,aes(slope     = lfd.slope,
                                            intercept = lfd.intercept,
                                            color     = season))
lfd.plt <- lfd.plt + scale_color_brewer(palette = 'RdYlGn')
lfd.plt <- lfd.plt + theme(legend.key.size = unit(.3, "cm"),
                           legend.position = "bottom")
lfd.plt <- lfd.plt + labs(x = "Temperatures",
                          y = "Days")

print(lfd.plt)


###############################################################################
# big combined plots
###############################################################################

# ffd histogram
ffd.hist <- ggplot(prty.plt.df, aes(ffd.slope, fill = season))
ffd.hist <- ffd.hist + geom_histogram(binwidth = 0.1)
ffd.hist <- ffd.hist + scale_fill_brewer(palette = 'RdYlGn')
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
lfd.hist <- ggplot(prty.plt.df, aes(lfd.slope, fill = season))
lfd.hist <- lfd.hist + geom_histogram(binwidth = 0.1)
lfd.hist <- lfd.hist + scale_fill_brewer(palette = 'RdYlGn')
lfd.hist <- lfd.hist + guides(fill = FALSE)
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
a <- ggplot(prty.plt.df, aes(ffd.slope, lfd.slope, color = season))
a <- a + geom_hline(yintercept = 0, lwd = 0.2)
a <- a + geom_vline(xintercept = 0, lwd = 0.2)
a <- a + geom_point(size = 2)
a <- a + theme_bw()
a <- a + scale_color_brewer(palette = 'RdYlGn')
a <- a + guides(color = guide_legend(reverse=T))
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
grid.arrange(ffd.hist, empty, a, lfd.hist, ncol = 2, nrow = 2,
             widths = c(4, 1), heights = c(1, 4))
