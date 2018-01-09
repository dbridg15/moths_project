#!usr/bin/env Rscript

# script: 7_example_plots.R
# Desc:   plots FFD and LFD response for example species
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# Example species - Silver-ground Carpet (ID = 42)
###############################################################################

# choose species
ID <- "42"

# load data from X = 3 and N = 20
load("../Results/r_data/3_20.rda")

# dataframe with example species data
exmp.spc <- data.frame("FFD"    = flight[ID, "FFD", ],
                       "LFD"    = flight[ID, "LFD", ],
                       "aa.FFD" = aa.ss.flight[ID, "FFD", ],
                       "aa.LFD" = aa.ss.flight[ID, "LFD", ],
                       "ytemp"  = temperatures$ytemp[31:55],
                       "stemp"  = temperatures[31:55, as.character(
                                           all.spc.df$season[as.numeric(ID)])],
                       "winter" = temperatures$winter[31:55])


# make df for plot
plt.df <- melt(exmp.spc, id=c("ytemp", "stemp", "winter"))  # melt

# column saying whether data was adjusted
plt.df$adjusted <- NA
plt.df$adjusted[which(plt.df$variable %in% c("aa.FFD","aa.LFD"))] <- "adjusted"
plt.df$adjusted[which(is.na(plt.df$adjusted))] <- "observed"

# FFD or LFD - get rid of aa.
plt.df$variable[which(plt.df$variable == "aa.FFD")] <- "FFD"
plt.df$variable[which(plt.df$variable == "aa.LFD")] <- "LFD"


# start pdf
pdf("../Results/plots/example_species.pdf")

for(i in c("ytemp", "stemp", "winter")){  # for expl

    exmp.plt <- ggplot(plt.df, aes_string(x     = i,
                                          y     = "value",
                                          color = "adjusted",
                                          shape = "variable"))
    exmp.plt <- exmp.plt + geom_point()
    exmp.plt <- exmp.plt + geom_smooth(method = 'lm', se = FALSE)
    exmp.plt <- exmp.plt + theme_classic()
    exmp.plt <- exmp.plt + labs(x = paste(i, "Temperature (celcius)"),
                                y = "Day")
    exmp.plt <- exmp.plt + ggtitle(paste(all.spc.df[ID, 3], ": ", i))

    print(exmp.plt)
}

dev.off()
