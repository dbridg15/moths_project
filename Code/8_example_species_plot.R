#!usr/bin/env Rscript

# script: 0_run_moths_project.R
# Desc: runs the project
# Author: David Bridgwood (dmb2417@ic.ac.uk)


###############################################################################
# silver ground carpet id = 42
###############################################################################

example.species <- data.frame("measure" = c(rep("FFD", 25), rep("LFD", 25)),
                              "day"     = c(aa.ss.flight["42", "FFD", ],
                                            aa.ss.flight["42", "LFD", ]))

example.species$ytemp  <- as.numeric(rep(temperatures$ytemp[31:55], 2))
example.species$stemp  <- as.numeric(rep(temperatures$early.summer[31:55], 2))
example.species$winter <- as.numeric(rep(temperatures$winter[31:55], 2))


a <- ggplot(example.species, aes(x = ytemp, y = day, color = measure))
a <- a + geom_point()
a <- geom_smooth(method = 'lm')
plot(a)
