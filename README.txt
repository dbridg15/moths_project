###############################################################################
# moths_project
###############################################################################

Author: David Bridgwood (dmb2417@ic.ac.uk)

Desc  : project to determine if the timing of moth flights (first flight day
        and last flight day) are being affected by changing climate
        (specifically temperature)

* To run project set wd to moths_project/Code/ and Rscript/source the file
  0.0_run_moths_project.R  approx 30 min runtime

###############################################################################
# known issues/to do
###############################################################################

* Current choice of colour gradient for combined plot does not have enough
  colours for all seasons when more leniant values are used in sensitivity
  analysis

###############################################################################
# required R packages
###############################################################################

ggplot2        # for all plots
RColorBrewer   # for colour gradient of seaons on combined plots
gridExtra      # for putting together combined plot
lme4           # for models
reshape2       # for reshaping dataframes

###############################################################################
# repository structure
###############################################################################

moths_project/
│
├── Code/
│   │
│   ├── 0.0_run_moths_project.R-----------source/Rscript this file to run the
│   │                                     project - set variables for
│   │                                     sensitivity analysis
│   │
│   ├── 0.1_functions.R-------------------code for functions used in project
│   │
│   │
│   ├── 1_read_sort_data.R----------------reads in moth and climate data and
│   │                                     produces some useful summaries
│   │
│   ├── 2_analyse_climate.R---------------analysis of climate data
│   │
│   ├── 3_select_&_adjust_abundance.R-----select species for analysis & adjust
│   │                                     FFD/LFD by sampling to least abundant
│   │                                     year
│   │
│   ├── 4_effects_on_moths.R--------------are moths flying earlier/later/longer
│   │
│   ├── 5_the_model.R---------------------produces general lines model from the
│   │                                     data and plots for each species
│   │
│   ├── 6_sorting_results.R---------------reads in and amalgamates results and
│   │                                     saves to csv file
│   │
│   ├── 7_example_plots.R-----------------plots FFD and LFD response for exampl
│   │                                     species (Silver-ground carpet)
│   │
│   └── extract_mean_temp.py--------------python script to extract data from
│                                         required grid squares
│
├── Data/
│   │
│   ├── Climate/-------------------------contains all climate data
│   │   │
│   │   ├── MeanTemp_1.csv-------------⎤
│   │   ├── MeanTemp_2.csv-------------⎥
│   │   ├── MeanTemp_3.csv-------------⎥
│   │   ├── MeanTemp_4.csv-------------⎥
│   │   ├── MeanTemp_5.csv-------------⎬ mean daily temp at each grid square
│   │   ├── MeanTemp_6.csv-------------⎥
│   │   ├── MeanTemp_7.csv-------------⎥
│   │   ├── MeanTemp_8.csv-------------⎥
│   │   └── MeanTemp_9.csv-------------⎦
│   │
│   ├── Climate_Source/------------------RAW climate data (in .gitignore)
│   │
│   └── Moths/---------------------------contains all moth filght data
│       │
│       ├── Days.csv---------------------days where no moths were recorded
│       │
│       ├── Moths1990.csv--------------⎤
│       ├── Moths1991.csv--------------⎥
│       ├── Moths1992.csv--------------⎥
│       ├── Moths1993.csv--------------⎥
│       ├── Moths1994.csv--------------⎥
│       ├── Moths1995.csv--------------⎥
│       ├── Moths1996.csv--------------⎥
│       ├── Moths1997.csv--------------⎥
│       ├── Moths1998.csv--------------⎥
│       ├── Moths1999.csv--------------⎥
│       ├── Moths2000.csv--------------⎥
│       ├── Moths2001.csv--------------⎥
│       ├── Moths2002.csv--------------⎬ moth flight data for given year
│       ├── Moths2003.csv--------------⎥
│       ├── Moths2004.csv--------------⎥
│       ├── Moths2005.csv--------------⎥
│       ├── Moths2006.csv--------------⎥
│       ├── Moths2007.csv--------------⎥
│       ├── Moths2008.csv--------------⎥
│       ├── Moths2009.csv--------------⎥
│       ├── Moths2010.csv--------------⎥
│       ├── Moths2011.csv--------------⎥
│       ├── Moths2012.csv--------------⎥
│       ├── Moths2013.csv--------------⎥
│       ├── Moths2014.csv--------------⎦
│       │
│       └── Speciesindexlist.csv--------species index list id, B&F, common name
│
└── Results/----------------------------files in this directory are produced
    │                                   when the project is run
    │
    ├── ChiSqr_Results.csv--------------csv with results from Chi-Squared tests
    │                                   for each measure/values of X/N from
    │                                   sensitivity analysis
    │
    ├── Models_summary.txt--------------summary of every model for for each run
    │                                   of the sensitivity analysis
    │
    ├── plots/--------------------------pdfs of plots are saved here
    │
    └── r_data/-------------------------.rda files from each run of the
                                        sensitivity analysis are saved here
