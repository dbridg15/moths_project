moths_project
│
├── Code
│   │
│   ├── 0.0_run_moths_project.R-----------sources all R scripts and performs
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
│   ├── extract_mean_temp.py--------------python script to extract data from
│   │                                     required grid squares
│   │
│   └──Legacy-----------------------------contains old code still to be sorted
│       │
│       ├── aaFlight Period.R
│       ├── Example species figure.R
│       ├── Figures.R
│       ├── Flight Period.R
│       ├── Model-DAVID.R
│       ├── Model.R
│       ├── phenology hypothesis.R
│       ├── Season plot-DAVID.R
│       ├── Season plot.R
│       ├── seasontemp and number.R
│       ├── Sensitivity.R
│       └── shcplot.R
│
├── Data
│   │
│   ├── Climate--------------------------contains all climate data
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
│   ├── Climate_Source-------------------RAW climate data (in .gitignore)
│   │
│   └── Moths----------------------------contains all moth filght data
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
└── Results-----------------------------where results go!
