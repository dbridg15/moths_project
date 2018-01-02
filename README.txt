moths_project
│
├── Code
│   │
│   ├── 0_extract_mean_temp.py------------python script to extract data from
│   │                                     required grid squares
│   │
│   ├── 0_run_moths_project.R-------------sources all R scripts
│   │
│   ├── 1-read_in_data.R------------------reads in moth and climate data
│   │
│   ├── 2_sorting_data.R------------------sorts data ready for analysis
│   │
│   ├── 3_adjusting_abundance.R-----------adjusts FFD/LFD by sampling to least
│   │                                     abundant year
│   │
│   ├── 4_species_classifications.R-------classifies species into season
│   │
│   ├── 5_climate_analysis.R--------------analysis of climate data
│   │
│   ├── 6_effect_on_moths.R---------------are moths flying earlier/later/longer
│   │
│   ├── 7_the_model.R---------------------
│   │
│   └──Legacy-----------------------------contains old code still to be sorted
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
