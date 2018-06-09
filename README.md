# moths_project

__Author:__ David Bridgwood *dbridg15@gmail.com*

__Description:__ Project to determine if the timing of moth flights (first flight day and last flight day) are being affected by changing climate (specifically temperature).

## Dependencies

The following R packages are required

- ggplot2-----------------------plotting
- RColorBrewer--------------setting colour gradient of seaons on combined plots
- gridExtra---------------------combining plots
- lme4---------------------------models
- reshape2---------------------reshaping dataframes

## How to Run

Set working directory to moths_project/Code/ and Rscript/source the file 0_run_moths_project.R
The default has sensitivity Analysis turned off and take aprox 5 mins to run. With sensitivity analysis on (see below). Run time is >30mins.

__In R__

	source 0_run_moths_project.R

__In terminal__

	Rscript  0_run_moths_project.R

## Sensitivity Analysis

To turn on sensitivity analysis amend 0_run_moths_project.R file line 24.

	with_sensitivity <- TRUE  # set to TRUE to run with sensitivity analysis

By default sensitvity analysis runs though these values - which can be ammended.

	# values for sesitivity analysis *MUST INCLUDE X=3 and N=20 TO RUN*
	X.vals <- c(1, 3, 5, 10, 15, 25)    # sightings for mean ffd day
	N.vals <- c(3, 5, 10, 15, 20, 25)   # minimum years to include
	
## To Do
- Current choice of colour gradient for combined plot does not have enough
  colours for all seasons when more leniant values are used in sensitivity
  analysis

## Repository Structure
```
moths_project/
│
├── Code/
│	│
│	├── 0_run_moths_project.R-------------source/Rscript this file to run the project
│	│
│	├── functions.R-----------------------code for functions used in project
│	│
│	├── 1_read_sort_data.R----------------reads in moth and climate data and produces summaries
│	│
│	├── 2_analyse_climate.R---------------analysis of climate data
│	│
│	├── 3_select_&_adjust_abundance.R-----select species for analysis & adjust FFD/LFD by sampling to least abundant year
│	│
│	├── 4_effects_on_moths.R--------------are moths flying earlier/later/longer
│	│
│	├── 5_the_model.R---------------------produces general lines model from the data and plots for each species
│	│
│	├── 6_sorting_results.R---------------reads in and amalgamates results and saves to csv file
│	│
│	├── 7_example_plots.R-----------------plots FFD and LFD response for example species (Silver-ground carpet)
│	│
│	└── extract_mean_temp.py--------------python script to extract data from required grid squares
│
├── Data/
│	│
│	├── Climate/-------------------------contains all climate data
│	│	│
│	│	├── MeanTemp_1.csv-------------⎤
│	│	├── MeanTemp_2.csv-------------⎥
│	│	├── MeanTemp_3.csv-------------⎥
│	│	├── MeanTemp_4.csv-------------⎥
│	│	├── MeanTemp_5.csv-------------⎬ mean daily temp at each grid square
│	│	├── MeanTemp_6.csv-------------⎥
│	│	├── MeanTemp_7.csv-------------⎥
│	│	├── MeanTemp_8.csv-------------⎥
│	│	└── MeanTemp_9.csv-------------⎦
│	│
│	├── Climate_Source/------------------RAW climate data (in .gitignore)
│	│
│	└── Moths/---------------------------contains all moth filght data
│		│
│		├── Days.csv---------------------days where no moths were recorded
│		│
│		├── Moths1990.csv--------------⎤
│		├── Moths1991.csv--------------⎥
│		├── Moths1992.csv--------------⎥
│		├── Moths1993.csv--------------⎥
│		├── Moths1994.csv--------------⎥
│		├── Moths1995.csv--------------⎥
│		├── Moths1996.csv--------------⎥
│		├── Moths1997.csv--------------⎥
│		├── Moths1998.csv--------------⎥
│		├── Moths1999.csv--------------⎥
│		├── Moths2000.csv--------------⎥
│		├── Moths2001.csv--------------⎥
│		├── Moths2002.csv--------------⎬ moth flight data for given year
│		├── Moths2003.csv--------------⎥
│		├── Moths2004.csv--------------⎥
│		├── Moths2005.csv--------------⎥
│		├── Moths2006.csv--------------⎥
│		├── Moths2007.csv--------------⎥
│		├── Moths2008.csv--------------⎥
│		├── Moths2009.csv--------------⎥
│		├── Moths2010.csv--------------⎥
│		├── Moths2011.csv--------------⎥
│		├── Moths2012.csv--------------⎥
│		├── Moths2013.csv--------------⎥
│		├── Moths2014.csv--------------⎦
│		│
│		└── Speciesindexlist.csv--------species index list id, B&F, common name
│
└── Results/----------------------------files in this directory are produced  when the project is run
	│
	├── ChiSqr_Results.csv--------------csv with results from Chi-Squared tests or each sensitivity analysis
	│
	├── Models_summary.txt--------------summary of every model for for each run of the sensitivity analysis
	│
	├── plots/--------------------------pdfs of plots
	│
	└── r_data/-------------------------.rda files from each run of sensitivity analysis
```
