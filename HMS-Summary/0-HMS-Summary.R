#####################################################################################################
#
# HMS Data Summary: Machines and Coffee Pods
# Xiliang Lin
# Sept, 2015
#
#####################################################################################################
# Settings
rm(list = ls())
run_on_Linux        = FALSE    # Determines the path settings

coffee_modules      = c(1463, 1464, 1465, 1466, 1467)
#Module Description
#1463 - Ground and Whole Bean Coffee - including most Keurig K-Cups
#1464 - Soluable Flavored Coffee - Latte, Cappuccino etc.
#1465 - Soluable Coffee - Essentially instant coffee.
#1466 - Liquid Coffee- e.g. Starbucks Frappuccino.
#1467 - Coffee Substitutes such as POSTUM and Pero, tends to be fairly small.

maker_modules       = 7755
#Module Description
#7755 - Coffee and Tea Maker Appliances - Single Cup Serving or Dripping Machines.

#Load Necessary Packages
library(data.table)
setNumericRounding(0)
library(ggplot2)
library(gridExtra)
library(scales)

#Set Working Folder Path Here
setwd("~/Keurig")
input_dir = "Data/HMS-Transactions"
meta_dir  = "Data/Meta-Data"
output_dir = "Data/HMS-Summary"
graph_dir = "Tabfigs/HMS-Summary"

#Source the function file
source('Scripts/HMS-Summary/functions.R')
#---------------------------------------------------------------------------------------------------#
# Scripts

# Load and pre-process the data 
source('Scripts/HMS-Summary/1-Data-Cleaning.R', echo=T)

# Discrete Choice Assumption and Choice Behavior For Given Trips
one_person_hh = TRUE
source('Scripts/HMS-Summary/2-Discrete-Choice.R', echo=T)

# Choice Concentration and spending concentration.
one_person_hh = TRUE
source('Scripts/HMS-Summary/3-Choice-Concentration.R', echo=T)

# Temporal Choice Behavior
source('Scripts/HMS-Summary/4-Temporal-Choice.R', echo=T)

# Platform evolution of Keurig - such as variety available and adoption.
source('Scripts/HMS-Summary/5-Platform-Evolution.R', echo=T)

# RMS DMA table with seasonal information (Need to run 5 first before running 6)
source('Scripts/HMS-Summary/6-RMS-DMA-Brand-Summary.R', echo=T)
#---------------------------------------------------------------------------------------------------#
