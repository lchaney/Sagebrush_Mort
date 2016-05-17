#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to LOAD data needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#this data set has survival (last data collection in May 2015) for all three gardens

	surv3d <- read.csv("Data/3gardsurv_2016.csv")


#climate data set has daily avg, min, and max temperature for three garden sites

	climate <- read.csv("Data/daily_temps.csv")
	
	  #note that moving averages were calculated then copied into excel 
		#(due to missing values, I couldn't figure out how match them up in R)
		
		
#this data set is Provenance climate data (i.e., seed source population climate)
	prov_clim <- read.csv("Data/prov_clim.csv")
	
	
#install and load required packages
	source('Analysis/01a_SM_load_pack.R')
