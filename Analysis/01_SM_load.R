#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to LOAD data needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#=== change source file location ===#

#this data set has survival (last data collection in May 2015) for all three gardens

	surv3d <- read.csv("~/Documents/Sagebrush/Manuscript Scripts and Data/Data/3gardsurv_2015.csv")



#climate data set has daily avg, min, and max temperature for three garden sites
#Date ranges: 1/1/10 - 5/8/15 for Ephraim and Orchards and 10/20/10 - 3/13/14 for Majors
#Ephraim data accessed here: http://www.wcc.nrcs.usda.gov/nwcc/site?sitenum=2126&state=ut
#Orchards data accessed here: http://www.wcc.nrcs.usda.gov/nwcc/site?sitenum=674&state=id
#Majors data collected for data logger temperature probe

	climate <- read.csv("~/Documents/Sagebrush/Manuscript Scripts and Data/Data/daily_temps3.csv")

		#note that moving averages were calculated then copied into excel 
		#(due to missing values, I couldn't figure out how match them up in R)
		
		
#this data set is just Ephraim survival with Provenance climate data (i.e., seed source population climate)
	survd <- read.csv("~/Documents/Sagebrush/Manuscript Scripts and Data/Data/surv2.csv")
