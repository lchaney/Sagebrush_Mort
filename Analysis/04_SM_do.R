#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to PRINT output needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#set working directory -- change this to where you have files saved to
	setwd("~/GitHub/Sagebrush_Mort/Analysis")

#source to LOAD data
	source("01_SM_load.R")

#source to CLEAN data
	source("02_SM_clean.R")

#source to perform FUNCTION on data
	source("03_SM_func.R")


#3 garden survival

	#print summary of survivorship
	surv3summary
	
	#print summary of 