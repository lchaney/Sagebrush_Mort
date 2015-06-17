#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to PRINT output needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
# Set working directory and source LOAD, CLEAN, and FUNCTION scripts

	#set working directory -- change this to where you have files saved to
		setwd("~/Documents/Sagebrush")
	
	#source to LOAD data
#		source("01_SM_load.R")
		source('~/GitHub/Sagebrush_Mort/Analysis/01_SM_load.R')
	
	#source to CLEAN data
#		source("02_SM_clean.R")
		source('~/GitHub/Sagebrush_Mort/Analysis/02_SM_clean.R')
	
	#source to perform FUNCTION on data
#		source("03_SM_func.R")
		source('~/GitHub/Sagebrush_Mort/Analysis/03_SM_func.R')
#==============================================================================================#




#==============================================================================================#
# Printing output for 3 garden survival
	
	#print 2 by 2 kaplain meyer plot for all 3 gardens
		#change parameters for a 2 by 2 plot
		par(mfrow=c(2,2))

			#all three gardens
			gar3plot
			gar3leg
			
			#ephraim
			ephplot
			ephleg
			
			#majors
			majplot
			majleg
			
			#orchard
			orchplot
			orchleg			
			
		#reset parameters back to 1 by 1
		par(mfrow=c(1,1))
	
	#save above plot to figures folder
	

	#print summary of survivorship
	surv3summary
	
	#print sample sizes tables for all 3 gardens
	surv3dsample

#==============================================================================================#
	
 