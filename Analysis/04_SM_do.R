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
	km22plot
	
			#or individual km plots
			#gar3plot
			#ephplot
			#majplot
			#orchplot
	
	#save above plot to figures folder
	save_plot("~/Documents/Sagebrush/Manuscript Figures/SF1.png", km22plot,
          ncol = 2,
          nrow = 2,
          base_aspect_ratio = 2)

	#print summary of survivorship
	surv3summary
	
	#print sample sizes tables for all 3 gardens
	surv3dsample

#==============================================================================================#
	
#==============================================================================================#
# Printing output for climate
	
	#print death and climate plot for all 3 gardens
	deathclim
	
		#or individual graphs
		#death2
		#clim_graph
	
	#save above plot to figures folder
	save_plot("~/Documents/Sagebrush/Manuscript Figures/climgraph.png", deathclim, base_aspect_ratio = 2)

#==============================================================================================#



#==============================================================================================#
#Ephraim survival data

	#print survival regression (Table 2)
	s_sx
	
	#print kaplien meyer plot with survival regression curves (Figure 1)
	
	
	#save above plot to figures folder
	#fig1
	
	#Print log rank test for survival curves
	svdlrtest
	
	#print posthoc pairwise comparison table of chisquare log-rank test 
	lrchisqtable
	
		#and associated p-values
		pval_lrchisqtable
		
		#new bonferoni corrected p-value for significane
		newbfp
		
	#median survival
	medsurv
		
	#probability of survival at different months -- column survival gives you those times 
	probsurv1
	probsurv2	
		