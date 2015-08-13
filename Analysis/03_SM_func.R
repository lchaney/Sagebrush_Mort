#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#=== change source file location ===#

#==============================================================================================#
#packages
	###you will need to make sure these parackages are installed first###
	#load needed packages
  library(devtools) #allows installation of packages from github
  library(survival) #survival analysis
	library(ggplot2) #plotting graphics
  library(grid) #required to change margins in ggplot
#	library(GGally) #ggsurv to plot KM plot using ggplots
	library(cowplot) #for multipanels ggplots
	library(gridExtra) #arrangeGrob multiple ggplots together
	library(scales) #required for ggsurv plots
  library(doBy) #used for the summaryBy function
  library(tidyr) #used for data wrangling
  library(dplyr) #used for data wrangling


#	library(lme4) #used for linear mixed models
#	library(lmerTest) #calculate p values for fixed and random effects from lmer
#	library(plyr) #used for function rbind.fill in VIF step

#source custom ggsurv package from Edwin Thoen
  source_url("https://raw.githubusercontent.com/lchaney/ggsurv_m/master/ggsurv_m_with_size_parameters.R")
  #this ggsurv plot allows for custom line size
		#you can find ggsurv function in the GGally library, but this is a custom :)
	  #thanks to Edwin Thoen <edwinthoen@gmail.com> for assistance!
    #more information can be found at github.com/lchaney/ggsurv_m

#==============================================================================================#

  
#==============================================================================================#
  #set 'global' colors for garden and subspecies
  ephcol <- "#FF0000"
  majcol <- "#680BAB"
  orchcol <- "#1B979F"
  
  T4xcol = "#e31a1c"
  T2xcol = "#ff7f00"
  W4xcol = "#33a02c"
  V2xcol = "#1f78b4"
  V4xcol = "#885dbc"
#==============================================================================================#
  
  
#==============================================================================================#
#source functions for each subpart of the analysis
	#three garden survival
	source('Analysis/03a_SM_func_3gardsurv.R')

	#climate data
	source('Analysis/03b_SM_func_climate.R')

	#ephraim survival data and contemporary climate data
	source('Analysis/03c_SM_func_ephsurv.R')
  
#==============================================================================================#
