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
	library(survival) #survival analysis
	library(ggplot2) #plotting graphics
	library(GGally) #ggsurv to plot KM plot using ggplots
	library(cowplot) #for multipanels ggplots
	library(gridExtra) #arrangeGrob multiple ggplots together
	library(scales)
	library(lme4) #used for linear mixed models
	library(lmerTest) #calculate p values for fixed and random effects from lmer
	library(plyr) #used for function rbind.fill in VIF step

#source custom ggsurv package from Edwin Thoen
	###CHANGE DIRECTORY HERE###
	source('~/GitHub/ggsurv/ggsurv_m_with_size_parameters.R')
	
#source from GIT
	source('~/GitHub/rsquared.glmm/rsquaredglmm.R')
	#https://raw.githubusercontent.com/jslefche/rsquared.glmm/master/rsquaredglmm.R
#==============================================================================================#

#==============================================================================================#
#source functions for each subpart of the analysis
	#three garden survival
	source('~/GitHub/Sagebrush_Mort/Analysis/03a_SM_func_3gardsurv.R')

	#climate data
	source('~/GitHub/Sagebrush_Mort/Analysis/03b_SM_func_climate.R')

	#ephraim survival data
	source('~/GitHub/Sagebrush_Mort/Analysis/03c_SM_func_ephsurv.R')

	#ephraim survival with contemporary climate data
	source('~/GitHub/Sagebrush_Mort/Analysis/03d_SM_func_ephsurvclim.R')

#==============================================================================================#
