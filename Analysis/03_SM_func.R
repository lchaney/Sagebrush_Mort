#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for the Chaney et al 2015 Sagebrush mortatlity paper
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
  #install and load required packages
  source('Analysis/03_SM_func_pack.R')
  
  #three garden survival
	source('Analysis/03a_SM_func_3gardsurv.R')

	#climate data
	source('Analysis/03b_SM_func_climate.R')

	#ephraim survival data and contemporary climate data
	source('Analysis/03c_SM_func_ephsurv.R')
  
#==============================================================================================#
