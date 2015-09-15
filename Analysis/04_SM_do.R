#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to source all files needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
# Set working directory and source LOAD, CLEAN, and FUNCTION scripts

	#source to LOAD data
		source('Analysis/01_SM_load.R')

	#source to CLEAN data
		source('Analysis/02_SM_clean.R')

	#source to perform FUNCTION on data
		source('Analysis/03_SM_func.R')

#==============================================================================================#