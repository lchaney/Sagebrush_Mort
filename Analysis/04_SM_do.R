#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lindsay.chaney@snow.edu
# Script created in version R 3.1.3 
# This script is used to source all files needed for the Chaney et al 2017 Sagebrush mortatlity paper
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
		
		
#==============================================================================================#		
#save figures in output	folder
		
		#create Output folder
		if(!file.exists("Output"))
		  dir.create("Output")
		
		#Figure 1
		#Kaplien-Meir survivorship curves by type (subspecies:ploidy) in big sagebrush grown in Eprhaim
		  ggsave("Output/Fig1.png", ephsurvplot_lognorm)
		
		#Supplementary Figure 2
		#Kaplien-Meir survivorship curves for big sagebrush by (a) common garden and survivor ship by type (subspecies:ploidy) for the (b) Ephraim, (c) Majors Flat and (d) Orchards garden.
		  save_plot("Output/SuplFig2.png", km22plot,
		           ncol = 2,
		           nrow = 2,
		           base_aspect_ratio = 2)		
		
		#Supplementary Figure 3
		#Interaction plot of genetics and environment for each population.
		  ggsave("Output/SuplFig3.png", int_plot)
		
		#Supplementary Figure 4
		#Top panel shows minimum daily temperatures (light lines) with 30 day moving averages (bold lines) for the three garden locations during the experimental time period. 
		  #this is not currently supported, but should be soon.
		  #see https://github.com/baptiste/gridextra/issues/13
		  #A hack to get around this with ggplot2 v1.0.1, but I will leave this commented out for now
		  #ggsave <- ggplot2::ggsave; body(ggsave) <- body(ggplot2::ggsave)[-2];
		  
		  #save_plot("Output/SuplFig4.png", deathclim, base_aspect_ratio = 2)
		  

#==============================================================================================#	