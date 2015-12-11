#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for install packages for FUNCTIONS needed
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
#install and load the following packages for graphing
packagelist <- c("devtools", "survival", "ggplot2", "grid", "cowplot", 
                 "gridExtra", "scales", "doBy", "tidyr", "dplyr", "rmarkdown")

new.packages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]

if(length(new.packages)>0) {install.packages(new.packages)}

#load needed packages
  library(devtools) #allows installation of packages from github
  library(survival) #survival analysis
  library(ggplot2) #plotting graphics
  library(grid) #required to change margins in ggplot
  library(cowplot) #for multipanels ggplots
  library(gridExtra) #arrangeGrob multiple ggplots together
  library(scales) #required for ggsurv plots
  library(doBy) #used for the summaryBy function
  library(tidyr) #used for data wrangling
  library(dplyr) #used for data wrangling
  library(rmarkdown) #used to compile final report

#source custom ggsurv package from Edwin Thoen
  source_url("https://raw.githubusercontent.com/lchaney/ggsurv_m/master/ggsurv_m_with_size_parameters.R")
    #this ggsurv plot allows for custom line size
    #you can find ggsurv function in the GGally package, but this is a custom :)
    #thanks to Edwin Thoen <edwinthoen@gmail.com> for assistance!
    #more information can be found at github.com/lchaney/ggsurv_m

#==============================================================================================#

