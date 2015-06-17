#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used to CLEAN data needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#CLEANING UP surv3d data

	#resave type so that they appear in the following order
	surv3d$type <- factor(surv3d$type, levels = c("T4x", "T2x", "W4x", "V2x", "V4x"))

	#subset data by garden	
	sdat_O <- surv3d[which(surv3d$garden == "Orchard"),]
	sdat_M <- surv3d[which(surv3d$garden == "Majors"),]
	sdat_E <- surv3d[which(surv3d$garden == "Ephraim"),]