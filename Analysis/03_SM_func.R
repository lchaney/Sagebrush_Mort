#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
#packages
	#load needed packages
	library(survival)
	library(ggplot2)
	library(GGally)
	
#==============================================================================================#




#==============================================================================================#
#3 garden survival


	#fit cox ph model to use in kaplain meyer plots
	sfit_garden <- survfit(Surv(time, death)~strata(garden), data=surv3d)
	sfit_typeO <- survfit(Surv(time, death)~strata(type), data=sdat_O)
	sfit_typeM <- survfit(Surv(time, death)~strata(type), data=sdat_M)
	sfit_typeE <- survfit(Surv(time, death)~strata(type), data=sdat_E)

		#create survivorship plot for all three gardens
				#all three gardens
				gar3plot <- ggsurv(sfit_garden, surv.col = "black",
								   cens.col = "black") +
								   scale_linetype_manual(name="Garden",
								   		values = 2:4) +
								   guides(color = FALSE) +
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25))


				#ephraim
				ephplot <- ggsurv(sfit_typeO, lty.est = 2, plot.cens = FALSE) +
								   scale_color_manual(name="Ephraim",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "red4",
								   				   T2x = "orange1",
								   				   W4x = "darkgreen",
								   				   V2x = "darkblue",
								   				   V4x = "purple4")) +
								   guides(linetype = FALSE) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25))

				#Majors
				majplot <- plot(survfit(fitcph_typeM), col = 2:6, lty=3, lwd=4)
					majleg <- legend(1, 0.3, 
				       legend = levels(surv3d$type), 
				       lty = 3, 
				       col = 2:6,
				       lwd=3,
				       title = "Majors Flat garden")
				#Orchard
				orchplot <- plot(survfit(fitcph_typeO), col = 2:6, lty=4, lwd=4)
					orchleg <- legend(1, 0.3, 
				       legend = levels(surv3d$type), 
				       lty = 4, 
				       col = 2:6,
				       lwd=3,
				       title = "Orchard garden")	

	#summary of surviorship
	surv3summary <- survfit(Surv(time, death)~ strata(garden), data=surv3d)

	#sample size tables
	surv3dsample <- with(surv3d, table(pop, type, garden))

#==============================================================================================#
