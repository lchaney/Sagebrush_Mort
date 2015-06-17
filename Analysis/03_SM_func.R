#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#3 garden survival

	#loard survival package
	library(survival)
	
	#fit cox ph model to use in kaplain meyer plots
	fitcph_garden <- coxph(Surv(time, death)~strata(garden), data=surv3d)
	fitcph_typeO <- coxph(Surv(time, death)~strata(type), data=sdat_O)
	fitcph_typeM <- coxph(Surv(time, death)~strata(type), data=sdat_M)
	fitcph_typeE <- coxph(Surv(time, death)~strata(type), data=sdat_E)

		#create survivorship plot for all three gardens
				#all three gardens
				gar3plot <- plot(survfit(fitcph_garden), col = 1, lty = 2:4, lwd=4)
					gar3leg <- legend(1, 0.3, 
					       legend = levels(surv3d$garden), 
					       lty = 2:4, 
					       col = 1,
					       lwd=3,
					       title = "Survivorship by garden")
				#ephraim
				ephplot <- plot(survfit(fitcph_typeE), col = 2:6, lty=2, lwd=4)
					ephleg <- legend(1, 0.3, 
				       legend = levels(surv3d$type), 
				       lty = 2, 
				       col = 2:6,
				       lwd=3,
				       title = "Ephraim garden")
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
