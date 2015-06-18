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
	library(cowplot)
	
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
				ephplot <- ggsurv(sfit_typeE, lty.est = 2, plot.cens = FALSE) +
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
				majplot <- ggsurv(sfit_typeM, lty.est = 3, plot.cens = FALSE) +
								   scale_color_manual(name="Majors",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "red4",
								   				   T2x = "orange1",
								   				   W4x = "darkgreen",
								   				   V2x = "darkblue",
								   				   V4x = "purple4")) +
								   guides(linetype = FALSE) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25))

			#Orchard
				orchplot <- ggsurv(sfit_typeO, lty.est = 4, plot.cens = FALSE) +
								   scale_color_manual(name="Orchards",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "red4",
								   				   T2x = "orange1",
								   				   W4x = "darkgreen",
								   				   V2x = "darkblue",
								   				   V4x = "purple4")) +
								   guides(linetype = FALSE) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25))

		#plot kaplain meyer plots into a 2 x 2 grids
		km22plot <- plot_grid(gar3plot, ephplot, majplot, orchplot, labels = c("A", "B", "C", "D"), ncol = 2)

	#summary of surviorship
	surv3summary <- survfit(Surv(time, death)~ strata(garden), data=surv3d)

	#sample size tables
	surv3dsample <- with(surv3d, table(pop, type, garden))

#==============================================================================================#
