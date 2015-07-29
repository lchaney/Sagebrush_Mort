#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *3 garden survival plots*
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
#3 garden survival


	#fit cox ph model to use in kaplain meyer plots
	sfit_garden <- survfit(Surv(time, death) ~ strata(garden), data=surv3d)
	sfit_typeE <- survfit(Surv(time, death) ~ strata(type), data=sdat_E)
	sfit_typeM <- survfit(Surv(time, death) ~ strata(type), data=sdat_M)
	sfit_typeO <- survfit(Surv(time, death) ~ strata(type), data=sdat_O)

		#create survivorship plot for all three gardens
				#all three gardens
				gar3plot <- ggsurv_m(sfit_garden, surv.col = "black",
								   cens.col = "black", size.est = 1) +
								   scale_linetype_manual(name = "Garden",
								   		values = 2:4) +
								   guides(color = FALSE) +
								   xlim(0, 60) + ylim(0, 1) +
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   	 legend.position = c(0.1, 0.25),
								   	 legend.title = element_text(face = "italic"),
								   	 legend.background = element_rect(colour = "gray"))

				#ephraim
				ephplot <- ggsurv_m(sfit_typeE, lty.est = 2, plot.cens = FALSE, size.est = 1) +
								   scale_color_manual(name = "Ephraim",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "#e31a1c",
								   				   T2x = "#ff7f00",
								   				   W4x = "#33a02c",
								   				   V2x = "#1f78b4",
								   				   V4x = "#885dbc")) +
								   guides(linetype = FALSE) +
								   xlim(0, 60) + ylim(0, 1) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))
				#Majors
				majplot <- ggsurv_m(sfit_typeM, lty.est = 3, plot.cens = FALSE, size.est = 1) +
								   scale_color_manual(name = "Majors",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "#e31a1c",
								   				   T2x = "#ff7f00",
								   				   W4x = "#33a02c",
								   				   V2x = "#1f78b4",
								   				   V4x = "#885dbc")) +
								   guides(linetype = FALSE) +
								   xlim(0, 60) + ylim(0, 1) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))
			#Orchard
				orchplot <- ggsurv_m(sfit_typeO, lty.est = 4, plot.cens = FALSE, size.est = 1) +
								   scale_color_manual(name = "Orchards",
								   		breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
								   		values = c(T4x = "#e31a1c",
								   				   T2x = "#ff7f00",
								   				   W4x = "#33a02c",
								   				   V2x = "#1f78b4",
								   				   V4x = "#885dbc")) +
								   guides(linetype = FALSE) +
								   xlim(0, 60) + ylim(0, 1) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))

		#plot kaplain meyer plots into a 2 x 2 grids
		km22plot <- plot_grid(gar3plot, ephplot, majplot, orchplot, labels = c("A", "B", "C", "D"), ncol = 2)

	#summary of surviorship
	surv3summary <- survfit(Surv(time, death)~ strata(garden), data=surv3d)

	#sample size tables
	surv3dsample <- surv3d %>% group_by(pop, type, garden) %>% summarise(time = n()) %>% spread(garden, time)
	
	#create plot (to append to climate data on when deaths occur)
		death2 <- ggplot(data = surv3dd, aes(x = date, y = 1, color = garden)) + 
			geom_jitter(position = position_jitter(width = .5), alpha = 0.4, na.rm = TRUE) +
		    scale_x_date(limits = as.Date(c('2010-01-01','2015-05-08'))) +
		    theme_minimal() +
		    scale_y_continuous(breaks = 1) +
			labs(x = "Year", y = "Mortality") +
			scale_colour_manual(values = c("darkorange","indianred1", "steelblue"), 
								labels = c("Ephraim", "Majors Flat", "Orchard")) +
			theme(legend.title = element_blank(), 
				  plot.margin = unit(c(-2.7,0.5,0.5,0.5), "lines"), 
				  #plot.margins #top, #right, #bottom, #left
				  legend.position = "none",
		   		  axis.text.y = element_blank(),
		  		  axis.ticks.y = element_blank(),
		  		  axis.ticks.margin = unit(1.1, "lines")
		  		  )
		  		  
#==============================================================================================#