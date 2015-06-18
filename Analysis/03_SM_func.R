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
	library(gridExtra)
	library(scales)
	
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
								   xlim(0, 60) + ylim(0, 1) +
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))

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
								   xlim(0, 60) + ylim(0, 1) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))
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
								   xlim(0, 60) + ylim(0, 1) + 
								   theme_minimal() +
								   theme(axis.line = element_line(color = "black", size = .25),
								   		 legend.position = c(0.1, 0.25),
								   		 legend.title = element_text(face = "italic"),
								   		 legend.background = element_rect(colour = "gray"))
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
	surv3dsample <- with(surv3d, table(pop, type, garden))

	
	#create plot (to append to climate data on when deaths occur)
		death2 <- ggplot(data=surv3dd, aes(x=date, y=1, color= garden)) + 
			geom_jitter(position = position_jitter(width = .5), alpha=.4, na.rm=TRUE) +
		    scale_x_date(limits = as.Date(c('2010-01-01','2015-05-08'))) +
		    theme_minimal() +
		    scale_y_continuous(breaks = 1) +
			labs(x="Year", y="Mortality") +
			scale_colour_manual(values = c("darkorange","indianred1", "steelblue"), 
								labels=c("Ephraim", "Majors Flat", "Orchard")) +
			theme(legend.title=element_blank(), 
				  plot.margin = unit(c(-2.7,0.5,0.5,0.5), "lines"), 
				  #plot.margins #top, #right, #bottom, #left
				  legend.position="none",
		   		  axis.text.y = element_blank(),
		  		  axis.ticks.y = element_blank(),
		  		  axis.ticks.margin = unit(1.1, "lines")
		  		  )


#==============================================================================================#



#==============================================================================================#
#climate data

	#Calculate 30 day moving average
		## USE THIS TO DETERMINE HOW BIG OF A WINDOW TO USE TO CALCULATE MOVING AVERAGE
		fx4 <- rep(1/30, 30) 	

			#Ephraim Daily mins
				E30MAM <- filter(na.omit(climate$Eph_min), fx4, sides=2)
		
			#Orchard Daily mins
				O30MAM <- filter(na.omit(climate$Orch_min), fx4, sides=2)
		
			#Major Daily mins
				M30MAM <- filter(na.omit(climate$Maj_min), fx4, sides=2)
		#I couldn't figure out how to add these columns to the data frame 
		#and allign the missing values so I did that part in excel 
		#(e.g. 	write.csv(E30MAM, "sym.csv"))
		
			#create chart with minimum daily temperatures for the three gardens
			#in bold is the 30 day moving average for daily minimum temperatures 
			clim_graph <- ggplot(data=climate, aes(x=Date))+
				   labs(x= "", y="Temperature")+
				   theme_minimal()+
				   geom_line(aes(y= Eph_min), color='darkorange',  size=.5, alpha=0.3)+
				   geom_line(aes(y= E30MovAvgMin, color='EMA'), size=.5)+
				   geom_line(aes(y= Maj_min), color='indianred1', size=.5, alpha=0.3)+
				   geom_line(aes(y= M30MovAvgMin, color='MMA'), size=.5)+	   
				   geom_line(aes(y= Orch_min), color='steelblue', size=.5, alpha=0.3)+
				   geom_line(aes(y= O30MovAvgMin, color='OMA'), size=.5)+
				   scale_colour_manual(name="Legend",
				   		values=c(EMA="darkorange", 
				   				 MMA="indianred1",
				   				 OMA="steelblue"),
				   		labels=c("Ephraim",
				   				 "Majors Flat", 
				   				 "Orchard")) +
				   	theme(legend.title=element_blank(), 
				   		  plot.margin = unit(c(0.5,0.5,-1,0.5), "lines"), 
				   		  #plot.margins #top, #right, #bottom, #left
				   		  legend.position=c(0.95,0.15),
				   		  axis.text.x = element_blank(),
				   		  axis.ticks.x = element_blank())
				   	
			#merge the death plot and the climate plot			
			
			deathclim <- grid.arrange(clim_graph, death2, ncol=1, heights=c(4, 0.9))	