#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3
# This script is used for FUNCTIONS needed for *Climate*
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
#climate data

	#Calculate 30 day moving average
		## USE THIS TO DETERMINE HOW BIG OF A WINDOW TO USE TO CALCULATE MOVING AVERAGE
		fx4 <- rep(1/30, 30)

			#Ephraim Daily mins
				E30MAM <- stats::filter(na.omit(climate$Eph_min), fx4, sides=2)

			#Orchard Daily mins
				O30MAM <- stats::filter(na.omit(climate$Orch_min), fx4, sides = 2)

			#Major Daily mins
				M30MAM <- stats::filter(na.omit(climate$Maj_min), fx4, sides = 2)
		#I couldn't figure out how to add these columns to the data frame
		#and allign the missing values so I did that part in excel
		#(e.g. 	write.csv(E30MAM, "sym.csv"))

			#create chart with minimum daily temperatures for the three gardens
			#in bold is the 30 day moving average for daily minimum temperatures
			clim_graph <- ggplot(data = climate, aes(x = Date)) +
			  labs(x = "", y = "Temperature") +
			  theme_minimal() +
			  geom_line(aes(y = Eph_min), color = ephcol,  size = 0.5, alpha = 0.3) +
			  geom_line(aes(y = E30MovAvgMin, color = 'EMA'), size = 0.7) +
			  geom_line(aes(y = Maj_min), color = majcol, size = 0.5, alpha = 0.3) +
			  geom_line(aes(y = M30MovAvgMin, color = 'MMA'), size = 0.55) +
			  geom_line(aes(y = Orch_min), color = orchcol, size = 0.5, alpha = 0.3) +
			  geom_line(aes(y = O30MovAvgMin, color = 'OMA'), size = 0.55) +
			  scale_colour_manual(name = "Legend",
			                      values = c(EMA = ephcol,
			                                 MMA = majcol,
			                                 OMA = orchcol),
			                      labels = c("Ephraim",
			                                 "Majors Flat",
			                                 "Orchard")) +
			  theme(legend.title = element_blank(),
			        plot.margin = unit(c(0.5, 0.5, -1, 0.5), "lines"),
			        #plot.margins #top, #right, #bottom, #left
			        legend.position = c(0.93, 0.9),
			        axis.text.x = element_blank(),
			        axis.ticks.x = element_blank())
			#merge the death plot and the climate plot

			deathclim <- grid.arrange(clim_graph, death2, ncol = 1, heights = c(4, 0.9))

	#filter for just during study period
	climaterange <- filter(climate, Date >= as.Date("2010-04-15 00:00:00"), Date < as.Date("2015-05-08 00:00:00"))
			
	#compare minimumn temperatures by garden
	mintempsmonth <- summaryBy(Eph_min + Maj_min + Orch_min ~ Year + Month, data = climaterange, FUN = min, na.rm = TRUE)
	is.na(mintempsmonth) <- do.call(cbind,lapply(mintempsmonth, is.infinite))

	winterclimaterange <- filter(climaterange, Month < "3", Month > "11") #December, Jan and Feb
	max(winterclimaterange$Maj_min - winterclimaterange$Eph_min, na.rm = TRUE)
	mean(winterclimaterange$Maj_min - winterclimaterange$Eph_min, na.rm = TRUE)
	max(winterclimaterange$Orch_min - winterclimaterange$Eph_min, na.rm = TRUE)
	mean(winterclimaterange$Orch_min - winterclimaterange$Eph_min, na.rm = TRUE)
	
	#compare average temperatures by garden
	tempsmonth <- summaryBy(Eph_avg + Maj_avg + Orch_avg ~ Year + Month, data = climaterange, FUN = mean, na.rm = TRUE)

	mean(climaterange$Orch_avg - climaterange$Eph_avg, na.rm = TRUE)
	mean(climaterange$Orch_avg - climaterange$Maj_avg, na.rm = TRUE)
	
	#compare maximum temperatures by garden
	maxtempsmonth <- summaryBy(Eph_max + Maj_max + Orch_max ~ Year + Month, data = climaterange, FUN = max, na.rm = TRUE)
	is.na(maxtempsmonth) <- do.call(cbind,lapply(maxtempsmonth, is.infinite))
	
#==============================================================================================#
