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
				   		  legend.position=c(0.93, 0.9),
				   		  axis.text.x = element_blank(),
				   		  axis.ticks.x = element_blank())

			#merge the death plot and the climate plot

			deathclim <- arrangeGrob(clim_graph, death2, ncol=1, heights=c(4, 0.9))


	#compare minimumn temperatures by garden
		climate$EOdif <- climate$Orch_min - climate$Eph_min
		climate$EMdif <- climate$Maj_min - climate$Eph_min

			sort(climate$EOdif)
				#28.5
			sort(climate$EMdif)
				#maj min -2.2222
				#eph min -20.8
				#18.57777778
				#date 2013-12-18

	mintempsmonth <- summaryBy(Eph_min + Orch_min + Maj_min ~ Year + Month, data=climate, FUN=min, na.rm=TRUE)
		mintempsmonth$EOdif <- mintempsmonth$Orch_min.min - mintempsmonth$Eph_min.min
		mintempsmonth$EMdif <- mintempsmonth$Maj_min.min - mintempsmonth$Eph_min.min


	mintempsyear <- summaryBy(Eph_min + Orch_min + Maj_min ~ Year, data=climate, FUN=min, na.rm=TRUE)
		mintempsyear$EOdif <- mintempsyear$Orch_min.min - mintempsyear$Eph_min.min
		mintempsyear$EMdif <- mintempsyear$Maj_min.min - mintempsyear$Eph_min.min

		library(reshape)
		mtm <- melt(mintempsmonth[,1:5], id = c("Year", "Month"))
		is.na(mtm) <- do.call(cbind,lapply(mtm, is.infinite))

		fit <- aov(value ~ variable, data = mtm)

		clim <- melt(climate[,c(1:2, 6, 10, 13)], id = c("Year", "Month"))
		fit1 <- aov(value ~ variable, data = clim)

		clim2 <- clim[ which(clim$Month >= 11 | clim$Month <= 2), ]
    names(clim2) <- c("Year", "Month", "Garden", "MinTemp")
		fit2 <- aov(MinTemp ~ Garden * Year, data = clim2)
		summary(fit2)
		ggplot(aes(y = MinTemp, x = Garden), data = clim2) +
		  geom_violin() + stat_smooth(method = "lm", col = "red") + facet_grid( ~Year)
#==============================================================================================#
