#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for the Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
#packages
	###you will need to make sure these parackages are installed first###
	#load needed packages
	library(survival)
	library(ggplot2)
	#library(GGally)
	library(cowplot)
	library(gridExtra)
	library(scales)


#source custom ggsurv package from Edwin Thoen
	###CHANGE DIRECTORY HERE###
	source('~/GitHub/ggsurv/ggsurv_m_with_size_parameters.R', chdir = TRUE)
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
				gar3plot <- ggsurv_m(sfit_garden, surv.col = "black",
								   cens.col = "black", size.est=1) +
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
				ephplot <- ggsurv_m(sfit_typeE, lty.est = 2, plot.cens = FALSE, size.est=1) +
								   scale_color_manual(name="Ephraim",
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
				majplot <- ggsurv_m(sfit_typeM, lty.est = 3, plot.cens = FALSE, size.est=1) +
								   scale_color_manual(name="Majors",
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
				orchplot <- ggsurv_m(sfit_typeO, lty.est = 4, plot.cens = FALSE, size.est=1) +
								   scale_color_manual(name="Orchards",
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
				   		  legend.position=c(0.93, 0.9),
				   		  axis.text.x = element_blank(),
				   		  axis.ticks.x = element_blank())
				   	
			#merge the death plot and the climate plot			
			
			deathclim <- arrangeGrob(clim_graph, death2, ncol=1, heights=c(4, 0.9))

#==============================================================================================#



#==============================================================================================#
#ephraim survival data

	#fit different survival regressions to determine what distribution is best
		t_exp <- survreg(Surv(time,death) ~ type, data=svd, dist="exponential")
		t_log <- survreg(Surv(time,death) ~ type, data=svd, dist="loglogistic")
		t_wei <- survreg(Surv(time,death) ~ type, data=svd, dist="weibull")
		t_lnorm <- survreg(Surv(time,death) ~ type, data=svd, dist="lognormal")
			
			s_t_exp <- summary(t_exp)$loglik
			s_t_log <- summary(t_log)$loglik
			s_t_wei <- summary(t_wei)$loglik
			s_t_lnorm <- summary(t_lnorm)$loglik
			####choose lognormal, has lowest AIC calculated by 
			######   AIC  =2logL+ 2p were p = 2
		
	#fit lognormal survival regression		
		sx <- survreg(Surv(time, death)~type, data=svd, dist="lognormal")
		s_sx <- summary(sx)
		
#NEED TO UPDATE THIS!!!
		
	#Kaplien Meyer plot with survival regression curve generated
				fitcph_type <- coxph(Surv(time, death)~strata(type), data=svd)
				plot(survfit(fitcph_type), col = 1:5)
				legend(1, 0.3, 
				       legend = levels(svd$type), 
				       lty = 1, 
				       col = 1:5,
				       title = "Survivorship by type")
		    sl <- survreg(Surv(time, death)~type, data=svd, dist="lognormal")
    
			    lines(predict(sl, newdata=list(type="T4x"),
			    	  type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col=1, lty=4)
			    
			    lines(predict(sl, newdata=list(type="T2x"),
			    	  type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col=2, lty=4)
			    
			    lines(predict(sl, newdata=list(type="W4x"),
			    	  type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col=3, lty=4)
			    
			    lines(predict(sl, newdata=list(type="V2x"),
			    	  type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col=4, lty=4)
			    
			    lines(predict(sl, newdata=list(type="V4x"),
			    	  type="quantile",p=seq(.01,.99,by=.01)),seq(.99,.01,by=-.01),col=5, lty=4)
			
			#http://stackoverflow.com/questions/9151591/how-to-plot-the-survival-curve-generated-by-survreg-package-survival-of-r
			
		#Use a log rank test to see if there is a difference in survival by TYPE
		svdlrtest <- survdiff(formula = Surv(time, death) ~ type, data = svd)
		
		#now posthoc comparisons between each type
			#how many comparisons?
			typesize <- length(unique(svd$type))
			
			#function that will do pairwise comparisons (see here)
			#http://r.789695.n4.nabble.com/Kaplan-Meier-Post-Hoc-td4647363.html
			#http://stats.stackexchange.com/questions/36352/post-hoc-analysis-for-logrank-test
			#http://stackoverflow.com/questions/17338774/r-formula-how-to-constrain-calculations-to-two-groups-using-formula/17339707
			
			lrchisqtable <- matrix(0., typesize, typesize) 
				for (i in 1: typesize) { 
				    for (j in (1: typesize)[-i]) { 
				          temp <- survdiff(Surv(time, death) ~ type, data=svd, 
				                             subset=(type %in% (unique(type))[c(i,j)])) 
				         lrchisqtable[i,j] <- temp$chisq 
				          } 
				     } 
			rownames(lrchisqtable) <- unique(svd$type)
			colnames(lrchisqtable) <- unique(svd$type)
			
			#and p-values for the table
			pval_lrchisqtable <- round(pchisq(lrchisqtable, 1, lower.tail=FALSE), 5)
			
			#need to correct for multiple comparisons now, we will use the conservative bonferroni
			newbfp <- (0.05 / (10))
				#replace 10 for how many pairwise comparisons you have
				#not sure how to quickly automate that for typesize
			
			
		#median survival: the probability of survival after ______ is 50%
		#similar to LD50
		medsurv <- survfit(Surv(time, death)~ strata(type), data= svd)

		#what is the probability that a plant will survive after 1 year (12mo), 2 year (24mo), 3 years (36mo), 4 years (48mo), 5 years (60mo)

		probsurv1 <- summary(s_sx, times=seq(from=12, to=60, by=12))
		probsurv2 <- summary(s_sx, times=seq(from=58, to=60, by=1))

		#column survival gives the probability of survival at each of those times 