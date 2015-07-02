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
	library(GGally) #might not need this one if you use ggsurv_m instead
	library(cowplot)
	library(gridExtra)
	library(scales)
	library(lme4) #used for linear mixed models
	library(lmerTest) #calculate p values for fixed and random effects from lmer
	library(plyr) #used for function rbind.fill in VIF step

#source custom ggsurv package from Edwin Thoen
	###CHANGE DIRECTORY HERE###
	source('~/GitHub/ggsurv/ggsurv_m_with_size_parameters.R')
	
#source from GIT
	source('~/GitHub/rsquared.glmm/rsquaredglmm.R')
	https://raw.githubusercontent.com/jslefche/rsquared.glmm/master/rsquaredglmm.R
#==============================================================================================#

#==============================================================================================#
#3 garden survival


	#fit cox ph model to use in kaplain meyer plots
	sfit_garden <- survfit(Surv(time, death)~strata(garden), data=surv3d)
	sfit_typeE <- survfit(Surv(time, death)~strata(type), data=sdat_E)
	sfit_typeM <- survfit(Surv(time, death)~strata(type), data=sdat_M)
	sfit_typeO <- survfit(Surv(time, death)~strata(type), data=sdat_O)

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
				ephplot <- ggsurv_m(sfit_typeE, lty.est = 2, plot.cens = FALSE, size.est = 1) +
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
				majplot <- ggsurv_m(sfit_typeM, lty.est = 3, plot.cens = FALSE, size.est = 1) +
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
				orchplot <- ggsurv_m(sfit_typeO, lty.est = 4, plot.cens = FALSE, size.est = 1) +
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
		ephsurvlogn <- survreg(Surv(time, death)~type, data=svd, dist="lognormal")
		summary_ephsurvlogn <- summary(sx)
			
	#Kaplien Meyer plot with survival regression curve generated

		#specify colors for each type
			typecolors <- c(T4x = "#e31a1c",
		   				    T2x = "#ff7f00",
		   				    W4x = "#33a02c",
		   				    V2x = "#1f78b4",
		   				    V4x = "#885dbc")
				
		#reminder from above: sfit_typeE <- survfit(Surv(time, death)~strata(type), data=sdat_E)
		ephsurvplot_lognorm <- ggsurv_m(sfit_typeE, 
										lty.est = 1, 
										plot.cens = TRUE, 
										cens.col = typecolors, 
										size.est = 1, 
										size.cens = 8,
										cens.shape = 43) +
						   	   scale_color_manual(name="Type",
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
						   		 	 legend.title = element_text(face = "italic")) +
						       stat_smooth(method=lm, se=TRUE, linetype=4)

	#NEED TO UPDATE THIS!!!
		    ephsurvlogn <- survreg(Surv(time, death)~type, data=svd, dist="lognormal")
    			
    			pct <- seq(.01,.99,by=.01)
			    
			    lines(predict(sl, newdata = list(type = "T4x"),
			    	  type = "quantile", p = pct), 1-pct, col="#e31a1c", lty=4)
			    
			    lines(predict(sl, newdata=list(type="T2x"),
			    	  type = "quantile", p = pct), 1-pct, col="#ff7f00", lty=4)
			    
			    lines(predict(sl, newdata=list(type="W4x"),
			    	  type = "quantile", p = pct), 1-pct, col="#33a02c", lty=4)
			    
			    lines(predict(sl, newdata=list(type="V2x"),
			    	  type = "quantile", p = pct), 1-pct, col="#1f78b4", lty=4)
			    
			    lines(predict(sl, newdata=list(type="V4x"),
			    	  type = "quantile", p = pct), 1-pct, col="#885dbc", lty=4)
			
		#http://stackoverflow.com/questions/9151591/how-to-plot-the-survival-curve-generated-by-survreg-package-survival-of-r
	#NEED TO UPDATE THIS!!!	^^^^^^^


				
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
			pval_lrchisqtagible <- round(pchisq(lrchisqtable, 1, lower.tail=FALSE), 5)
			
			#need to correct for multiple comparisons now, we will use the conservative bonferroni
			newbfp <- (0.05 / (((typesize-1)*typesize)/2))

			
		#median survival: the probability of survival after ______ is 50%
		#similar to LD50
		medsurv <- survfit(Surv(time, death)~ strata(type), data= svd)

		#what is the probability that a plant will survive after 1 year (12mo), 2 year (24mo), 3 years (36mo), 4 years (48mo), 5 years (60mo)

		probsurv1 <- summary(s_sx, times=seq(from=12, to=60, by=12))
		probsurv2 <- summary(s_sx, times=seq(from=58, to=60, by=1))

		#column survival gives the probability of survival at each of those times
		
#==============================================================================================#



#==============================================================================================#
#ephraim survival data with climate


#===========================================================================================

##USING STEPWISE REMOVAL BASED ON VIC (CODE FROM http://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/)

####################################################################################
####################################################################################

library(plyr)


modmer <- lmer(timedeath ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + (1|type) + (1|type:pop), data= svd)
#needed to remove mtwm due to linear combinations
#removed using findLinearCombos {caret}

 vif.mer <- function (fit) {
    ## adapted from rms::vif

    v <- vcov(fit)
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }

    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}

cutoff <- 2

flag = TRUE
viftable = data.frame()
while(flag==TRUE){
	vfit=vif.mer(modmer)
	viftable=rbind.fill(viftable, as.data.frame(t(vfit)))
	if(max(vfit)>cutoff) { modmer=
		update(modmer, as.formula(paste(".", "~", ".", "-", names(which.max(vfit)))))}
	else {flag=FALSE}
}

print(viftable)
print(vfit)
print(modmer)

#sdimindd0
#tdiff
#smrpb
#winp
#sdimtcm
#dd0map

modlm <- lmer(timedeath ~ sdimindd0 + tdiff + smrpb + winp + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove smrpb
modlm2 <- lmer(timedeath ~ sdimindd0 + tdiff + winp + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove winp
modlm3 <- lmer(timedeath ~ sdimindd0 + tdiff + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove tdiff
modlm4 <- lmer(timedeath ~ sdimindd0 + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove dd0map
modlm5 <- lmer(timedeath ~ sdimindd0 + sdimtcm + (1|type) + (1|type:pop), data= svd)
rand(modlm5)
anova(modlm5)

rsquared.glmm(modlm5)
#conditional is variance explained by fixed plus random effects
#marginal is variance explained by fixed effects
 

#can't plot well due to all the NA's
svd1 <- na.omit(svd)
### GRAPH MODEL
ggplot(svd1, 
			aes(y = fitted(modlm5), 
				x = fitted(modlm5) + residuals(modlm5), 
				color=factor(ssp), 
				shape=factor(type)
				),
			size=3) + 
			ylim(min(fitted(modlm5)), max(fitted(modlm5))) + 
			xlim(min(fitted(modlm5) + residuals(modlm5)), 
			max(fitted(modlm5) + residuals(modlm5))) + 
			theme_bw() + scale_shape(solid=TRUE) + 
			stat_smooth(method=lm, se=FALSE, linetype=4) + 
			stat_summary(fun.y = mean, fun.ymin = "sd", fun.ymax = "sd") + 
			labs(x = "Observed", y = "Predicted", color="SubSpecies", shape="Ploidy")
