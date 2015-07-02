#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *Ephraim Survival Data* 
# Chaney et al 2015 Sagebrush mortatlity paper
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