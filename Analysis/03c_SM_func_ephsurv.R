#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lindsay.chaney@snow.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *Ephraim Survival Data* 
# Chaney et al 2017 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
#ephraim survival data

	#fit different survival regressions to determine what distribution is best
		t_exp <- survreg(Surv(time,death) ~ type, data = sdat_E, dist = "exponential")
		t_log <- survreg(Surv(time,death) ~ type, data = sdat_E, dist = "loglogistic")
		t_wei <- survreg(Surv(time,death) ~ type, data = sdat_E, dist = "weibull")
		t_lnorm <- survreg(Surv(time,death) ~ type, data = sdat_E, dist = "lognormal")
			
			s_t_exp <- summary(t_exp)$loglik
			s_t_log <- summary(t_log)$loglik
			s_t_wei <- summary(t_wei)$loglik
			s_t_lnorm <- summary(t_lnorm)$loglik
			####choose lognormal, has lowest AIC calculated by 
			######   AIC  =2logL+ 2p were p = 2
		
	#fit lognormal survival regression		
		ephsurvlogn <- survreg(Surv(time, death) ~ type, data = sdat_E, dist = "lognormal")
		summary_ephsurvlogn <- summary(ephsurvlogn)
			
	#Kaplien Meyer plot with survival regression curve generated

		#pull the predicted line from the survival regression
			#set quantile levels
			pct <- seq(0.01, 0.99, by = 0.01)
			
			predict_dat <- data.frame(surv_prop = c(1 - pct, 1 - pct, 1 - pct, 1 - pct, 1 - pct),
                           time = c(predict(ephsurvlogn, newdata = list(type = "T4x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "T2x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "W4x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "V2x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "V4x"), type = "quantile", p = pct)),
                           group = factor(rep(1:5, each = 99)))
                           
            	max_time <- max(sdat_E$time)
				predict_dat <- predict_dat[predict_dat$time <= max_time ,]
				
		esurvfit <- survfit(Surv(time, death) ~ strata(type), data = sdat_E) 
		
		ephsurvplot_lognorm <- ggsurv_m(esurvfit, 
										lty.est = 1, 
										plot.cens = TRUE, 
										cens.col = c(T4x = T4xcol,
										             T2x = T2xcol,
										             W4x = W4xcol,
										             V2x = V2xcol,
										             V4x = V4xcol), 
										size.est = 1, 
										size.cens = 8,
										cens.shape = 43) +
						   	    geom_line(data = predict_dat, aes(x = time, y = surv_prop), linetype = "dotdash") +
						   	    scale_color_manual(name = "Type",
						   				breaks = c("T4x", "T2x", "W4x", "V2x", "V4x"),
						   				values = c(T4x = T4xcol,
						   				   T2x = T2xcol,
						   				   W4x = W4xcol,
						   				   V2x = V2xcol,
						   				   V4x = V4xcol)) +
						   	   guides(linetype = FALSE) +
						   	   xlim(0, 60) + ylim(0, 1) + 
						   	   theme_minimal() +
						   	   theme(axis.line = element_line(color = "black", size = 0.25),
						   		 	 legend.title = element_text(face = "italic"))
						      
		#http://stackoverflow.com/questions/9151591/how-to-plot-the-survival-curve-generated-by-survreg-package-survival-of-r
	#Thank you to Edward Theoin for ggsurv help


				
		#Use a log rank test to see if there is a difference in survival by TYPE
		sdat_Elrtest <- survdiff(formula = Surv(time, death) ~ type, data = sdat_E)
		
		#now posthoc comparisons between each type
			#how many comparisons?
			typesize <- length(unique(sdat_E$type))
			
			#function that will do pairwise comparisons (see here)
			#http://r.789695.n4.nabble.com/Kaplan-Meier-Post-Hoc-td4647363.html
			#http://stats.stackexchange.com/questions/36352/post-hoc-analysis-for-logrank-test
			#http://stackoverflow.com/questions/17338774/r-formula-how-to-constrain-calculations-to-two-groups-using-formula/17339707
			
			lrchisqtable <- matrix(0., typesize, typesize) 
				for (i in 1:typesize) { 
				 for (j in (1:typesize)[-i]) {
				   temp <- survdiff(Surv(time, death) ~ type, data = sdat_E,
				                    subset = (type %in% (unique(type))[c(i,j)]))
				   lrchisqtable[i,j] <- temp$chisq
				 }
				  } 
			rownames(lrchisqtable) <- unique(sdat_E$type)
			colnames(lrchisqtable) <- unique(sdat_E$type)
			
			lrchisqtable[lower.tri(lrchisqtable, diag = TRUE)] <- NA
			
			#and p-values for the table
			pval_lrchisqtagible <- round(pchisq(lrchisqtable, 1, lower.tail = FALSE), 5)
			pval_lrchisqtagiblefdr <- round(p.adjust(pchisq(lrchisqtable, 1, lower.tail = FALSE), method = "fdr"), 5)
			pval_lrchisqtagiblebonf <- round(p.adjust(pchisq(lrchisqtable, 1, lower.tail = FALSE), method = "bonferroni"), 5)
		
						pval_lrchisqtagiblebonf <- matrix(pval_lrchisqtagiblebonf, nrow = typesize, ncol = typesize)
						rownames(pval_lrchisqtagiblebonf) <- unique(sdat_E$type)
						colnames(pval_lrchisqtagiblebonf) <- unique(sdat_E$type)
						 
		#median survival: the probability of survival after ______ is 50%
		#similar to LD50
		medsurv <- esurvfit
		
		#what is the probability that a plant will survive after 1 year (12mo), 2 year (24mo), 3 years (36mo), 4 years (48mo), 5 years (60mo)

		probsurv <- summary(esurvfit, times = c(12, 24, 36, 48, 59))
      #column survival gives the probability of survival at each of those times
		
		#population level differences
		poplrtest <- survdiff(formula = Surv(time, death) ~ pop, data = sdat_E)
		popmedsurv <- survfit(Surv(time, death) ~ strata(pop), data = sdat_E) 
		popprobsurv <- summary(popmedsurv, times = c(12, 24, 36, 48, 59))
		
#==============================================================================================#