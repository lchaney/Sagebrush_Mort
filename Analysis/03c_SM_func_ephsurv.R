#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *Ephraim Survival Data* 
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#

#==============================================================================================#
#ephraim survival data

	#fit different survival regressions to determine what distribution is best
		t_exp <- survreg(Surv(time,death) ~ type, data = svdat, dist = "exponential")
		t_log <- survreg(Surv(time,death) ~ type, data = svdat, dist = "loglogistic")
		t_wei <- survreg(Surv(time,death) ~ type, data = svdat, dist = "weibull")
		t_lnorm <- survreg(Surv(time,death) ~ type, data = svdat, dist = "lognormal")
			
			s_t_exp <- summary(t_exp)$loglik
			s_t_log <- summary(t_log)$loglik
			s_t_wei <- summary(t_wei)$loglik
			s_t_lnorm <- summary(t_lnorm)$loglik
			####choose lognormal, has lowest AIC calculated by 
			######   AIC  =2logL+ 2p were p = 2
		
	#fit lognormal survival regression		
		ephsurvlogn <- survreg(Surv(time, death) ~ type, data = svdat, dist = "lognormal")
		summary_ephsurvlogn <- summary(ephsurvlogn)
			
	#Kaplien Meyer plot with survival regression curve generated

		#specify colors for each type
			typecolors <- c(T4x = "#e31a1c",
		   				    T2x = "#ff7f00",
		   				    W4x = "#33a02c",
		   				    V2x = "#1f78b4",
		   				    V4x = "#885dbc")

		#pull the predicted line from the survival regression
			#set quantile levels
			pct <- seq(0.01, 0.99,by = 0.01)
			
			predict_dat <- data.frame(surv_prop = c(1 - pct, 1 - pct, 1 - pct, 1 - pct, 1 - pct),
                           time = c(predict(ephsurvlogn, newdata = list(type = "T4x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "T2x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "W4x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "V2x"), type = "quantile", p = pct),
                                   predict(ephsurvlogn, newdata = list(type = "V4x"), type = "quantile", p = pct)),
                           group = factor(rep(1:5, each = 99)))
                           
            	max_time <- max(svdat$time)
				predict_dat <- predict_dat[predict_dat$time <= max_time ,]
				
		esurvfit <- survfit(Surv(time, death) ~ strata(type), data = svdat) 
		
		ephsurvplot_lognorm <- ggsurv_m(esurvfit, 
										lty.est = 1, 
										plot.cens = TRUE, 
										cens.col = typecolors, 
										size.est = 1, 
										size.cens = 8,
										cens.shape = 43) +
						   	    geom_line(data = predict_dat, aes(x = time, y = surv_prop), linetype = "dotdash") +
						   	    scale_color_manual(name = "Type",
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
						   		 	 legend.title = element_text(face = "italic"))
						      
		#http://stackoverflow.com/questions/9151591/how-to-plot-the-survival-curve-generated-by-survreg-package-survival-of-r
	#Thank you to Edward Theoin for ggsurv help


				
		#Use a log rank test to see if there is a difference in survival by TYPE
		svdatlrtest <- survdiff(formula = Surv(time, death) ~ type, data = svdat)
		
		#now posthoc comparisons between each type
			#how many comparisons?
			typesize <- length(unique(svdat$type))
			
			#function that will do pairwise comparisons (see here)
			#http://r.789695.n4.nabble.com/Kaplan-Meier-Post-Hoc-td4647363.html
			#http://stats.stackexchange.com/questions/36352/post-hoc-analysis-for-logrank-test
			#http://stackoverflow.com/questions/17338774/r-formula-how-to-constrain-calculations-to-two-groups-using-formula/17339707
			
			lrchisqtable <- matrix(0., typesize, typesize) 
				for (i in 1:typesize) { 
				 for (j in (1:typesize)[-i]) {
				   temp <- survdiff(Surv(time, death) ~ type, data = svdat,
				                    subset = (type %in% (unique(type))[c(i,j)]))
				   lrchisqtable[i,j] <- temp$chisq
				 }
				  } 
			rownames(lrchisqtable) <- unique(svdat$type)
			colnames(lrchisqtable) <- unique(svdat$type)
			
			#and p-values for the table
			pval_lrchisqtagible <- round(pchisq(lrchisqtable, 1, lower.tail = FALSE), 5)
			
			#need to correct for multiple comparisons now, we will use the conservative bonferroni
			newbfp <- (0.05/(((typesize - 1) * typesize)/2))

			
		#median survival: the probability of survival after ______ is 50%
		#similar to LD50
		medsurv <- survfit(Surv(time, death) ~ strata(type), data= svdat)

		#what is the probability that a plant will survive after 1 year (12mo), 2 year (24mo), 3 years (36mo), 4 years (48mo), 5 years (60mo)

		probsurv1 <- summary(esurvfit, times = seq(from = 12, to = 60, by = 12))
		probsurv2 <- summary(esurvfit, times = seq(from = 59, to = 60, by = 1))
      #column survival gives the probability of survival at each of those times
		
#==============================================================================================#
		
		
#==============================================================================================#
#ephraim survival data with climate
		
	#one way to examine proportion dead would be looking at the percentage mortality, but this is 
	#not best because a) errors are not normally distributed, b) the variance is not constant, 
	#c) response is bounded (by 1 above and by 0 below) and d) we lose information of 
	#sample size from which the proportion was estimated.
	#A better method is bind together two vectors using cbind into a single object (y)
	#comprising the numbers of successes and the number of failures.
	#Use a generalized linear model that follows the bionomial distribution
	#Check for overdispersion (residual deviance > residual degrees of freedom), and correct for it by using 
	#family=quasibinomial rather than binomial
	#Use the F test with quasibionomial to test significance
	#You can back transform from logits (z) to proportions (p) by p = 1 / (1 + 1/exp(z))
	#see Crawley for more information on proportion data
		
  #not shown is the narrowing down of this model to the two climate variables that gives the best Rsq value
		
	#model using proportion died
		modglm <- glm(cbind(surv, death) ~ sday + gspmtcm + type, data = popdat, family = "quasibinomial")
		
	#calculate GLM R squared value
		glmrsq <- function( model, ... ){
		  (1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))
		}
		
	anova(modglm, test = "F")