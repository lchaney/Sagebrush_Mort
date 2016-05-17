#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *3 garden survival plots*
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
#3 garden glm
  
  #glm for the 3 gardens
  fit3 <- glm(cbind(surv, death) ~ pop  + garden + pop:garden, 
              data = surv3counts, family = "quasibinomial")

  aovfit3 <- anova(fit3, test = "F")
  
    #restructure data for plotting
    s3c <- surv3counts %>% ungroup() %>% arrange(garden, propdead)
      s33c <- as.data.frame(s3c)
      
      #create a dummie variable with pop as a number for coloring purposes - ordered by mortality in Ephraim
      s33c$popnum <- as.integer(with(s33c, factor(pop, 
                                      levels = pop[order(garden, propdead)], 
                                      ordered = TRUE)))


      #interaction plot
      int_plot <- ggplot(data = s33c, 
             aes(x = garden, y = 1 - propdead, group = pop, color = popnum)) + 
        stat_summary(fun.y = mean, na.rm = TRUE, geom = "line") + 
        scale_color_gradientn(colours = c("red","violet","blue")) + 
        theme(legend.position = "none") + 
        labs(x = "Garden", y = "Survival")

#==============================================================================================#


#==============================================================================================#
#3 garden survival analysis

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
								   		values = c(T4x = T4xcol,
								   				   T2x = T2xcol,
								   				   W4x = W4xcol,
								   				   V2x = V2xcol,
								   				   V4x = V4xcol)) +
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
								   		values = c(T4x = T4xcol,
								   		           T2x = T2xcol,
								   		           W4x = W4xcol,
								   		           V2x = V2xcol,
								   		           V4x = V4xcol)) +
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
								   		values = c(T4x = T4xcol,
								   		           T2x = T2xcol,
								   		           W4x = W4xcol,
								   		           V2x = V2xcol,
								   		           V4x = V4xcol)) +
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
	surv3summary <- survfit(Surv(time, death) ~ strata(garden), data = surv3d)

	#logrank test to test for differences in garden
	gardenlrtest <- survdiff(formula = Surv(time, death) ~ garden, data = surv3d)

		#now posthoc comparisons between each garden
	  #how many comparisons?
	  gardensize <- length(unique(surv3d$garden))
	
	  #function that will do pairwise comparisons (see here)
	  gardenlrchisqtable <- matrix(0., gardensize, gardensize) 
	  for (i in 1:gardensize) { 
	    for (j in (1:gardensize)[-i]) {
	      temp <- survdiff(Surv(time, death) ~ garden, data = surv3d,
	                       subset = (garden %in% (unique(garden))[c(i,j)]))
	      gardenlrchisqtable[i,j] <- temp$chisq
	    }
	  } 
	  rownames(gardenlrchisqtable) <- unique(surv3d$garden)
  	colnames(gardenlrchisqtable) <- unique(surv3d$garden)
	
	  #and p-values for the table
	  gardenpval_lrchisqtagible <- round(pchisq(gardenlrchisqtable, 1, lower.tail = FALSE), 5)
	
	#sample size tables
	surv3dsample <- surv3d %>% group_by(pop, type, garden) %>% summarise(time = n()) %>% spread(garden, time)
	
	#create plot (to append to climate data on when deaths occur)
		death2 <- ggplot(data = surv3dd, aes(x = date, y = 1, color = garden)) + 
			geom_jitter(position = position_jitter(width = .5), alpha = 0.4, na.rm = TRUE) +
		    scale_x_date(limits = as.Date(c('2010-01-01','2015-05-08'))) +
		    theme_minimal() +
		    scale_y_continuous(breaks = 1) +
			labs(x = "Year", y = "Mortality") +
			scale_colour_manual(values = c(ephcol, majcol, orchcol), 
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
		
#==============================================================================================#
#3 garden genecological model
		
		climvars <- names((surv3clim)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", 
		                                 "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", 
		                                 "mmax", "mmin", "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", 
		                                 "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])
		
		#FUNCTION
		sorter.rlm <- function( models, ... ) {
		  rsquared.glmm.results <- rsquared.glmm(models)
		  rsquared.glmm.results <- cbind(climvars, rsquared.glmm.results)
		  rsquared.glmm.results[ order(rsquared.glmm.results[,6], decreasing=TRUE), ]
		}
		
		models1 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i  + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
		})
		
		sorter.rlm(models1)
		
# 		29     tdiff glmerMod binomial logit 0.1869403   0.4478408 571.5610
# 		13     gsptd glmerMod binomial logit 0.1652440   0.4352558 568.5207
# 		16   mapmtcm glmerMod binomial logit 0.1613540   0.4346517 566.9694
		
		models2 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i + tdiff + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
		})
		
		sorter.rlm(models2)
# 		
# 		34      smrp glmerMod binomial logit 0.1858036   0.4506061 567.3541
# 		20      mmax glmerMod binomial logit 0.1883275   0.4492052 572.9752
# 		35   sdimtcm glmerMod binomial logit 0.1819501   0.4486575 567.7172	
		
		
		mod3gar <- glmer(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim, family=binomial)
		anova(mod3gar)
		summary(mod3gar)
		coef(mod3gar)
		
		modtest <- glm(cbind(surv, death) ~ tdiff + smrp + type + garden:type + garden, data = surv3clim, family=binomial)
		anova(modtest)
		summary(modtest)
		coef(modtest)
		
		modtest2 <- lmer(propdead ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim)
		anova(modtest2)
		summary(modtest2)
		coef(modtest2)
		
		
		models1 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i  + (1|type) + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
		})
		
		sorter.rlm(models1)
		
		models2 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i  + mapmtcm + (1|type) + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
		})
		
		sorter.rlm(models2)
		
		
		
		mod3step <- glmer(cbind(surv, death) ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim, family=binomial)
		
		mod3step2 <- step(mod3step)
		
		#not shown is the narrowing down of this model to the two climate variables that gives the best Rsq value
		#will only use from populations with total sample numbers >2 (removes 7 points -- total of 52 populations)	
		surv3clim_filter <- surv3clim %>% filter(total > 2)
		
		models1 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i  + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter, family=binomial)
		})
		
		sorter.rlm(models1)
		
# 		29     tdiff glmerMod binomial logit 0.1674837   0.4308679 540.6547
# 		16   mapmtcm glmerMod binomial logit 0.1512361   0.4208720 542.1206
# 		22      mtcm glmerMod binomial logit 0.1489307   0.4192169 545.8160
		
		models2 <- lapply(climvars, function(x) {
		  glmer(substitute(cbind(surv, death) ~ i + tdiff + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter, family=binomial)
		})
		
		sorter.rlm(models2)
# 		35   sdimtcm glmerMod binomial logit 0.1683486   0.4347859 539.2953
# 		20      mmax glmerMod binomial logit 0.1705808   0.4338376 541.5944
# 		34      smrp glmerMod binomial logit 0.1686003   0.4336819 538.7690
		
		
		
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
		