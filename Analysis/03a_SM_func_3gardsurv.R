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
		
# 		climvars <- names((surv3clim)[,-c(1:7)])
# 
# 		#FUNCTION
# 		sorter.rlm <- function( models, ... ) {
# 		  rsquared.glmm.results <- rsquared.glmm(models)
# 		  rsquared.glmm.results <- cbind(climvars, rsquared.glmm.results)
# 		  rsquared.glmm.results[ order(rsquared.glmm.results[,6], decreasing=TRUE), ]
# 		}
# 		
# 		models1 <- lapply(climvars, function(x) {
# 		  glmer(substitute(cbind(surv, death) ~ i  + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
# 		})
# 		
# 		sorter.rlm(models1)
# 
# # 		37     tdiff glmerMod binomial logit 0.1782063   0.4504604 543.0587
# # 		8    dd5mtcm glmerMod binomial logit 0.1643283   0.4385943 557.1595
# # 		13    gspdd5 glmerMod binomial logit 0.1699539   0.4382220 558.3863
# 		
# 		models2 <- lapply(climvars, function(x) {
# 		  glmer(substitute(cbind(surv, death) ~ i + tdiff + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim, family=binomial)
# 		})
# 		
# 		sorter.rlm(models2)
# # 		33      smrp glmerMod binomial logit 0.1835436   0.4558232 541.2585
# # 		12       gsp glmerMod binomial logit 0.1818801   0.4535886 542.1341
# # 		15     gsptd glmerMod binomial logit 0.1820689   0.4533774 542.5865
# 
# 		 		
# 		mod3gar <- glmer(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim, family=binomial)
#  		anova(mod3gar)
#  		summary(mod3gar)
#  		coef(mod3gar)
#  		rsquared.glmm(mod3gar)
#  		
#  		library(afex)
#  		gar3glm <- mixed(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim, family = binomial, method = "LRT")
#  		anova(gar3glm)
#  		nice(gar3glm)
# 
#  		
# # 		#not shown is the narrowing down of this model to the two climate variables that gives the best Rsq value
# # 		#will only use from populations with total sample numbers >2 (removes 7 points -- total of 52 populations)	
# 		surv3clim_filter <- surv3clim %>% filter(total > 2)
# # 		
# 		models1 <- lapply(climvars, function(x) {
# 		  glmer(substitute(cbind(surv, death) ~ i  + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter, family=binomial)
# 		})
# 		
# 		sorter.rlm(models1)
# 
# # 		37     tdiff glmerMod binomial logit 0.1655153   0.4341422 520.1423
# # 		8    dd5mtcm glmerMod binomial logit 0.1610652   0.4296736 533.0327
# # 		18   mapmtcm glmerMod binomial logit 0.1565902   0.4243131 531.8999
# 		
# 		models2 <- lapply(climvars, function(x) {
# 		  glmer(substitute(cbind(surv, death) ~ i + tdiff + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter, family=binomial)
# 		})
# 		
# 		sorter.rlm(models2)
# # 		33      smrp glmerMod binomial logit 0.1710303   0.4396341 518.4304
# # 		18   mapmtcm glmerMod binomial logit 0.1696223   0.4386990 519.3230
# # 		12       gsp glmerMod binomial logit 0.1704890   0.4383489 518.0615
# 
# # # 		with(prov_clim, cor.test(tdiff, smrp))
# 
# #  		
# #  		
#  		mod3gar_fil <- glmer(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim_filter, family=binomial)
# 		
# #  		confint(mod3gar_fil, level = .2)
#  		    gar3glm_fil <- mixed(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim_filter, family = binomial, method = "LRT")
#  		 		anova(gar3glm_fil)
#  		 		anova(mod3gar_fil, update(mod3gar_fil, .~.-(1|garden)))
#  		 		anova(mod3gar_fil, update(mod3gar_fil, .~.-(1|garden:type)))
# #  		
# #  		mod3gar_fil
# #  		summary(mod3gar_fil)
# #  		
# #  		
# #  		surv3clim_filter3 <- surv3clim %>% filter(total > 3)
# #  		
#  		models1 <- lapply(climvars, function(x) {
#  		   		  glmer(substitute(cbind(surv, death) ~ i  + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter3, family=binomial)
#  		   		})
#  		   		
#  		   		sorter.rlm(models1)
# # #  		
# #  		   		37     tdiff glmerMod binomial logit 0.1597568   0.4329849 488.6098
# #  		   		8    dd5mtcm glmerMod binomial logit 0.1535619   0.4265480 502.1137
# #  		   		24      mtcm glmerMod binomial logit 0.1490470   0.4222346 502.2943
# #  		   		
# #  		   		
#  		   		models2 <- lapply(climvars, function(x) {
#  		   		  glmer(substitute(cbind(surv, death) ~ i + tdiff + type + (1|garden:type) + (1|garden), list(i = as.name(x))), data = surv3clim_filter3, family=binomial)
#  		   		})
#  		   		
#  		   		sorter.rlm(models2)
#  		   		
# #  		   		33      smrp glmerMod binomial logit 0.1652061   0.4382776 486.8903
# #  		   		21      mmax glmerMod binomial logit 0.1639461   0.4382324 487.9585
# #  		   		18   mapmtcm glmerMod binomial logit 0.1634424   0.4369257 487.7818
#  		   		
# #  		   		
#   		   		mod3gar_fil3 <- glmer(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim_filter3, family=binomial)
# #  		   		
# #  		   		confint(mod3gar_fil, level = .2)
#  		   		     gar3glm_fil3 <- mixed(cbind(surv, death) ~ tdiff + smrp + type + (1|garden:type) + (1|garden), data = surv3clim_filter3, family = binomial, method = "LRT")
#  		   		  		anova(gar3glm_fil3)
#  		   		  		nice(gar3glm_fil3)
#  		   		  		anova(mod3gar_fil3, update(mod3gar_fil3, .~.-(1|garden)))
#  		   		  		anova(mod3gar_fil3, update(mod3gar_fil3, .~.-(1|garden:type)))
# #  		   		
# #  		   		mod3gar_fil
# #  		   		summary(mod3gar_fil3)
# #  		   		
#  		   		
 		   		            
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
		