#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *3 garden survival plots*
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#


#==============================================================================================#
#3 garden glm
  
  #restucture data so summarized by each pop and garden
  surv3counts <- surv3d %>% 
    group_by(pop, type, garden) %>% 
    summarise(death = sum(death), total = n()) %>% 
    mutate(surv = total - death, propdead = death / total)

  #glm for the 3 gardens
  fit3 <- glm(cbind(surv, death) ~ pop  + garden + pop:garden, 
              data = surv3counts, family = "quasibinomial")

  anova(fit3, test = "F")
  
    #restructure data for plotting
    s3c <- surv3counts %>% ungroup() %>% arrange(garden, propdead)
      s33c <- as.data.frame(s3c)
      
      #create a dummie variable with pop as a number for coloring purposes
      s33c$popnum <- as.integer(with(s33c, factor(pop, 
                                      levels = pop[order(garden, propdead)], 
                                      ordered = TRUE)))


      #interaction plot
      ggplot(data = s33c, 
             aes(x = garden, y = propdead, group = pop, color = popnum)) + 
        stat_summary(fun.y = mean, geom = "line") + 
        scale_color_gradientn(colours = c("red","violet","blue")) + 
        theme(legend.position = "none") + 
        labs(x = "Garden", y = "Proportion Dead")

#==============================================================================================#


#==============================================================================================#
#3 garden survival

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