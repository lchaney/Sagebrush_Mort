#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *Ephraim Surv with Climate* 
# Chaney et al 2015 Sagebrush mortatlity paper
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


#==============================================================================================#

#model using proportion died
modglm <- glm(cbind(surv, death) ~ sday + gspmtcm + type, data = popdat, family = "quasibinomial")

#calculate GLM R squared value
glmrsq <- function( model, ... ){
  (1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))
}

anova(modglm, test = "F")




