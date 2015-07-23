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
modglm <- glm(cbind(surv, death) ~ gspmtcm + type, data=popdat, family="quasibinomial")


#calculate GLM R squared value
glmrsq <- function( model, ... ){
  (1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))
}

modglm <- glm(y ~ gspmtcm + type, data=popdat, family="quasibinomial")
anova(modglm, test="Chi")
anova(modglm)

#check for overdispersion			
sum( residuals(mod3, type="pearson")^2)/mod3$df.residual

#################
#################
# Gives Rsq + all predictor variables used.
glmrsq2 <- function( model, ... ){
  
  cbind( deparse(model$formula[[3]]), (1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null)))
  
}


# Function for Sorting & ordering the output
sorter.r <- function( models, ... ) {
  glmrsq2.results <- data.frame(do.call(rbind, lapply(models,  glmrsq2)))
  glmrsq2.results[,2] <- as.numeric(as.character(glmrsq2.results[,2]))
  glmrsq2.results[ order(glmrsq2.results[,2], decreasing=TRUE), ]
}



#################
#################

climvars <- names((popdat)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", "mmax", "mmin",      "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])

models1 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
})

sorter.r(models1)

#12   gspmtcm + type 0.6782123
#16   mapmtcm + type 0.6766355

models2 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + gspmtcm + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
})

sorter.r(models2)

#25      sday + gspmtcm + type 0.7573906
#7        ffp + gspmtcm + type 0.7510739
#8      gsdd5 + gspmtcm + type 0.7467814

models2 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + mapmtcm + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
})

sorter.r(models2)

#25      sday + mapmtcm + type 0.7816989
#7        ffp + mapmtcm + type 0.7736579
#8      gsdd5 + mapmtcm + type 0.7654081


fitmoda <- glm(cbind(surv, death) ~ sday + mapmtcm + type, data = popdat, family=quasibinomial)
fitmodB <- glm(cbind(surv, death) ~ sday + gspmtcm + type, data = popdat, family=quasibinomial)


anova(fitmoda, test="F")
anova(fitmodB, test="F")


#~~~~~by ssp~~~~~#
models1t <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i, list(i = as.name(x))), data = popdat[which(popdat$type == "T4x" | popdat$type == "T2x"),], family=quasibinomial)
})

sorter.r(models1t)

#29     tdiff 6.023354e-01
#12   gspmtcm 4.703641e-01

models2t <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~  i + tdiff, list(i = as.name(x))), data = popdat[which(popdat$type == "T4x" | popdat$type == "T2x"),], family=quasibinomial)
})

sorter.r(models2t)

#20      mmax + tdiff 0.6675465
#27 sdimindd0 + tdiff 0.6658496
#25      sday + tdiff 0.6545857

#~~~~~by ssp~~~~~#
models1w <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i, list(i = as.name(x))), data = popdat[which(popdat$type == "W4x"),], family=quasibinomial)
})

sorter.r(models1w)

#12   gspmtcm 0.622720251
#16   mapmtcm 0.613275992

fitmodwb <- glm(cbind(surv, death) ~ gspmtcm, data = popdat[which(popdat$type == "W4x"),], family=quasibinomial)
anova(fitmodwb, test="F")

#~~~~~by ssp~~~~~#
models1v <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i, list(i = as.name(x))), data = popdat[which(popdat$type == "V4x" | popdat$type == "V2x"),], family=quasibinomial)
})

sorter.r(models1v)

# 22      mtcm 0.741979999
# 16   mapmtcm 0.679365109
# 24   mtcmmap 0.658301330
# 4        dd0 0.590968349
# 23   mtcmgsp 0.589966175
# 21      mmin 0.556414204
# 12   gspmtcm 0.549813729
# 29     tdiff 0.544753516
# 35   sdimtcm 0.498217342

models2v <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ i + mtcm, list(i = as.name(x))), data = popdat[which(popdat$type == "V4x" | popdat$type == "V2x"),], family=quasibinomial)
})

sorter.r(models2v)

# 31     smrpb + mtcm 0.7970469
# 20      mmax + mtcm 0.7743183
# 34      smrp + mtcm 0.7738930
# 21      mmin + mtcm 0.7726901

fitmodva <- glm(cbind(surv, death) ~ smrpb + mtcm, data = popdat[which(popdat$type == "V4x" | popdat$type == "V2x"),], family=quasibinomial)
anova(fitmodva, test="F")


