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
  glm(substitute(cbind(surv, death) ~ scale(i) + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
})

sorter.r(models1)

#12   gspmtcm + type 0.6782123
#16   mapmtcm + type 0.6766355

models2 <- lapply(climvars, function(x) {
  glm(substitute(cbind(surv, death) ~ scale(gspmtcm) + scale(i) + type, list(i = as.name(x))), data = popdat, family=quasibinomial)
})

sorter.r(models2)

#25      scale(gspmtcm) + scale(sday) + type 0.7573906
#7        scale(gspmtcm) + scale(ffp) + type 0.7510739

modtest <- glm(cbind(surv, death) ~ scale(gspmtcm) + scale(sday) + type, data = popdat, family = quasibinomial)
