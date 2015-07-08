#model using proportion died

library(doBy)

#population level data
	popsurvd <- summaryBy(death ~ pop + type + adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp, data = svdat, FUN = mean)

	#of interest is death.mean -- proporation died

modmer <- lmer(death.mean ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + (1|type), data= popsurvd)


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

#maptd         
#mmax    
#sdimindd0        
#smrpb         
#winp       
#dd0map

pmodlm <- lmer(death.mean ~ maptd + mmax + sdimindd0 + smrpb + winp + dd0map + (1 | type), data = popsurvd)

#remove sdimindd0
pmodlm2 <- lmer(death.mean ~ maptd + mmax + smrpb + winp + dd0map + (1 | type), data = popsurvd)

#remove smrpb
pmodlm3 <- lmer(death.mean ~ maptd + mmax + winp + dd0map + (1 | type), data = popsurvd)

#remove mmax
pmodlm4 <- lmer(death.mean ~ maptd + winp + dd0map + (1 | type), data = popsurvd)

#remove winp
pmodlm4b <- lmer(death.mean ~ maptd + mmax + dd0map + (1 | type), data = popsurvd)

#remove mmax
pmodlm5 <- lmer(death.mean ~ maptd + dd0map + (1 | type), data = popsurvd)
anova(pmodlm5)
rand(pmodlm5)
rsquared.glmm(pmodlm4b)

### GRAPH MODEL
ggplot(popsurvd, 
			aes(y = fitted(pmodlm5), 
				x = fitted(pmodlm5) + residuals(pmodlm5), 
				color=factor(type), 
				shape=factor(type)
				),
			size=3) + 
			ylim(min(fitted(pmodlm5)), max(fitted(pmodlm5))) + 
			xlim(min(fitted(pmodlm5) + residuals(pmodlm5)), 
			max(fitted(pmodlm5) + residuals(pmodlm5))) + 
			theme_bw() + scale_shape(solid=TRUE) + 
			stat_smooth(method=lm, se=FALSE, linetype=4) + 
			stat_summary(fun.y = mean, fun.ymin = "sd", fun.ymax = "sd") + 
			labs(x = "Observed", y = "Predicted", color="SubSpecies", shape="Ploidy")

###OPPS THAT WASN'T RIGHT -- NEED QUASIBIONOMIAL


gmod <- glm(death.mean ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + as.numeric(type), data= popsurvd, family = quasibinomial)

library(car)
mod <- gmod

# Choose a VIF cutoff under which a variable is retained (Zuur et al. 2010 
# MEE recommends 2)
cutoff=8
# Create function to sequentially drop the variable with the largest VIF until 
# all variables have VIF > cutoff
flag=TRUE
viftable=data.frame()
while(flag==TRUE) {
  vfit=vif(mod)[,1] #GLM needs the column specified, LM doesn't
  viftable=rbind.fill(viftable,as.data.frame(t(vfit)))
  if(max(vfit)>cutoff) { mod=
	update(mod,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
  else { flag=FALSE } }
# Look at the final model
print(mod)
# And associated VIFs
print(vfit)
# And show the order in which variables were dropped
print(viftable)

gmod1 <- glm(death.mean ~ fday + sdimindd0 + smrpb + winp + sdimtcm + as.numeric(type), data = popsurvd, family = quasibinomial)
    
#remove fday
gmod2 <- glm(death.mean ~ sdimindd0 + smrpb + winp + sdimtcm + as.numeric(type), data = popsurvd, family = quasibinomial)

#remove smrpb
gmod3 <- glm(death.mean ~ sdimindd0 + winp + sdimtcm + as.numeric(type), data = popsurvd, family = quasibinomial)

#remove winp
gmod4 <- glm(death.mean ~ sdimindd0 + sdimtcm + as.numeric(type), data = popsurvd, family = quasibinomial)

#Playing around
gmodt <- glm(death.mean ~ mtcmmap + as.numeric(type), data = popsurvd, family = quasibinomial)

gmod <- gmodt

### GRAPH MODEL
ggplot(popsurvd, 
			aes(y = fitted(gmod), 
				x = fitted(gmod) + residuals(gmod), 
				color=factor(type), 
				shape=factor(type)
				),
			size=3) + 
			ylim(min(fitted(gmod)), max(fitted(gmod))) + 
			xlim(min(fitted(gmod) + residuals(gmod)), 
			max(fitted(gmod) + residuals(gmod))) + 
			theme_bw() + scale_shape(solid=TRUE) + 
			stat_smooth(method=lm, se=FALSE, linetype=4) + 
			stat_summary(fun.y = mean, fun.ymin = "sd", fun.ymax = "sd") + 
			labs(x = "Observed", y = "Predicted", color="SubSpecies", shape="Ploidy")

gmodt <- glm(death.mean ~ mtcmgsp + sprp + as.numeric(type), data = popsurvd, family = quasibinomial)

gmodt <- glm(death.mean ~ gspmtcm + dd5 + as.numeric(type), data = popsurvd, family = quasibinomial)

gmodt <- glm(death.mean ~ mtcm + d100 + fday + gsp + sprp + as.numeric(type), data = popsurvd, family = quasibinomial)
gmodt <- glm(death.mean ~ mtcm + fday + gsp + sprp + as.numeric(type), data = popsurvd, family = quasibinomial)
gmodt <- glm(death.mean ~ mtcm + fday + sprp + as.numeric(type), data = popsurvd, family = quasibinomial)
gmodt <- glm(death.mean ~ mtcm + fday + as.numeric(type), data = popsurvd, family = quasibinomial)
gmodt <- glm(death.mean ~ mtcm + as.numeric(type), data = popsurvd, family = quasibinomial)



library(MASS)
gmodd <- glm(death.mean ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + as.numeric(type), data= popsurvd, family = binomial)

gmodst <- stepAIC(gmodd, TRACE=FALSE)
gmodst$anova



#calculate GLM R squared value
glmrsq <- function( model, ... ){
		(1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))
			}

#check for overdispersion			
sum( residuals(mod3, type="pearson")^2)/mod3$df.residual			

glmrsq2.pretty <- function( model, ... ){

pred.var <- names(model$coef)[[2]]
rsq<-(1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))

cat( "R^2: ", rsq, "\n")
cat( "Predictor: ", pred.var, "\n")
}

glmrsq2.pretty(mod1) # output not extractable


 # Can us this output to extract/call values/names later
glmrsq2 <- function( model, ... ){

print(names(model$coef)[[2]])
(1-exp((model$dev - model$null)/model$df.null)) / (1-exp(-model$null/model$df.null))

}

glmrsq2(mod1)

#population level data
	popsvd2 <- read.csv("~/Desktop/prop.csv")

	proppop <- merge(popsvd2, popsurvd, by="pop", all=TRUE)
	
	proppop$y <- cbind(proppop$noSurv, proppop$noDead)


	
	modglm <- glm(y ~ gspmtcm + type, data=proppop, family="quasibinomial")
	anova(modglm, test="Chi")
	anova(modglm)


modfullq <- glm(formula = y ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + type, family = "quasibinomial", data = proppop)

#run VIF
library(car)
mod <- modfullq

# Choose a VIF cutoff under which a variable is retained (Zuur et al. 2010 
# MEE recommends 2)
cutoff=2
# Create function to sequentially drop the variable with the largest VIF until 
# all variables have VIF > cutoff
flag=TRUE
viftable=data.frame()
while(flag==TRUE) {
  vfit=vif(mod)[,1] #GLM needs the column specified, LM doesn't
  viftable=rbind.fill(viftable,as.data.frame(t(vfit)))
  if(max(vfit)>cutoff) { mod=
	update(mod,as.formula(paste(".","~",".","-",names(which.max(vfit))))) }
  else { flag=FALSE } }
# Look at the final model
print(mod)
# And associated VIFs
print(vfit)
# And show the order in which variables were dropped
print(viftable)


#reduced from VIF model
modqvred <- glm(formula = y ~ mmax + sday + sdimindd0 + tdiff + sprp + winp + 
    smrp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove sdimindd0
modqvred1 <- glm(formula = y ~ mmax + sday + tdiff + sprp + winp + 
    smrp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove mmax
modqvred2 <- glm(formula = y ~ sday + tdiff + sprp + winp + 
    smrp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove tdiff
modqvred3 <- glm(formula = y ~ sday + sprp + winp + 
    smrp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove winp
modqvred4 <- glm(formula = y ~ sday + sprp + smrp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove smrp
modqvred5 <- glm(formula = y ~ sday + sprp + sdimtcm + dd0map + type, family = "quasibinomial", data = proppop)

#remove dd0map
modqvred6 <- glm(formula = y ~ sday + sprp + sdimtcm + type, family = "quasibinomial", data = proppop)

#remove sday
modqvred7 <- glm(formula = y ~ sprp + sdimtcm + type, family = "quasibinomial", data = proppop)


#create a function to find the best fit according to r squared value
#TEST DATA
modvars<- names(mammalsleep[,c(1:2, 6:9)])

models <- lapply(modvars, function(x) {
    glm(substitute(cbind(dream, nondream) ~ i + danger, list(i = as.name(x))), data = mammalsleep, family=quasibinomial)
})

lapply(models, glmrsq2.pretty)

#####END TEST DATA######



#my data
climvars <- names((proppop)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", "mmax", "mmin",      "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])

models <- lapply(climvars, function(x) {
    glm(substitute(cbind(noSurv, noDead) ~ i + type, list(i = as.name(x))), data = proppop, family=quasibinomial)
})

resul1var <- lapply(models, glmrsq2.pretty)


R^2:  0.7042838 
Predictor:  gspmtcm 

R^2:  0.7063801 
Predictor:  mapmtcm 


models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(noSurv, noDead) ~ i + mapmtcm + type, list(i = as.name(x))), data = proppop, family=quasibinomial)
})

resul1var2 <- lapply(models2, glmrsq2)

R^2:  0.7877982 
Predictor:  sday 

models3 <- lapply(climvars, function(x) {
    glm(substitute(cbind(noSurv, noDead) ~ i + gspmtcm + type, list(i = as.name(x))), data = proppop, family=quasibinomial)
})

resul1var3 <- lapply(models3, glmrsq2)

R^2:  0.7611406 
Predictor:  sday



fitmodel <- glm(cbind(noSurv, noDead) ~ sday + gspmtcm + type, data = proppop, family=quasibinomial)




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

climvars <- names((proppop)[,c("adi", "adimindd0", "d100", "dd0", "dd5", "fday", "ffp", "gsdd5", "gsp", "pratio", "gspdd5", "gspmtcm", "gsptd", "map", "mapdd5", "mapmtcm", "maptd", "mat", "mmindd0", "mmax", "mmin",      "mtcm", "mtcmgsp", "mtcmmap", "sday", "sdi", "sdimindd0", "tdgsp", "tdiff", "tdmap", "smrpb", "sprp", "winp", "smrp", "sdimtcm", "dd0map", "dd0gsp")])

models1 <- lapply(climvars, function(x) {
    glm(substitute(cbind(noSurv, noDead) ~ i + type, list(i = as.name(x))), data = proppop, family=quasibinomial)
})

sorter.r(models1)

#16   mapmtcm + type 0.7063801
#12   gspmtcm + type 0.7042838

models2 <- lapply(climvars, function(x) {
    glm(substitute(cbind(noSurv, noDead) ~ i + mapmtcm + type, list(i = as.name(x))), data = proppop, family=quasibinomial)
})

sorter.r(models2)

#25      sday + mapmtcm + type 0.7877982
#7        ffp + mapmtcm + type 0.7792186
#8      gsdd5 + mapmtcm + type 0.7720290


fitmoda <- glm(cbind(noSurv, noDead) ~ sday + mapmtcm + type, data = proppop, family=quasibinomial)
fitmodb <- glm(cbind(noSurv, noDead) ~ ffp + mapmtcm + type, data = proppop, family=quasibinomial)
fitmodc <- glm(cbind(noSurv, noDead) ~ gsdd5 + mapmtcm + type, data = proppop, family=quasibinomial)

anova(fitmoda, test="F")
anova(fitmodb, test="F")
anova(fitmodc, test="F")
