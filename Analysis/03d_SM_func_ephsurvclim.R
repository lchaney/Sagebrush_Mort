#==============================================================================================#
# Script created by Lindsay Chaney 2015 - lchaney@byu.edu
# Script created in version R 3.1.3 
# This script is used for FUNCTIONS needed for *Ephraim Surv with Climate* 
# Chaney et al 2015 Sagebrush mortatlity paper
#==============================================================================================#




#==============================================================================================#
#ephraim survival data with climate


#===========================================================================================

##USING STEPWISE REMOVAL BASED ON VIC (CODE FROM http://jonlefcheck.net/2012/12/28/dealing-with-multicollinearity-using-variance-inflation-factors/)

####################################################################################
####################################################################################

library(plyr)


modmer <- lmer(timedeath ~ adi + adimindd0 + d100 + dd0 + dd5 + fday + ffp + gsdd5 + gsp + pratio + gspdd5 + gspmtcm + gsptd + map + mapdd5 + mapmtcm + maptd + mat + mmindd0 + mmax + mmin + mtcm + mtcmgsp + mtcmmap + sday + sdi + sdimindd0 + tdgsp + tdiff + tdmap + smrpb + sprp + winp + smrp + sdimtcm + dd0map + dd0gsp + (1|type) + (1|type:pop), data= svd)
#needed to remove mtwm due to linear combinations
#removed using findLinearCombos {caret}

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

#sdimindd0
#tdiff
#smrpb
#winp
#sdimtcm
#dd0map

modlm <- lmer(timedeath ~ sdimindd0 + tdiff + smrpb + winp + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove smrpb
modlm2 <- lmer(timedeath ~ sdimindd0 + tdiff + winp + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove winp
modlm3 <- lmer(timedeath ~ sdimindd0 + tdiff + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove tdiff
modlm4 <- lmer(timedeath ~ sdimindd0 + sdimtcm + dd0map + (1|type) + (1|type:pop), data= svd)

#remove dd0map
modlm5 <- lmer(timedeath ~ sdimindd0 + sdimtcm + (1|type) + (1|type:pop), data= svd)
rand(modlm5)
anova(modlm5)

rsquared.glmm(modlm5)
#conditional is variance explained by fixed plus random effects
#marginal is variance explained by fixed effects
 

#can't plot well due to all the NA's
svd1 <- na.omit(svd)
### GRAPH MODEL
ggplot(svd1, 
			aes(y = fitted(modlm5), 
				x = fitted(modlm5) + residuals(modlm5), 
				color=factor(ssp), 
				shape=factor(type)
				),
			size=3) + 
			ylim(min(fitted(modlm5)), max(fitted(modlm5))) + 
			xlim(min(fitted(modlm5) + residuals(modlm5)), 
			max(fitted(modlm5) + residuals(modlm5))) + 
			theme_bw() + scale_shape(solid=TRUE) + 
			stat_smooth(method=lm, se=FALSE, linetype=4) + 
			stat_summary(fun.y = mean, fun.ymin = "sd", fun.ymax = "sd") + 
			labs(x = "Observed", y = "Predicted", color="SubSpecies", shape="Ploidy")
