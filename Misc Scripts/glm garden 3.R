surv3counts <- surv3d %>% group_by(pop, type, garden) %>% summarise(death = sum(death), total = n()) %>% mutate(surv = total - death, propdead = death / total)

fit3 <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden + type:garden + garden:type:pop, data = surv3counts, family = "quasibinomial")
#remove 3way interactions due to singularity
fit3a <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden + type:garden, data = surv3counts, family = "quasibinomial")
#doesn't look like type:garden should be kept
fit3b <- glm(cbind(surv, death) ~ pop + type + garden + pop:garden, data = surv3counts, family = "quasibinomial")
anova(fit3a, fit3b, test = "F")
#confirmed, not significant, we can remove type:garden
#now can re remove type
fit3c <- glm(cbind(surv, death) ~ pop  + garden + pop:garden, data = surv3counts, family = "quasibinomial")
anova(fit3b, fit3c, test = "F")
#yes type can be removed from the model
#so the final, simplist model has pop, garden and pop:garden
anova(fit3c, test = "F")



s3c <- surv3counts %>% ungroup() %>% arrange(garden, propdead)
s33c <- as.data.frame(s3c)
s33c$popnum <- with(s33c, factor(pop, levels = pop[order(garden, propdead)], ordered = TRUE))
s33c$popnum <- as.integer(s33c$popnum)


ggplot(data = s33c, aes(x = garden, y = propdead, group = pop, color = popnum)) + 
  stat_summary(fun.y = mean, geom = "line") + 
  scale_color_gradientn(colours = c("red","violet","blue")) + 
  theme(legend.position = "none") +
  labs(x = "Garden", y = "Proportion Dead")
  








library(RColorBrewer)
colourCount = length(unique(surv3counts$pop))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = pop)) + 
  stat_summary(fun.y=mean, geom="line") + 
  scale_color_manual(values = getPalette(colourCount)) + 
  theme(legend.position="none")

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = as.integer(pop), order = propdead)) + 
  stat_summary(fun.y=mean, geom="line") + 
  scale_color_gradientn(colours = rainbow(7)) + 
  theme(legend.position="none")






ggplot(data = surv3counts, aes(x = garden, y=propdead, color = as.integer(pop))) + geom_jitter() + scale_color_gradient()




p3g <- ggplot(data = surv3counts, aes(x = pop, y = propdead, color = garden)) + geom_point()
p3g <- ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line")

ggplot(data = surv3counts, aes(x = garden, y = propdead, group = pop, color = pop)) + stat_summary(fun.y=mean, geom="line") + scale_color_grey() + theme(legend.position="none")

#with text of means
ggplot(data = surv3counts, aes(x = garden, y = propdead, color = pop, group = pop)) + stat_summary(fun.y=mean, geom="point") + stat_summary(fun.y=mean, geom="line") + stat_summary(fun.y=mean, geom="text",aes(label=round(..y..,2)))