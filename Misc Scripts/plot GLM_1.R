modglm


modglmd <- glm(death ~ sday + gspmtcm + type, family = "quasibinomial", data = svdat)

modtest <- glm(death ~ scale(sday) + scale(gspmtcm) + type, family = "quasibinomial", data = svdat)


ggplot(data = svdat, aes(x = scale(sday) + scale(gspmtcm), y = death, color = type)) +
  geom_point() +
  facet_wrap( ~ type) +
  stat_smooth(method = 'glm', family = 'binomial')

ggplot(data = svdat, aes(x = gspmtcm, y = death, color = type)) +
  geom_point() + stat_smooth(method = 'glm', family = 'binomial')
  