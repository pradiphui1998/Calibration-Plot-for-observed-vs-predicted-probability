set.seed(124)
library(ggplot2); theme_set(theme_bw(base_size = 14))

dat <- lme4::InstEval
dat <- dat[sample(nrow(dat), 1000), ]

fit <- glm(y > 3 ~ studage + lectage + service + dept, binomial, dat)
pdat <- with(dat, data.frame(y = ifelse(y > 3, 1, 0),
                             prob = predict(fit, type = "response")))
ggplot(pdat, aes(prob, y)) +
  geom_point(shape = 21, size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = stats::loess, se = FALSE) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  xlab("Estimated Prob.") +
  ylab("Data w/ Empirical Prob.") +
  ggtitle("Logistic Regression Calibration Plot")


library(rms)

refit <- lrm(y > 3 ~ studage + lectage + service + dept, dat, x = TRUE, y = TRUE)
plot(calibrate(refit, B = 400))

