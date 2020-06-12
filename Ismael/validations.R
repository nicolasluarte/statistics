pacman::p_load(tidyverse, ggplot2, furrr, purrr, lme4, broom, corrplot, ROCR, lmtest, pbnm, knitr, sjPlot, regclass, effects, jtools, sjstats, performance, grid, ggthemes, stargazer, caret, optimx, ggeffects, lmtest, groupdata2, cvms, cvTools)

## ggplot theme for plots
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

## read the data
data <- read_csv('data/complete_data.csv')
## change colnames
names(data) <- c("control_stress", "beta_power",
                 "gamma_power", "alpha_power",
                 "reported_anxiety", "participant_number")
## dependent as factor
data$reported_anxiety <- as.factor(data$reported_anxiety)
data$participant_number[3062:5911] <- c(data$participant_number[3062:5911]) + 21
## scale the variables
data_scaled <- data %>%
        mutate(beta_power_scaled = scale(beta_power),
                gamma_power_scaled = scale(gamma_power),
                alpha_power_scaled = scale(alpha_power))
data_scaled$participant_number <- as.factor(data_scaled$participant_number)

## Final models

## Model with only intercept
intercept_only <- glm(reported_anxiety ~ 1,
                family = binomial(link="logit"),
                data = data_scaled)
summary(intercept_only)
## Model with random effect
random_intercept <- glmer(reported_anxiety ~ 1 + (0 + beta_power_scaled || participant_number),
                data = data_scaled,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
lmtest::lrtest(intercept_only, random_intercept)

## Model with random intercept and beta fixed effect
model_beta <- glmer(reported_anxiety ~ beta_power_scaled +  
                    (0 + beta_power_scaled || participant_number),
                data = data_scaled,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                nAGQ = 1)
summary(model_beta)
lmtest::lrtest(random_intercept, model_beta)
AIC(random_intercept)
AIC(model_beta)

## model gamma
model_gamma <- glmer(reported_anxiety ~ gamma_power_scaled +  
                    (0 + beta_power_scaled || participant_number),
                data = data_scaled,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)),
                nAGQ = 1)
summary(model_gamma)
AIC(model_beta)
AIC(model_gamma)

## Plots
beta_plot <- ggpredict(model_beta, terms = "beta_power_scaled [all]")
gamma_plot <- ggpredict(model_gamma, term = "gamma_power_scaled [all]")

ggplot(beta_plot, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15)) +
  ylim(c(0, 1)) +
  xlim(c(-2, 8)) +
  xlab("Beta power difference") +
  ylab("Pr(Condition = Anxiety)")

ggplot(gamma_plot, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15)) +
  ylim(c(0, 1)) +
  xlim(c(-2, 8)) +
  xlab("Gamma power difference") +
  ylab("Pr(Condition = Anxiety)")

## cross validation
