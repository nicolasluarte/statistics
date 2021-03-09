pacman::p_load(tidyverse,
               ggplot2,
               lme4,
               caret,
               e1071,
               pROC,
               rms,
               ggeffects,
               lmtest,
               brglm2,
               gridExtra)

## ggplot theme for plots
theme_article <- function() {
  theme_bw(base_size = 12, base_family = "") %+replace%
    theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15),
                plot.title = element_text(hjust = 0.5))
}

## read the data
data <- read_csv('data/complete_data.csv')
## change colnames
names(data) <- c("control_stress", "beta_power",
                 "gamma_power", "alpha_power",
                 "reported_anxiety", "participant_number")
## dependent as factor
data$reported_anxiety <- as.factor(data$reported_anxiety)
data$control_stress <- as.factor(data$control_stress)
data$participant_number[3062:5911] <- c(data$participant_number[3062:5911]) + 21
## scale the variables
data_scaled <- data %>%
        mutate(beta_power_scaled = scale(beta_power),
                gamma_power_scaled = scale(gamma_power),
                alpha_power_scaled = scale(alpha_power),
                outcome = factor(control_stress, labels = c("control", "stress")))
data_scaled$participant_number <- as.factor(data_scaled$participant_number)

test_data <- data_scaled %>% group_by(participant_number) %>% 
  summarise(beta_mean = mean(beta_power),
                             gamma_mean = mean(gamma_power),
                             alpha_mean = mean(alpha_power),
            control_stress = names(which.max(table(reported_anxiety))),
            n = n(),
            condition = as.numeric(control_stress),
            outcome = names(which.max(table(outcome))))
test_data$control_stress <- as.factor(test_data$control_stress)

## scale by condition
model_data <- test_data %>% 
  mutate_at(c("beta_mean", "gamma_mean", "alpha_mean"), scale)

## Model with only intercept
intercept_only <- glm(control_stress ~ 1,
                family = binomial(link="logit"),
                data = model_data)
summary(intercept_only)

## Model with beta as predictor
test_beta <- glm(control_stress ~ as.vector(beta_mean),
                family = binomial(link="logit"),
                data = model_data)
summary(test_beta)
exp(coef(test_beta))
# Model with gamma as predictor
test_gamma <- glm(control_stress ~ as.vector(gamma_mean),
                family = binomial(link="logit"),
                data = model_data)
summary(test_gamma)
# Model with alpha as predictor
test_alpha <- glm(as.factor(control_stress) ~ alpha_mean,
                family = binomial(link="logit"),
                data = model_data)
summary(test_alpha)

## repeated k-fold cross validation
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              savePredictions = TRUE,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
model <- train(form = outcome ~ beta_mean,
               data = model_data,
               trControl = train_control,
               method = "glm",
               family = "binomial")
summary(model)
print(model)
caret::confusionMatrix(model)
model$results

model_gamma <- train(form = outcome ~ gamma_mean,
               data = model_data,
               trControl = train_control,
               method = "glm",
               family = "binomial")
summary(model_gamma)
print(model_gamma)
caret::confusionMatrix(model_gamma)
model_gamma$results

model_null <- train(form = outcome ~ gamma_mean,
               data = model_data,
               trControl = train_control,
               method = "glm",
               family = "binomial")
summary(model_null)
print(model_null)
caret::confusionMatrix(model_null)

## Results Beta model
## Accuracy: 0.8508
## Kappa: 0.7018
## AUC: 0.9375
## Sens: 0.895
## Spec: 0.8266
## Estimate beta_mean: 2.9156, std error: 0.9161; Walds Z: 3.183, Pr(>|Z|): 0.00146; conf int (1.1201, 4.7111)
## Estimate Intercept: 0.1245, std error: 0.4293; Walds Z: 0.290, Pr(>|Z|): 0.77181; conf int (-0.7168, 0.9658)
## McFadden's pseudo R^2: 0.4051708
## Likelihood ratio test: Df: 1; Chisq: 23.591; Pr(>Chisq): < 0.001
waldtest(test_beta, test = c("Chisq"))
confint.default(test_beta)
waldtest(test_gamma, test = c("Chisq"))
confint.default(test_gamma)
lrtest(intercept_only, test_gamma)
lrtest(intercept_only, test_beta)
aod::wald.test(b = coef(test_gamma), Sigma = vcov(test_beta), Terms = 1:1)
## Results Gamma model
## Accuracy: 0.6870
## Kappa: 0.3760
## AUC: 0.7622
## Sens: 0.6372
## Spec: 0.7404
## Estimate gamma_mean: -2.4142, std error: 1.9906; Walds Z: -1.213; Pr(>|Z|): 0.225
## Estimate Intercept: 0.0175, std error: 0.3148; Walds Z: 0.056; Pr(>|Z|): 0.956
## McFadden's pseudo R^2: 0.02708
## Likelihood ratio test: Df: 1; Chisq: 1.5768; Pr(>Chisq): 0.2092

## Null model
## Accuracy: 0.6870235
## Kappa: 0.3760351



model_perf <- lrm(control_stress ~ beta_mean, data = model_data)
model_pred <- predict(test_beta, type = "response")
model_data$prob <- model_pred
roc_curve <- roc(control_stress ~ prob, data = model_data)
conf_mat <- confusionMatrix(confusionMatrix(model)$table)
roc_curve

## Plots
ggroc(roc_curve, legacy.axes = TRUE) + theme_article() + ggtitle("Beta power model ROC\n") +
  geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "dashed")

plot_model_beta <- ggpredict(test_beta, terms = c(as.vector("beta_mean"))) %>%
  plot() +
  xlab("Beta power Z-score") +
  ylab("Pr(Stress condition)") +
  labs(color = "") +
  ylim(c(0, 1)) +
  xlim(c(-1, 1)) +
  geom_point(data = test_data, aes(x = beta_mean,
                                   y = condition,
                                   colour = control_stress), alpha = 0.3) +
  scale_colour_manual(values=c("gray", "green"), labels = c("Control", "Stress")) +
  ggtitle("Beta model\n") +
  theme_article()
  
plot(plot_model_beta)


plot_model_gamma <- ggpredict(test_gamma, terms = c(as.vector("gamma_mean"))) %>%
  plot() +
  xlab("Gamma power Z-score") +
  ylab("Pr(Stress condition)") +
  labs(color = "") +
  ylim(c(0, 1)) +
  xlim(c(-1, 1)) +
  geom_point(data = test_data, aes(x = gamma_mean,
                                   y = condition,
                                   colour = control_stress), alpha = 0.3) +
  scale_colour_manual(values=c("gray", "green"), labels = c("Control", "Stress")) +
  ggtitle("Gamma model\n") +
  theme_article()
  
plot(plot_model_gamma)

# facet wrap
grid.arrange(plot_model_beta, plot_model_gamma)
