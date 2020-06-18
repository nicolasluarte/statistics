cman::p_load(tidyverse,
               ggplot2,
               lme4,
               caret,
               e1071,
               pROC,
               rms,
               ggeffects)

## ggplot theme for plots
theme_article <- function() {
  theme_bw(base_size = 12, base_family = "") %+replace%
    theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))
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
        mutate(beta_power_scaled = (beta_power - mean(beta_power) / sd(beta_power)),
                gamma_power_scaled = scale(gamma_power),
                alpha_power_scaled = scale(alpha_power))
data_scaled$participant_number <- as.factor(data_scaled$participant_number)

test_data <- data_scaled %>% group_by(participant_number) %>% 
  summarise(beta_mean = mean(beta_power_scaled),
                             gamma_mean = mean(gamma_power_scaled),
                             alpha_mean = mean(alpha_power_scaled),
            control_stress = names(which.max(table(control_stress))),
            n = n())
test_data$control_stress <- as.factor(test_data$control_stress)

## Model with only intercept
intercept_only <- glm(control_stress ~ 1,
                family = binomial(link="logit"),
                data = test_data)
summary(intercept_only)

## Model with beta as predictor
test_beta <- glm(as.factor(control_stress) ~ beta_mean,
                family = binomial(link="logit"),
                data = test_data)
summary(test_beta)
# Model with gamma as predictor
test_gamma <- glm(as.factor(control_stress) ~ gamma_mean,
                family = binomial(link="logit"),
                data = test_data)
summary(test_gamma)
# Model with alpha as predictor
test_alpha <- glm(as.factor(control_stress) ~ alpha_mean,
                family = binomial(link="logit"),
                data = test_data)
summary(test_alpha)

## repeated k-fold cross validation
train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              savePredictions = TRUE)
model <- train(form = control_stress ~ beta_mean,
               data = test_data,
               trControl = train_control,
               method = "glm",
               family = "binomial")
summary(model)
print(model)


## model summary
model_perf <- lrm(control_stress ~ beta_mean, data = test_data)
model_pred <- predict(test_beta, type = "response")
test_data$prob <- model_pred
roc_curve <- roc(control_stress ~ prob, data = test_data)
conf_mat <- confusionMatrix(confusionMatrix(model)$table)

## Plots
ggroc(roc_curve, legacy.axes = TRUE) + theme_article()
plot_model <- ggpredict(test_beta, term = "beta_mean [all]") %>%
  plot() +
  theme_article() +
  xlab("Beta power Z-score") +
  ylab("Pr(Induced anxiety condition)") +
  ylim(c(0, 1)) +
  geom_point(data = test_data, aes(x = beta_mean,
                                   y = as.numeric(control_stress),
                                   colour = control_stress), alpha = 0.3) +
  scale_colour_manual(values=c("blue", "red"))
plot(plot_model, add.data = TRUE)
