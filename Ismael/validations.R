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
data$control_stress <- as.factor(data$control_stress)
data$participant_number[3062:5911] <- c(data$participant_number[3062:5911]) + 21
## scale the variables
data_scaled <- data %>%
        mutate(beta_power_scaled = scale(beta_power),
                gamma_power_scaled = scale(gamma_power),
                alpha_power_scaled = scale(alpha_power))
data_scaled$participant_number <- as.factor(data_scaled$participant_number)

## Final models

## Model with only intercept
intercept_only <- glm(control_stress ~ 1,
                family = binomial(link="logit"),
                data = data_scaled)
summary(intercept_only)
## Model with random effect
random_intercept <- glmer(control_stress ~ 1 + (0 + beta_power_scaled || participant_number),
                data = data_scaled,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
lmtest::lrtest(intercept_only, random_intercept)

## Model with random intercept and beta fixed effect
model_beta <- glmer(control_stress ~ beta_power_scaled +  
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
model_gamma <- glmer(control_stress ~ gamma_power_scaled +  
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
model_accuracy <- c()
conf_mat <- matrix(c(0,0,0,0), nrow = 2, ncol = 2)
all_participants <- unique(data_scaled[['participant_number']])
num_folds <- 10
folds <- caret::createMultiFolds(all_participants,
                        k = num_folds,
                        times = 10)
for(i in 1:length(folds)){
        train_ids <- folds[[i]]
        test_ids <- all_participants[!(all_participants%in%folds[[i]])]
        train_data <- subset(data_scaled, participant_number %in% train_ids)
        test_data <- subset(data_scaled, participant_number %in% test_ids)
        glm_mdl <- glmer(control_stress ~ beta_power_scaled +  
                            (0 + beta_power_scaled || participant_number),
                        data = train_data,
                        family = binomial(link="logit"),
                        control = glmerControl(optimizer = "bobyqa",
                                               optCtrl = list(maxfun = 2e5)),
                        nAGQ = 1)
        test_participants <- unique(test_data[['participant_number']])
        for(participant in test_participants) {
                prediction_subset <- subset(test_data,
                                            participant_number == participant)
                y_pred <- predict(glm_mdl,
                                  newdata = prediction_subset,
                                  type = c("response"),
                                  allow.new.levels = TRUE)
                caret_obj <- caret::confusionMatrix(factor(round(y_pred)),
                prediction_subset$control_stress)
                model_accuracy <- c(model_accuracy,caret_obj$overall[['Accuracy']])
                conf_mat <- conf_mat + caret_obj$table
        }
}

mean(model_accuracy)
conf_mat
prop.table(conf_mat)
pROC::roc(response = prediction_subset$control_stress, predictor = y_pred)
prediction_subset$control_stress

