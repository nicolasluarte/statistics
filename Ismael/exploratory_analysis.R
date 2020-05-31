## Load libraries
pacman::p_load(ggplot2, purrr, tidyverse, reshape2, caret, pROC, e1071, lmtest)

## Load data in tibble
d <- read_csv('/home/nicoluarte/repos/statistics/Ismael/data/trial_data.csv')

## Remove unnecesary columns
d <- d %>% select(-c(X4, X5, X6, X7, X8, X9, X10))
names(d) <- c("type", "beta", "anx", "n")
colnames(d)

## Plot type and anxiety agains beta power
ggplot(data = d, aes(as.factor(type), beta)) +
       geom_boxplot(outlier.colour = "red") +
       ggtitle("Beta power on type") +
       xlab("0 = control, 1 = experimental") +
       ylab("beta power")

## Same but knowing if participant got anxious or not
ggplot(data = d, aes(as.factor(anx), beta)) +
       geom_boxplot(outlier.colour = "red") +
       ggtitle("Beta power on anxiety") +
       xlab("0 = control, 1 = experimental") +
       ylab("beta power")

## both show the same results, beta power seems to have more spread on '1'
ggplot(d, aes(x=beta, fill=as.factor(anx))) +
        geom_histogram(alpha=0.4, position="identity")

## '1' has more extreme values, check with trim means
d  %>% filter(anx == 1)  %>% 
        select(beta)  %>% 
        unlist() %>% 
        mean(trim = 0.2)
d  %>% filter(anx == 0)  %>% 
        select(beta)  %>% 
        unlist() %>% 
        mean(trim = 0.2)

## seems different check with t-test
a.1 <- d  %>% filter(anx == 1)  %>% 
        select(beta)  %>% 
        unlist()
a.0 <- d  %>% filter(anx == 0)  %>% 
        select(beta)  %>% 
        unlist()
t.test(a.1, a.0)

## simple binary classification
## Include beta power as predictor
mdl.1 <- glm(anx ~ beta,
                data = d,
                family = binomial())
summary(mdl.1)
confint(mdl.1)
exp(coef(mdl.1))
predict(mdl.1, type = "response")
plot(residuals(mdl.1, type = "deviance"))

## check error
model_glm_pred = ifelse(predict(mdl.1, type = "link") > 0, 1, 0) # 1 = anxious
## calculate mean error
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## do
calc_class_err(actual = d$anx, predicted = model_glm_pred)
## 31% error on this classifier

## confusion matrix
conf_mat = table(predicted = model_glm_pred, actual = d$anx)
mdl.1_conf_mat = confusionMatrix(conf_mat, positive = "1")
c(mdl.1_conf_mat$overall["Accuracy"], 
  mdl.1_conf_mat$byClass["Sensitivity"], 
  mdl.1_conf_mat$byClass["Specificity"])

## roc curve
prob = predict(mdl.1, type = "response")
test_roc = roc(d$anx ~ prob, plot = TRUE, print.auc = TRUE)

## compare with the null model
mdl.0 <- glm(anx ~ 1,
                data = d,
                family = binomial())
summary(mdl.0)
lrtest(mdl.0, mdl.1)

## more complex model seem to do better

## check error of simple model

mdl.0_pred = ifelse(predict(mdl.0, type = "link") > 0, 1, 0) # 1 = anxious
## calculate mean error
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
## do
calc_class_err(actual = d$anx, predicted = mdl.0_pred)

## we see almost chance

