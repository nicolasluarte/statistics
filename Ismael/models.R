pacman::p_load(ggplot2, tidyverse, purrr, broom, caret, furrr, lmtest, sjPlot, ggiraphExtra, ggiraph, doMC, pROC)

## load complete data
data <- read_csv('/home/nicoluarte/repos/statistics/Ismael/data/complete_data.csv')
# fix column names
names(data) <- c("type", "beta", "gamma","alpha", "anx", "n")
colnames(data)

## get the helper functions
source("/home/nicoluarte/repos/statistics/Ismael/helper_functions.R")

## test all simple effect models
formulas <- get_all_formulas(c("beta", "gamma", "alpha"), c("as.factor(anx)"))
## add some models on request
formulas <- append(formulas, c("as.factor(anx) ~ beta:gamma", "as.factor(anx) ~ beta:alpha", "as.factor(anx) ~ beta:gamma + beta", "as.factor(anx) ~ beta:alpha + beta"))

## create models
models <- tibble(formulas)
models <- models  %>% 
        mutate(model = map(1:length(formulas), function(x) glm(formulas[x],
                                                               data = data,
                                                               family = binomial())))

models <- models  %>% 
        mutate(metrics = map(model, glance))  %>% 
        unnest(metrics)  %>% 
        select(formulas:AIC)  %>% 
        select(-c(null.deviance, df.null, logLik))

models <- models  %>% 
        mutate(k_fold_cv = map(1:length(formulas), function(x) train_model(data, as.formula(formulas[x]))))

## goodness of git
## create null model
null_model <- glm(as.factor(anx) ~ 1,
                        data = data,
                        family = binomial())

models <- models  %>% 
        mutate(null_lr = map(1:length(formulas), function(x) lrtest(null_model, models$model[[x]])))
models <- models  %>% 
        mutate(only_beta_lr = map(1:length(formulas), function(x) lrtest(models$model[[7]], models$model[[x]])))


## comparisons against null
## simple model with beta is the only meaningfull
models$null_lr[c(7, 4, 6)]

## adding terms
## non significat
models$only_beta_lr[c(1, 5, 3)]

## interaction terms
## only beta:alpha + beta significant
models$only_beta_lr[c(10, 11)]


only_beta <- glm(anx ~ beta, 
                        data = data,
                        family = binomial())
ggPredict(only_beta, se=TRUE, interactive=FALSE, digits=3, colorn=10)

beta_alpha <- glm(anx ~ beta:alpha + beta, 
                        data = data,
                        family = binomial())
ggPredict(only_beta, se=TRUE, interactive=FALSE, digits=3, colorn=10)

## test candidate models
## 7 = only beta
## 11 = beta:alpha + beta
caret::confusionMatrix.train(models$k_fold_cv[[7]])
caret::confusionMatrix.train(models$k_fold_cv[[11]])


prob=predict(beta_alpha,type=c("response"))
data$beta_alpha_pred = prob
prob2=predict(only_beta,type=c("response"))
data$only_beta = prob2
g <- roc(as.factor(anx) ~ prob, data = data)
g2 <- roc(as.factor(anx) ~ prob2, data = data)
plot(g)    
