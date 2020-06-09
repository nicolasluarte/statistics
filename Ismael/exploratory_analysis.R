## Load libraries
pacman::p_load(ggplot2, purrr, tidyverse, reshape2, caret, pROC, e1071, lmtest, car, furrr, Hmisc)

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

## load complete data
complete_data <- read_csv('/home/nicoluarte/repos/statistics/Ismael/data/complete_data.csv')

# fix column names
names(complete_data) <- c("type", "beta", "gamma","alpha", "anx", "n")
colnames(complete_data)

# melt data
complete_data_melted <- melt(complete_data,
                             id.vars = "type",
                             measure.vars = c("beta", "gamma", "alpha"))
head(complete_data_melted)
# get medians
medians <- as.data.frame(dcast( complete_data_melted, variable ~ type, median))

## plot all cases + medians
ggplot(complete_data_melted, aes(x = value, fill=as.factor(type))) +
        facet_wrap(~variable, scales = "free_x") +
        geom_histogram(alpha = 0.5, position = "identity") +
        geom_vline(data = filter(complete_data_melted, variable == "beta", ),
                   aes(xintercept = medians[1,2]), linetype = "dashed", color = "red") +
        geom_vline(data = filter(complete_data_melted, variable == "beta", ),
                   aes(xintercept = medians[1,3]), linetype = "dashed", color = "blue") +
        geom_vline(data = filter(complete_data_melted, variable == "gamma", ),
                   aes(xintercept = medians[2,2]), linetype = "dashed", color = "red") +
        geom_vline(data = filter(complete_data_melted, variable == "gamma", ),
                   aes(xintercept = medians[2,3]), linetype = "dashed", color = "blue") +
        geom_vline(data = filter(complete_data_melted, variable == "alpha", ),
                   aes(xintercept = medians[3,2]), linetype = "dashed", color = "red") +
        geom_vline(data = filter(complete_data_melted, variable == "alpha", ),
                   aes(xintercept = medians[3,3]), linetype = "dashed", color = "blue") 

## test for normal distribution
x <- list(0, 0, 0, 1, 1, 1)
y <- list("beta", "gamma", "alpha",
          "beta", "gamma", "alpha")
# no distribution is normal
map2(x, y, ~ complete_data %>%
                filter(type == .x) %>% 
                select(.y) %>% 
                unlist()  %>% 
                shapiro.test())

# no distribution is normal plot
qqfunc <- function(x, y) {
        complete_data %>%
        filter(type == x) %>% 
        select(y) %>% 
        unlist()  %>% 
        qqPlot() 
        Sys.sleep(1)
}
qqplots <- map2(x, y, qqfunc)

## t-test
beta.0 <- complete_data  %>% filter(anx == 1)  %>% 
        select(beta)  %>% 
        unlist()
beta.1 <- complete_data  %>% filter(anx == 0)  %>% 
        select(beta)  %>% 
        unlist()
gamma.0 <- complete_data  %>% filter(anx == 1)  %>% 
        select(gamma)  %>% 
        unlist()
gamma.1 <- complete_data  %>% filter(anx == 0)  %>% 
        select(gamma)  %>% 
        unlist()
alpha.0 <- complete_data  %>% filter(anx == 1)  %>% 
        select(alpha)  %>% 
        unlist()
alpha.1 <- complete_data  %>% filter(anx == 0)  %>% 
        select(alpha)  %>% 
        unlist()

# anxious power mean is only different for beta
print("T-TEST")
t.test(beta.0, beta.1)
t.test(gamma.0, gamma.1)
t.test(alpha.0, alpha.1)

# tails are not fat and data set is relatively large
# so t-test should be ok, however here we test with permutations
t_test_perm <- function(x, y, sample_size, n_times) {
        mnt_carlo <- replicate(n = n_times, expr = {
                sample_x <- sample(x, size = sample_size, replace = FALSE)
                sample_y <- sample(y, size = sample_size, replace = FALSE)
                t_stat_pval <- t.test(sample_x, sample_y)$p.value
                bool <- t_stat_pval > 0.05
                })
        return((length(mnt_carlo[mnt_carlo == "TRUE"]) + 1) / (n_times + 1))
}

# use all cores!
# gamma and alpha doesnt converge
# t-test indicates that H0 is accepted
future::plan(multiprocess)
beta_mnt_carlo <- seq(from = 10, to = 2000, by = 5) %>% 
        future_map(~ t_test_perm(beta.0, beta.1, .x, 10000))
gamma_mnt_carlo <- seq(from = 10, to = 2000, by = 5) %>% 
        future_map(~ t_test_perm(gamma.0, gamma.1, .x, 10000))
alpha_mnt_carlo <- seq(from = 10, to = 2000, by = 5) %>% 
        future_map(~ t_test_perm(alpha.0, alpha.1, .x, 10000))

# exploratory logistic regression
mdl_null <- glm(anx ~ 1,
                data = complete_data,
                family = binomial())
mdl_beta <- glm(anx ~ beta,
                data = complete_data,
                family = binomial())
mdl_gamma <- glm(anx ~ gamma,
                data = complete_data,
                family = binomial())
mdl_alpha <- glm(anx ~ alpha,
                data = complete_data,
                family = binomial())
mdl_beta_gamma <- glm(anx ~ beta + gamma,
                data = complete_data,
                family = binomial())
mdl_beta_gamma_alpha <- glm(anx ~ beta + gamma + alpha,
                data = complete_data,
                family = binomial())

# likelihood ratio test
lrtest(mdl_null, mdl_beta)
lrtest(mdl_null, mdl_gamma)
lrtest(mdl_null, mdl_alpha)

## plot test
head(cd)
cd$id <- 1:dim(cd)[1]
cd[c(2,3,4)] <- scale(cd[c(2,3,4)])


## numbering rows within groups in a data frame
dd.1 <- complete_data  %>%
        group_by(n)  %>%
        filter(anx == 1)  %>% 
        mutate(ID = row_number())
dd.1 <- dd.1  %>% mutate(beta_s = scale(beta), alpha_s = scale(alpha), gamma_s=(scale(gamma)))

## beta 
dd.1_t <- complete_data  %>%
        group_by(n)  %>%
        filter(anx == 1)  %>% 
        mutate(ID = row_number())  %>% 
        mutate(cuts = cut_number(ID, 5, labels = FALSE))
dd.1_t <- dd.1_t  %>%
        mutate(beta_s = scale(beta),
               alpha_s = scale(alpha),
               gamma_s=(scale(gamma)))

ggplot(dd.1_t, aes(x = ID, y = beta_s, )) +
        geom_point() +
        geom_smooth(method = "loess" , alpha = 0.5) +
        facet_wrap(~ cuts, nrow = 1)

## alpha 
dd.2_t <- complete_data  %>%
        group_by(n)  %>%
        filter(anx == 1)  %>% 
        mutate(ID = row_number())  %>% 
        mutate(cuts = cut_number(ID, 5, labels = FALSE))
dd.2_t <- dd.2_t  %>%
        mutate(beta_s = scale(beta),
               alpha_s = scale(alpha),
               gamma_s=(scale(gamma)))

ggplot(dd.2_t, aes(x = ID, y = alpha_s, )) +
        geom_point() +
        geom_smooth(method = "loess" , alpha = 0.5) +
        facet_wrap(~ cuts, nrow = 1)
