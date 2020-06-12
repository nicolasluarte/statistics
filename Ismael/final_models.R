pacman::p_load(tidyverse, ggplot2, furrr, purrr, lme4, broom, corrplot, ROCR, lmtest, pbnm, knitr, sjPlot, regclass, effects, jtools, sjstats, performance, grid, ggthemes, stargazer)
devtools::install_github("strengejacke/sjPlot")
install.packages("pbnm.tar.gz", repos=NULL,type="source")

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

## assumptions: independence (fail: repeated measures); multicolinearity; dependent variable ratios
## multicolinearity
## no meaningful correlations
data_correlations <- data  %>%
        select(-c(control_stress, participant_number, reported_anxiety)) %>% 
        cor()
corrplot(data_correlations, method = "number", type = "upper")
## dependent variable ratios is almost 1, no problem here
dv_ratio <- table(data$reported_anxiety)
dv_ratio
## check per participant
## something happend with participant 10, 11?
sub_dv_ratio <- prop.table(table(data$participant_number, data$reported_anxiety), 1)
head(sub_dv_ratio)
plot(sub_dv_ratio)

## check per participant mean values of band power
mean_values_per_band <- data  %>%
        group_by(participant_number, reported_anxiety)  %>%
        summarise(beta_mean = mean(beta_power),
                        alpha_mean = mean(alpha_power),
                        gamma_mean = mean(gamma_power))

## same but with dispersion
par(mfrow=(c(3, 2)))
## beta
boxplot(beta_power ~ participant_number,
        subset = reported_anxiety == 1, data = data, main = "reported anxiety = 1")
stripchart(beta_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 1)
boxplot(beta_power ~ participant_number,
        subset = reported_anxiety == 0, data = data, main = "reported anxiety = 0")
stripchart(beta_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 0)
## alpha
boxplot(alpha_power ~ participant_number,
        subset = reported_anxiety == 1, data = data, main = "reported anxiety = 1")
stripchart(alpha_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 1)
boxplot(alpha_power ~ participant_number,
        subset = reported_anxiety == 0, data = data, main = "reported anxiety = 0")
stripchart(alpha_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 0)
## gamma
boxplot(gamma_power ~ participant_number,
        subset = reported_anxiety == 1, data = data, main = "reported anxiety = 1")
stripchart(gamma_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 1)
boxplot(gamma_power ~ participant_number,
        subset = reported_anxiety == 0, data = data, main = "reported anxiety = 0")
stripchart(gamma_power ~ participant_number, vertical = TRUE, data = data, 
    method = "jitter", add = TRUE, pch = 20, col = 'blue',
        subset = reported_anxiety == 0)

## by subject
## note subject 10, 11 no 1 or 0 for reported anxiety
beta_g <- ggplot(data, aes(x=reported_anxiety, y=beta_power)) +
        geom_boxplot() +
        facet_wrap(~participant_number)
beta_g <- ggplot(data, aes(x=reported_anxiety, y=beta_power)) +
        geom_boxplot() +
        facet_wrap(~participant_number)
alpha_g <- ggplot(data, aes(x=reported_anxiety, y=alpha_power)) +
        geom_boxplot() +
        facet_wrap(~participant_number)
gamma_g <- ggplot(data, aes(x=reported_anxiety, y=gamma_power)) +
        geom_boxplot() +
        facet_wrap(~participant_number)

#  __  __           _      _     
# |  \/  | ___   __| | ___| |___ 
# | |\/| |/ _ \ / _` |/ _ \ / __|
# | |  | | (_) | (_| |  __/ \__ \
# |_|  |_|\___/ \__,_|\___|_|___/

## completely null model
## intercept is negative but super small, as expected by dv_ratio
## more likely to be 0
m1 <- glm(reported_anxiety ~ 1,
                family = binomial(link="logit"),
                data = data)
summary(m1)

## control for repeated measures
## participant_number intercept variance is 2.803, so they clearly vary in their intercepts
## intercept is no longer significant, probably because ratio 0-1 is not informative at
## participant level
m2 <- glmer(reported_anxiety ~ 1 + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m2)
## can also check for intra-class correlation
## variation of outcome can be accounted by participant (clustering)
## expected because of repeated measures: 46%~
icc(m2)

## model comparison
## adding random effect of participant improved the model
m1$aic
AIC(logLik(m2))

## addging fixed effects
## adding beta
## beta_power estimate is positive, as beta_power increases there's more odds that 
## participant is under stress trial
m3 <- glmer(reported_anxiety ~ beta_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m3)

## model comparison
## there's an improvement, and the fixed effect addition is significant
AIC(logLik(m2))
AIC(logLik(m3))

## variance in the slope by trials by participant number is near 0
m4 <- glmer(reported_anxiety ~ beta_power + (beta_power | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m4)

## model comparison
## there's is a super tiny improvement
AIC(logLik(m3))
AIC(logLik(m4))


#                      _      _        _               _    
#  _ __ ___   ___   __| | ___| |   ___| |__   ___  ___| | __
# | '_ ` _ \ / _ \ / _` |/ _ \ |  / __| '_ \ / _ \/ __| |/ /
# | | | | | | (_) | (_| |  __/ | | (__| | | |  __/ (__|   < 
# |_| |_| |_|\___/ \__,_|\___|_|  \___|_| |_|\___|\___|_|\_\
 
## alpha as fixed effect
## alpha_power is not significant
m3.1 <- glmer(reported_anxiety ~ alpha_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m3.1)

## gamma as fixed effect
## gamma power is significant but has a super small estimate
m3.2 <- glmer(reported_anxiety ~ gamma_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m3.2)

## complete model
m3.3 <- glmer(reported_anxiety ~ beta_power + alpha_power + gamma_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(m3.3)

## model comparison
## gamma shows a slight improvement over the null, however beta is better
## complete model offer a super small improvement over using only beta
AIC(logLik(m3.3))

## we could check for interaction effects
## adding interaction effect with alpha
mi1.1 <- glmer(reported_anxiety ~ beta_power + beta_power:alpha_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(mi1.1)

## adding interaction effect with gamma
mi1.2 <- glmer(reported_anxiety ~ beta_power + beta_power:gamma_power + (1 | participant_number),
                data = data,
                family = binomial(link="logit"),
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(mi1.2)

## model comparison
AIC(logLik(m2))
AIC(logLik(m3))
## interaction with alpha is slightly better
AIC(logLik(mi1.1))
## adding interaction effect with gamma increments AIC
AIC(logLik(mi1.2))



#  ____   ___   ____ 
# |  _ \ / _ \ / ___|
# | |_) | | | | |    
# |  _ <| |_| | |___ 
# |_| \_\\___/ \____|
## sensitivity: what proportion of truly class c cases are correctly recognized by the model?
## specificity: what proportion of cases truly not belonging to class c are correctly recognized as not coming from class c?
## not good
m3_pred <- predict(m3, type = "response")
m4_pred <- predict(m4, type = "response")
m3.3_pred <- predict(m3.3, type = "response")
mi1.1_pred <- predict(mi1.1, type = "response")

roc.m3 <- prediction(m3_pred, data$reported_anxiety)
roc.m4 <- prediction(m4_pred, data$reported_anxiety)
roc.m3.3 <- prediction(m3.3_pred, data$reported_anxiety)
roc.mi1.1 <- prediction(mi1.1_pred, data$reported_anxiety)

roc.1 <- performance(roc.m3, measure = "tpr", x.measure = "fpr")
roc.2 <- performance(roc.m4, measure = "tpr", x.measure = "fpr")
roc.3 <- performance(roc.m3.3, measure = "tpr", x.measure = "fpr")
roc.4 <- performance(roc.mi1.1, measure = "tpr", x.measure = "fpr")

plot(roc.1, col)
plot(roc.2, add = TRUE)
plot(roc.3, add = TRUE)
plot(roc.4, add = TRUE)
abline(a=0, b= 1)


#  _      _            _   
# | |_ __| |_ ___  ___| |_ 
# | | '__| __/ _ \/ __| __|
# | | |  | ||  __/\__ \ |_ 
# |_|_|   \__\___||___/\__|
#                          
## finally we test for if the differences found are significant
lmtest::lrtest(m2, m3)
lmtest::lrtest(m3, m4)
lmtest::lrtest(m3, m3.3)
## the interaction effect seem to be the only improvement over beta only
lmtest::lrtest(m3, mi1.1)


#  ____  _     ___ _____ ____  
# |  _ \| |   / _ \_   _/ ___| 
# | |_) | |  | | | || | \___ \ 
# |  __/| |__| |_| || |  ___) |
# |_|   |_____\___/ |_| |____/ 
options(browser="/usr/bin/brave")

## to create tables use stargazer

## beta:alpha
## quantiles for interaction effects
alpha_quantile <- quantile(data$alpha_power, probs=c(0, 0.50, 1))
alpha_quantile <- round(alpha_quantile, 2)
alpha_quantile
## interaction by quantile
mi1.1_p <- effect(term="beta_power:alpha_power",
                  xlevels= list(alpha_power=c(alpha_quantile)),
                  mod=mi1.1,
                  se=list(type="pointwise"))
mi1.1_p <- as.data.frame(mi1.1_p)
mi1.1_p$alpha_power <- as.factor(mi1.1_p$alpha_power)
ggplot(mi1.1_p, aes(x=beta_power, y=fit, color=alpha_power,group=alpha_power)) + 
#    geom_point(position = position_jitter()) + 
    geom_smooth(size=1.2, method = "lm") +
#    geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=alpha_power),alpha=0.3) +
    facet_wrap(~ alpha_power)

## only beta
m3_p <- effect(term="beta_power",
                  mod=mi1.1,
                  se=list(type="pointwise"))
## do Gamma only Plot
## con los puntitos arriba
## modelos descriptivos
## k-fold para gamma y beta (solos)
mgamma <- effect(term="gamma_power",
                  mod=m3.2,
                  se=list(type="pointwise"))
m3_p <- as.data.frame(m3_p)
mgamma <- as.data.frame(mgamma)
ggplot(mgamma, aes(x=gamma_power, y=fit)) + 
#    geom_point(position = position_jitter()) + 
    geom_smooth(size=1.2, method = "lm") +
    ylim(c(0, 1))
#    geom_ribbon(aes(ymin=fit-se, ymax=fit+se),alpha=0.3) +
#    theme_Publication() +
#    scale_fill_Publication() +
#    scale_colour_Publication()

#  _____ _   _ _   _  ____ _____ ___ ___  _   _ ____  
# |  ___| | | | \ | |/ ___|_   _|_ _/ _ \| \ | / ___| 
# | |_  | | | |  \| | |     | |  | | | | |  \| \___ \ 
# |  _| | |_| | |\  | |___  | |  | | |_| | |\  |___) |
# |_|    \___/|_| \_|\____| |_| |___\___/|_| \_|____/ 
#

theme_Publication <- function(base_size=14, base_family="helvetica") {
      library(grid)
      library(ggthemes)
      (theme_foundation(base_size=base_size, base_family=base_family)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.key.size= unit(0.2, "cm"),
               legend.margin = unit(0, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="bold")
          ))
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}





#  _____ _____ ____ _____ ____  
# |_   _| ____/ ___|_   _/ ___| 
#   | | |  _| \___ \ | | \___ \ 
#   | | | |___ ___) || |  ___) |
#   |_| |_____|____/ |_| |____/ 
#                               

