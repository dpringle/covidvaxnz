# #!/usr/local/bin/Rscript
library(tidyverse)
library(lubridate)
library(gridExtra)
library("grid")
library(reshape2)
library('drc')
#######################################################################################
#
#           Fit modified logistic to USD fraction of sales
#
########################################################################################


v_nz_d <- v_nz[dose == 2]
v_nz_d[ , weeknum := as.numeric(week)]
head(v_nz_d)

# 1. Simple logistics function fit ----------------

y <- v_nz_d$uptake
x <- v_nz_d$weeknum

log.ss <- nls(y ~ SSlogis(x, alpha, xmid, scale))

# Map coefs to form I like
A <- summary(log.ss)$coef[1]
mid <- summary(log.ss)$coef[2]
k <- 1/summary(log.ss)$coef[3]

fit <- data.frame(seq(from = 18650, to = 18950, by = 5))
colnames(fit)[1]<-c("weeknum")
fit$logistic <- A/(1+exp(-k*(fit$weeknum - mid)))

# Plot USD_frac vs. CIR_diff
ggplot(data = v_nz_d, aes(x = weeknum, y = uptake))+ 
  geom_point() +
  geom_line(data = fit, aes(x = weeknum, y = logistic), col="red", lty = 2) +
  labs(title = "Vaccine (dose 1) uptake with a simple logistic model fit",
       x = "weeknum",
       y = "Dose 1 vaccine uptake") +
  theme_light()

# 2. Custom logistic function
# Use parameters above as start values, with mapping, nls self-defined function
# Find the parameters for the equation

SS<-stats::getInitial(uptake ~ SSlogis(weeknum,alpha,xmid,scale),
               data=v_nz_d)

A_start <- SS["alpha"]  
x_mid_start <- SS["xmid"]
k_start <- 1/SS["scale"]
y_start <- SS["alpha"]/(1+exp(SS["xmid"]/SS["scale"]))

y_min_start <- 0
y_max_start <- 0.95

# Custom formula for model
weeknum <- x
adj_log_formula <-formula(y ~ y_min + (y_max-y_min)/(1+exp(-k*(weeknum - x_mid))))

# modified logistic fit
mod_logistic <- nls(adj_log_formula, 
               start = list(y_min=y_min_start, y_max=y_max_start, k=k_start, x_mid=x_mid_start))
# Calc new model over x-range for plotting
y_min <- summary(mod_logistic)$coef[1]
y_max <- summary(mod_logistic)$coef[2]
k     <- summary(mod_logistic)$coef[3]
x_mid <- summary(mod_logistic)$coef[4]
fit$mod_logistic <- y_min +(y_max - y_min)/(1+exp(-k*(fit$weeknum - x_mid)))

# log-logistic fit
log_logistic <- drm(uptake~weeknum, data = v_nz_d, fct = LL.4())
scal2 <- summary(log_logistic)$coef[1]
y_min2 <- summary(log_logistic)$coef[2]
y_max2 <- summary(log_logistic)$coef[3]
x_mid2 <- summary(log_logistic)$coef[4]
fit$log_logistic <- y_min2 +(y_max2 - y_min2)/(1+exp(scal2*(log(fit$weeknum) - log(x_mid2))))

# weibull2.3 fit
y_min3 = 0
weibull2.3 <- drm(uptake~weeknum, data = v_nz_d, fct = W2.3(fixed = c(NA,NA, NA)))
scal3 <- summary(weibull2.3)$coef[1]
y_max3 <- summary(weibull2.3)$coef[2]
x_mid3 <- summary(weibull2.3)$coef[3]
fit$weibull2.3 <- y_min3 +(y_max3 - y_min3)*(1-exp(-exp(scal3*(log(fit$weeknum) - log(x_mid3)))))

# skew log_logistic
y_min4 = 0
log_logistic5 <- drm(uptake~weeknum, data = v_nz_d, fct = LL.5())
summary(log_logistic5)

scal4 <-  summary(log_logistic5)$coef[1]
y_min4 <- summary(log_logistic5)$coef[2]
y_max4 <- summary(log_logistic5)$coef[3]
x_mid4 <- summary(log_logistic5)$coef[4]
skew4 <- summary(log_logistic5)$coef[5]

fit$log_logistic5 <- y_min4 + (y_max4 - y_min4)/((1+exp(scal4*(log(fit$weeknum) - log(x_mid4))))^skew4)

rm(fits)
fits <- fit %>%
  gather(key = "model", value = "uptake", -weeknum)
fits

ggplot(data = v_nz_d, aes(x = as.Date(weeknum, origin = ("1970-01-01")), y = uptake)) + 
  geom_point(col="black") +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,0.2))+
  geom_line(data = fits, aes(x = as.Date(weeknum, origin = ("1970-01-01")), y = uptake, col = model))+
  labs(title = "Various model fits to overall NZ 1st dose Vaccine Uptake",
       x = "week",
       y = "USD fraction of sales (JPY)") +
  theme_light()


# Check fits: confirms mod_log is a better fit
summary(lm(predict(log.ss) ~ y))$adj.r.squared
summary(lm(predict(mod_logistic) ~ y))$adj.r.squared
summary(lm(predict(log_logistic) ~ y))$adj.r.squared
summary(lm(predict(weibull2.3) ~ y))$adj.r.squared
summary(lm(predict(log_logistic5) ~ y))$adj.r.squared

# 3. Now make a prediction on new values
#    Format here is super finnickety! nls won't throw error; needs colname same 
#    as used in initial formula assignment. For all vars used. In this ex, just "CIR_diff"
data_to_predict <- data.frame(weeknum =  as.numeric(as.Date(c("2021-12-01","2022-01-01","2022-02-01"))))
data_to_predict
predict(log_logistic5, newdata = data_to_predict, type="response")

