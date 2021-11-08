#!/usr/local/bin/Rscript
library(tidyverse)
library(lubridate)
library(gridExtra)
library("grid")
library(reshape2)

#######################################################################################
#
#           Fit modified logistic to USD fraction of sales
#
########################################################################################

cat("Predict test on USD/ AUD SPWLwC banks \n")

test <- read_xlsx("SPWLwC_banks_predict_testdata.xlsx")
test <- test %>%
  mutate(PERIOD = as.Date(PERIOD)) %>%
  filter(SALES_FRAC.USD < 1)

# 1. Simple logistics function fit ----------------


y <- test$SALES_FRAC.USD
x <- test$CIR_diff

log.ss <- nls(y ~ SSlogis(x, alpha, xmid, scale))

# Map coefs to form I like
A <- summary(log.ss)$coef[1]
mid <- summary(log.ss)$coef[2]
k <- 1/summary(log.ss)$coef[3]

fit <- data.frame(seq(from = -0.005, to = 0.005, by = 0.0002))
colnames(fit)[1]<-c("CIR_diff")
fit$logistic <- A/(1+exp(-k*(fit$CIR_diff - mid)))

# Plot USD_frac vs. CIR_diff
ggplot(test, aes(x = CIR_diff, y = SALES_FRAC.USD))+ 
  geom_point() +
  geom_line(data = fit, aes(x = CIR_diff, y = logistic), col="red", lty = 2) +
  labs(title = "SPWLwC/banks \n USD/AUD Sales Split, simple logistic model fit",
       x = "CIR_diff (%) = (CIR_USD - CIR_AUD )/2",
       y = "USD fraction of sales (JPY)") +
  theme_light()

# 2. Custom logistic function
# Use parameters above as start values, with mapping, nls self-defined function
# Find the parameters for the equation

SS<-getInitial(y~SSlogis(x,alpha,xmid,scale),
               data=data.frame(y=y,x=x))

A_start <- SS["alpha"]  
x_mid_start <- SS["xmid"]
k_start <- 1/SS["scale"]
y_start <- SS["alpha"]/(1+exp(SS["xmid"]/SS["scale"]))

y_min_start <- 0.05
y_max_start <- 0.95

CIR_diff <- x
# Custom formula for model
adj_log_formula <-formula(y ~ y_min + (y_max-y_min)/(1+exp(-k*(CIR_diff - x_mid))))

# fit this model
mod_log <- nls(adj_log_formula, 
               start = list(y_min=y_min_start, y_max=y_max_start, k=k_start, x_mid=x_mid_start))

# Calc new model over x-range for plotting
y_min <- summary(mod_log)$coef[1]
y_max <- summary(mod_log)$coef[2]
k     <- summary(mod_log)$coef[3]
x_mid <- summary(mod_log)$coef[4]

fit$mod_logis <- y_min +(y_max - y_min)/(1+exp(-k*(fit$CIR_diff - x_mid)))

fits <- fit %>%
  gather(key = "model", value = "USD_frac", -CIR_diff)

sales_data <- test %>%
  select(CIR_diff, SALES_FRAC.USD) %>%
  mutate(model = "data",
         USD_frac = SALES_FRAC.USD) %>%
  select(CIR_diff, USD_frac, model) 

# Plot both models


pdf(file = 'SPWlwC_b_USD_frac_model_fits.pdf', width = 8, height = 6)

ggplot(data = sales_data, aes(x = CIR_diff*100, y = USD_frac)) + 
  geom_point(col="black") +
  geom_line(data = fits,aes(x = CIR_diff*100, y = USD_frac, col = model))+
  labs(title = "SPWLwC/banks \n USD/AUD Sales Split, model fits to CIR_diff",
       x = "CIR_diff (%) = (CIR_USD - CIR_AUD )/2",
       y = "USD fraction of sales (JPY)") +
  theme_light()

dev.off()

# Check fits: confirms mod_log is a better fit
summary(lm(predict(log.ss) ~ y))$adj.r.squared
summary(lm(predict(mod_log) ~ y))$adj.r.squared

# 3. Now make a prediction on new values
#    Format here is super finnickety! nls won't throw error; needs colname same 
#    as used in initial formula assignment. For all vars used. In this ex, just "CIR_diff"

data_to_predict <- data.frame(CIR_diff = c(0,0.0025))    
predict(mod_log, newdata = data_to_predict, type="response")


