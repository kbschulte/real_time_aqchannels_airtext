## Real-time air quality channels - linear regression model reproducible code#
## 16/05/2021 ##
## Kayla Schulte ##
## Leverhulme Centre for Demographic Science - University of Oxford ##

#Load packages

packages <- c("dplyr", "lubridate", "ggplot2", "corrplot", "robustbase", "lmtest")

lapply(packages, require, character.only = TRUE)

#Load data

load("/Users/kaylaschulte/Research/Rdas/airtext_data.rda")
load("/Users/kaylaschulte/Research/Rdas/GLA_2016.rda")
load("/Users/kaylaschulte/Research/Rdas/demographic_data.rda")

#Linear model for Airtext subscription by postcode

nrow(airtext_GLA2016_no2_postcode)

airtext_lm <- lm(total ~ mean_NO2 + usual_res + mean_NO2*usual_res + IMD, data=airtext_no2_postcode_dem2)
summary(airtext_lm)

par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(airtext_lm)

lmtest::bptest(airtext_lm)

airtext_lm_rob <- lmrob(total ~ mean_NO2 + usual_res + mean_NO2*usual_res + IMD, data=airtext_no2_postcode_dem2)
summary(airtext_lm_rob)

#Generate correlation plot to examine relationship between number of Airtext subscribers and 2016 mean annual NO2 concentration

NO2_total_corrplot <- ggscatter(airtext_no2_postcode_dem2, x = "mean_NO2", y = "total", color = "#66128c", 
                                cor.coef = TRUE, cor.method = "pearson", xlab = expression('2016 Mean Annual NO'[2]~'concentration'), 
                                ylab = "Number of Airtext Subscribers") 

NO2_total_corrplot
