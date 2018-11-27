##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Mixed Effects Model
##### Section: Charts and Tables
##### Date: 11/05/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)
library(lattice)
library(Matrix)
library(lme4)
library(nlme)
library(gridExtra)
library(grid)

## adjust scientific notation
options(scipen=999) 

## table view of the dataframe
## figure 1
table_dataframe <- df_eda_2 %>%
  select(region_account_number, effective_date_year, pen_rate, region, ri_mean, firm_size) %>%
  head(13)

grid.table(table_dataframe)

## examining the within and between variations of the data
## figure 2
set.seed(1234)
sample_20 <- with(df_eda_2, ## there are two many groups therefore sample 20
                  sample(unique(region_account_number),20)) 

sample_20 <- df_eda_2[is.element(df_eda_2$region_account_number, sample_20),]

xyplot(pen_rate ~ effective_date_year | region_account_number, data = sample_20, 
       main = "", 
       xlab = "Effective Date Year (Time)", ylab = "Penetration Rate (subscribers/eligibles)",
       panel = function(x,y){
         panel.points(x,y)
         panel.lmline(x,y,lty=1,lwd=1,col="darkgray")
       })

## fitting the linear model by id
## region equals scal and firm size equals greater than 1000 and less than or equal to 3000
## figure 4A 
SC_GT1000_LTE3000 <- df_eda_2 %>%
  filter((SC == 1) & (firmsize_GT1000_LTE3000 == 1))

fit_SC_GT1000_LTE3000 <- by(SC_GT1000_LTE3000
                            , SC_GT1000_LTE3000$region_account_number
                            , function(data) fitted.values(lm(pen_rate ~ effective_date_year, data=data)))  
fit_SC_GT1000_LTE3000 <- unlist(fit_SC_GT1000_LTE3000)
names(fit_SC_GT1000_LTE3000) <- NULL

# plotting the linear fit by id
interaction.plot(SC_GT1000_LTE3000$effective_date_year, SC_GT1000_LTE3000$region_account_number, fit_SC_GT1000_LTE3000
                 , main=""
                 , xlab="Effective Date Year"
                 , ylab="Penetration Rate"
                 , ylim=c(0, 1)
                 , legend = FALSE)

## fitting the linear model by id
## region equals mid-atlantic and firm size equals greater than 1000 and less than or equal to 3000
## figure 4B
MA_GT1000_LTE3000 <- df_eda_2 %>%
  filter((MA == 1) & (firmsize_GT1000_LTE3000 == 1))

fit_MA_GT1000_LTE3000 <- by(MA_GT1000_LTE3000
                            , MA_GT1000_LTE3000$region_account_number
                            , function(data) fitted.values(lm(pen_rate ~ effective_date_year, data=data)))  
fit_MA_GT1000_LTE3000 <- unlist(fit_MA_GT1000_LTE3000)
names(fit_MA_GT1000_LTE3000) <- NULL

# plotting the linear fit by id
interaction.plot(MA_GT1000_LTE3000$effective_date_year, MA_GT1000_LTE3000$region_account_number, fit_MA_GT1000_LTE3000
                 , main=""
                 , xlab="Effective Date Year"
                 , ylab="Penetration Rate"
                 , ylim=c(0, 1)
                 , legend = FALSE)

## plot unconditional growth model (model 2)
## figure 6
fixef.2 <- fixef(model_2)
fit.2 <- fixef.2[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.2[[2]]
plot(df_eda_2$effective_date_year[1:7], fit.2, ylim=c(.22, .3), type="b", 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year")   

## plot final model (model 3 by region)
## figure 7
fixef.3 <- fixef(model_3)
fit.sc <- fixef.3[[1]] + fixef.3[[3]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[17]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co <- fixef.3[[1]] + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.ga <- fixef.3[[1]] + fixef.3[[5]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[19]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.hi <- fixef.3[[1]] + fixef.3[[6]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[20]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.ma <- fixef.3[[1]] + fixef.3[[7]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[21]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.nw <- fixef.3[[1]] + fixef.3[[8]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]

plot(df_eda_2$effective_date_year[1:7], fit.sc, ylim=c(.0, .40), type="b", pch = 0, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=2) # red 
lines(df_eda_2$effective_date_year[1:7], fit.co, type="b", pch=1, col=4) # blue
lines(df_eda_2$effective_date_year[1:7], fit.ga, type="b", pch=2, col=8) # grey
lines(df_eda_2$effective_date_year[1:7], fit.hi, type="b", pch=3, col=3) # green
lines(df_eda_2$effective_date_year[1:7], fit.ma, type="b", pch=6, col=6) # pink
lines(df_eda_2$effective_date_year[1:7], fit.nw, type="b", pch=7, col=1) # black
legend("topright", legend = c("SC","CO","GA","HI","MA","NW"),
       pch = c(0,1,2,3,6,7)) 

## plot final model (model 3 by firm size for NW)
## figure 8
fit.lte50 <- fixef.3[[1]] + fixef.3[[9]] + fixef.3[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[23]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]]
fit.gt200lte500 <- fixef.3[[1]] + fixef.3[[10]] + fixef.3[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[24]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]]
fit.gt1000lte3000 <- fixef.3[[1]] + fixef.3[[11]] + fixef.3[[8]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]]
fit.gt3000 <- fixef.3[[1]] + fixef.3[[12]] + fixef.3[[8]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[26]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]]

plot(df_eda_2$effective_date_year[1:7], fit.lte50, ylim=c(0, .8), type="b", pch = 0, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=2) # red 
lines(df_eda_2$effective_date_year[1:7], fit.gt200lte500, type="b", pch=1, col=4) # blue
lines(df_eda_2$effective_date_year[1:7], fit.gt1000lte3000, type="b", pch=2, col=8) # grey
lines(df_eda_2$effective_date_year[1:7], fit.gt3000, type="b", pch=3, col=3) # green
legend("topright", legend = c("LTE 50","GT 200 & LTE 500","GT 1000 & LTE 3000","GT 3000"),
       pch = c(0,1,2,3)) 

## table view of the dataframe
## figure 9
table_dataframe <- ran_model_3 %>%
  head(5)

grid.table(table_dataframe)

## plot random effects by region
## figure 10 
fit.co <- fixef.3[[1]] + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co_1000 <- (fixef.3[[1]]+0.27803600) + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[2]]-0.030714323) +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co_10650 <- (fixef.3[[1]]-0.03463230) + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[2]]-0.016241604) +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co_1071 <- (fixef.3[[1]]+0.15011018) + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[2]]-0.004589831) +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co_1095 <- (fixef.3[[1]]+0.25560047) + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[2]]-0.025977470) +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]
fit.co_11551 <- (fixef.3[[1]]+0.01081298) + fixef.3[[4]] + fixef.3[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[2]]-0.004351499) +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[18]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[25]]

plot(df_eda_2$effective_date_year[1:7], fit.co, ylim=c(.0, .6), type="b", pch = 3, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=1) 
lines(df_eda_2$effective_date_year[1:7], fit.co_1000, type="l", pch=1, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_10650, type="l", pch=2, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_1071, type="l", pch=3, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_1095, type="l", pch=6, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_11551, type="l", pch=7, col=3)
legend("topright", legend = c("+ Equals Group Average"))

## plot final model (model 3 by NW, lte 50, and by counterfactual on prop black/hisp)
## figure 11
fit.lte50.m <- fixef.3[[1]] + fixef.3[[9]] + fixef.3[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[23]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]]
fit.lte50.h <- fixef.3[[1]] + fixef.3[[9]] + fixef.3[[8]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[23]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]] +
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[29]]*1.5) +
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[30]]*1.5)
fit.lte50.l <- fixef.3[[1]] + fixef.3[[9]] + fixef.3[[8]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[2]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[23]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.3[[22]] +
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[29]]*-1.5) +
  df_eda_2$c_effective_date_year[1:7]*(fixef.3[[30]]*-1.5)

plot(df_eda_2$effective_date_year[1:7], fit.lte50.m, ylim=c(0, .8), type="b", pch = 0, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=2) # red 
lines(df_eda_2$effective_date_year[1:7], fit.lte50.h, type="b", pch=1, col=4) # blue
lines(df_eda_2$effective_date_year[1:7], fit.lte50.l, type="b", pch=2, col=8) # grey
legend("topright", legend = c("No Counterfactual","Positive Scenario","Negative Scenario"),
       pch = c(0,1,2,3)) 

## creating the residuals (epsilon.hat)
## figure 12
resid <- residuals(model_3)
qqnorm(resid)

## extracting the random effects of model 3
## figure 13 and 14
qqnorm(ran_model_3_intercept$intercept)
qqnorm(ran_model_3_slope$slope)

## creating the standardized residual (std epsilon.hat)
## figure 15
resid.std <- resid/sd(resid)
plot(df_eda_2$region_account_number, resid.std, ylim=c(-10, 10), ylab="std epsilon hat")
abline(h=0)

## homoskedasticity plots
## figure 16
plot(df_eda_2$effective_date_year, resid, ylim=c(-1, 1), ylab="epsilon.hat", 
     xlab="Effective Date Year")
abline(h=0)

## evaluate forecast accuracy using RMSFE
## cbind fitted values to the original dataset to evaluate accuracy of model
yhat_pen_rate <- round(fitted(model_3),3)
df_eda_3 <- cbind(df_eda_2,yhat_pen_rate)

df_eda_3 <- df_eda_3 %>%
  select(region_account_number, effective_date_year, pen_rate, yhat_pen_rate)

head(df_eda_3)

df_eda_3 %>%
  filter(yhat_pen_rate < 0) # the model is not bounded between 0 and 1. 

# RMSFE
RMSFE <- mean((((df_eda_3$yhat_pen_rate - df_eda_3$pen_rate)^2)^0.5))
round(RMSFE,3)

## plot the relationship between fitted and actuals. evaluate the correlation between fitted and actural 
## figure 17
ggplot(data = df_eda_3) +
  geom_point(mapping = aes(pen_rate,yhat_pen_rate)) +
  geom_smooth(mapping = aes(pen_rate,yhat_pen_rate)) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x="Pen Rate Fitted",y="Pen Rate Actuals")

