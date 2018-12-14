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
  select(region_account_number, effective_date_year, pen_rate) %>%
  head(13)

grid.table(table_dataframe)

## examining the within and between variations of the data with empirical growth plots
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

## table view of the dataframe with time-varying and time-invariant predictors
## figure 6
table_dataframe <- df_eda_2 %>%
  select(region_account_number, effective_date_year, pen_rate, region, firm_size, prop_blackhisp, prop_dependents) %>%
  head(10)

grid.table(table_dataframe)

## plot discontinuity (model 5a)
## figure 7
fixef.5a.pex <- fixef(model_5a)
fit.5a.no.pex <- (fixef.5a.pex[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.5a.pex[[2]])
fit.5a.yes.pex <- (fixef.5a.pex[[1]] + fixef.5a.pex[[6]] + df_eda_2$c_effective_date_year[3:7]*fixef.5a.pex[[2]])
plot(df_eda_2$effective_date_year[1:7], fit.5a.no.pex, ylim=c(.22, .32), type="l", pch = 0, 
     ylab="Penetration Rate", xlab="Effective Date Year", col=4) 
lines(df_eda_2$effective_date_year[3:7], fit.5a.yes.pex, type="l", pch=1, col="cornflowerblue", lty=2)
segments(2014, fixef.5a.pex[[1]] + fixef.5a.pex[[6]] + fixef.5a.pex[[2]]*2
         , 2014, fixef.5a.pex[[1]] + fixef.5a.pex[[2]]*2, col="cornflowerblue", lty=2)

## plot discontinuity (model 5b)
## figure 8
fixef.5b.pex <- fixef(model_5b)
fit.5b.no.pex <- (fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.5b.pex[[2]])
fit.5b.yes.pex <- (fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.5b.pex[[2]])
fit.5b.yes.pex[3] <- fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[3]*fixef.5b.pex[[2]] + df_eda_2$c_effective_date_year[1]*fixef.5b.pex[[6]]
fit.5b.yes.pex[4] <- fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[4]*fixef.5b.pex[[2]] + df_eda_2$c_effective_date_year[2]*fixef.5b.pex[[6]]
fit.5b.yes.pex[5] <- fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[5]*fixef.5b.pex[[2]] + df_eda_2$c_effective_date_year[3]*fixef.5b.pex[[6]]
fit.5b.yes.pex[6] <- fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[6]*fixef.5b.pex[[2]] + df_eda_2$c_effective_date_year[4]*fixef.5b.pex[[6]]
fit.5b.yes.pex[7] <- fixef.5b.pex[[1]] + df_eda_2$c_effective_date_year[7]*fixef.5b.pex[[2]] + df_eda_2$c_effective_date_year[5]*fixef.5b.pex[[6]]
plot(df_eda_2$effective_date_year[1:7], fit.5b.no.pex, ylim=c(.20, .30), type="l", pch = 0, 
     ylab="Penetration Rate", xlab="Effective Date Year", col=4) 
lines(df_eda_2$effective_date_year[1:7], fit.5b.yes.pex, type="l", pch=1, col="cornflowerblue", lty=2)

## plot discontinuity (model 5c)
## figure 9
fixef.5c.pex <- fixef(model_5c)
fit.5c.no.pex <- (fixef.5c.pex[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.5c.pex[[2]])
fit.5c.yes.pex <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]] + df_eda_2$c_effective_date_year[3:7]*fixef.5c.pex[[2]])
fit.5c.yes.pex[1] <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]]) + df_eda_2$c_effective_date_year[3]*fixef.5c.pex[[2]] + df_eda_2$c_effective_date_year[1]*fixef.5b.pex[[6]]
fit.5c.yes.pex[2] <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]]) + df_eda_2$c_effective_date_year[4]*fixef.5c.pex[[2]] + df_eda_2$c_effective_date_year[2]*fixef.5b.pex[[6]]
fit.5c.yes.pex[3] <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]]) + df_eda_2$c_effective_date_year[5]*fixef.5c.pex[[2]] + df_eda_2$c_effective_date_year[3]*fixef.5b.pex[[6]]
fit.5c.yes.pex[4] <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]]) + df_eda_2$c_effective_date_year[6]*fixef.5c.pex[[2]] + df_eda_2$c_effective_date_year[4]*fixef.5b.pex[[6]]
fit.5c.yes.pex[5] <- (fixef.5c.pex[[1]] + fixef.5a.pex[[6]]) + df_eda_2$c_effective_date_year[7]*fixef.5c.pex[[2]] + df_eda_2$c_effective_date_year[5]*fixef.5b.pex[[6]]
plot(df_eda_2$effective_date_year[1:7], fit.5c.no.pex, ylim=c(.22, .32), type="l", pch = 0, 
     ylab="Penetration Rate", xlab="Effective Date Year", col=4) 
lines(df_eda_2$effective_date_year[3:7], fit.5c.yes.pex, type="l", pch=1, col="cornflowerblue", lty=2)
segments(2014, fixef.5c.pex[[1]] + fixef.5c.pex[[6]] + fixef.5c.pex[[2]]*2
         , 2014, fixef.5c.pex[[1]] + fixef.5c.pex[[2]]*2, col="cornflowerblue", lty=2)

## plot unconditional growth model (model 2)
## figure 10
fixef.2 <- fixef(model_2)
fit.2 <- fixef.2[[1]] + df_eda_2$c_effective_date_year[1:7]*fixef.2[[2]]
plot(df_eda_2$effective_date_year[1:7], fit.2, ylim=c(.22, .3), type="b", 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year")   

## plot final model (model 7 by region)
## figure 11
fixef.7 <- fixef(model_7)
fit.sc <- fixef.7[[1]] + fixef.7[[7]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]
fit.co <- fixef.7[[1]] + fixef.7[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]
fit.ga <- fixef.7[[1]] + fixef.7[[9]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]
fit.hi <- fixef.7[[1]] + fixef.7[[10]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]
fit.ma <- fixef.7[[1]] + fixef.7[[11]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]
fit.nw <- fixef.7[[1]] + fixef.7[[12]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7[[2]]

plot(df_eda_2$effective_date_year[1:7], fit.sc, ylim=c(.0, .40), type="b", pch = 0, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=2) # red 
lines(df_eda_2$effective_date_year[1:7], fit.co, type="b", pch=1, col=4) # blue
lines(df_eda_2$effective_date_year[1:7], fit.ga, type="b", pch=2, col='yellow') # grey
lines(df_eda_2$effective_date_year[1:7], fit.hi, type="b", pch=3, col=3) # green
lines(df_eda_2$effective_date_year[1:7], fit.ma, type="b", pch=6, col=6) # pink
lines(df_eda_2$effective_date_year[1:7], fit.nw, type="b", pch=7, col=1) # black
legend("topright", legend = c("SC","CO","GA","HI","MA","NW"),
       pch = c(0,1,2,3,6,7)) 

## table view of the dataframe
## figure 12
table_dataframe <- ran_model_7 %>%
  head(5)

grid.table(table_dataframe)

## plot random effects by region
## figure 13 
fixef.7.re <- fixef(model_7)
fit.co <- fixef.7.re[[1]] + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7.re[[2]]
fit.co_1000 <- (fixef.7.re[[1]]+0.211) + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.re[[2]]-0.037)
fit.co_10650 <- (fixef.7.re[[1]]+0.015) + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.re[[2]]-0.017)
fit.co_1071 <- (fixef.7.re[[1]]+0.137) + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.re[[2]]-0.010)
fit.co_1095 <- (fixef.7.re[[1]]+0.162) + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.re[[2]]-0.026)
fit.co_11551 <- (fixef.7.re[[1]]-0.054) + fixef.7.re[[8]] + 
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.re[[2]]-0.004)

plot(df_eda_2$effective_date_year[1:7], fit.co, ylim=c(.0, .6), type="b", pch = 3, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col=1) 
lines(df_eda_2$effective_date_year[1:7], fit.co_1000, type="l", pch=1, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_10650, type="l", pch=2, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_1071, type="l", pch=3, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_1095, type="l", pch=6, col=3)
lines(df_eda_2$effective_date_year[1:7], fit.co_11551, type="l", pch=7, col=3)
legend("topright", legend = c("+ Equals Group Average"))

## plot counterfactuals (model 7)
## figure 14
fixef.7.cf <- fixef(model_7)
fit.lte50.m <- fixef.7.cf[[1]] + fixef.7.cf[[12]] + 
  df_eda_2$c_effective_date_year[1:7]*fixef.7.cf[[2]]
fit.lte50.h <- fixef.7.cf[[1]] + fixef.7.cf[[12]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.7.cf[[2]] +
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.cf[[4]]*.25) +
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.cf[[5]]*.25)
fit.lte50.l <- fixef.7.cf[[1]] + fixef.7.cf[[12]] +
  df_eda_2$c_effective_date_year[1:7]*fixef.7.cf[[2]] +
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.cf[[4]]*-.25) +
  df_eda_2$c_effective_date_year[1:7]*(fixef.7.cf[[5]]*-.25)

plot(df_eda_2$effective_date_year[1:7], fit.lte50.m, ylim=c(0, .6), type="b", pch = 0, 
     ylab="Predicted Penetration Rate", xlab="Effective Date Year", col='green')  
lines(df_eda_2$effective_date_year[1:7], fit.lte50.h, type="b", pch=1, col='blue')
lines(df_eda_2$effective_date_year[1:7], fit.lte50.l, type="b", pch=2, col='red')
legend("topright", legend = c("No Counterfactual","Positive Scenario","Negative Scenario"),
       pch = c(0,1,2,3)) 

## creating the residuals (epsilon.hat)
## figure 15
resid <- residuals(model_7)
qqnorm(resid)

## extracting the random effects of model 3
## figure 16 and 17
qqnorm(ran_model_7_intercept$intercept)
qqnorm(ran_model_7_time$time)

## creating the standardized residual (std epsilon.hat)
## figure 18
resid.std <- resid/sd(resid)
plot(df_eda_2$region_account_number, resid.std, ylim=c(-10, 10), ylab="std epsilon hat")
abline(h=0)

## homoskedasticity plots
## figure 19
plot(df_eda_2$effective_date_year, resid, ylim=c(-1, 1), ylab="epsilon.hat", 
     xlab="Effective Date Year")
abline(h=0)

## plot the relationship between fitted and actuals. evaluate the correlation between fitted and actural 
## figure 20
yhat_pen_rate <- round(fitted(model_7),3) 
df_eda_3 <- cbind(df_eda_2,yhat_pen_rate) 

ggplot(data = df_eda_3) +
  geom_point(mapping = aes(pen_rate,yhat_pen_rate)) +
  geom_smooth(mapping = aes(pen_rate,yhat_pen_rate)) +
  scale_x_continuous(limits = c(0,.9)) +
  scale_y_continuous(limits = c(0,.9)) +
  labs(x="Pen Rate Fitted",y="Pen Rate Actuals")