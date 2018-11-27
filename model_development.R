##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Mixed Effects Model
##### Section: Model Development
##### Date: 11/05/2018

### load dependent libraries
library(tidyverse)
library(data.table)
library(dummies)
library(lattice)
library(Matrix)
library(lme4)
library(nlme)

## adjust scientific notation
options(scipen=999) 

## model 1. unconditional means model
model_1 <- lmer(pen_rate 
                ~ 1 
                + (1 | region_account_number)
                , df_eda_2
                , REML = FALSE)
summary(model_1)

print(VarCorr(model_1),comp="Variance")
as.data.frame(VarCorr(model_1),comp="Variance")

## model 2. unconditional growth model
model_2 <- lmer(pen_rate 
                ~ c_effective_date_year 
                + (c_effective_date_year | region_account_number)
                , df_eda_2
                , REML = FALSE)
summary(model_2)

print(VarCorr(model_2),comp="Variance")
as.data.frame(VarCorr(model_2),comp="Variance")

## model 3. unconditional growth model with additional parameters
model_3 <- lmer(pen_rate 
                ~ c_effective_date_year*SC
                + c_effective_date_year*CO 
                + c_effective_date_year*GA 
                + c_effective_date_year*HI 
                + c_effective_date_year*MA
                + c_effective_date_year*NW 
                + c_effective_date_year*firmsize_LTE50
                + c_effective_date_year*firmsize_GT200_LTE500
                + c_effective_date_year*firmsize_GT1000_LTE3000
                + c_effective_date_year*firmsize_GT3000
                + c_effective_date_year*c_kp_group_tenure
                + c_effective_date_year*c_log_mrn_avg_riskscore
                + c_effective_date_year*c_kp_prop_blackhisp
                + c_effective_date_year*c_prop_dependents
                + (c_effective_date_year
                   | region_account_number)
                , df_eda_2
                , REML = FALSE)
summary(model_3)

print(VarCorr(model_3),comp="Variance")
as.data.frame(VarCorr(model_3),comp="Variance")

coef(model_3) # coefficients of both RE and FE
ranef(model_3)
ran_model_3 <- as.data.frame(random.effects(model_3)) # random effects

ran_model_3_intercept <- ran_model_3 %>%  # random effects intercept
  filter(term == '(Intercept)') %>%
  select(grp, condval) %>%
  rename(intercept = condval, region_account_number = grp) 

ran_model_3_slope <- ran_model_3 %>% # random effects slope
  filter(term == 'c_effective_date_year') %>% 
  select(grp, condval) %>%
  rename(slope = condval, region_account_number = grp) 

ran_model_3 <- ran_model_3_intercept %>% # random effects
  left_join(.,ran_model_3_slope, by = 'region_account_number')

summary(df_eda_2$kp_group_tenure) # initial status for continuous variables
summary(df_eda_2$log_mrn_avg_riskscore) # initial status for continuous variables
summary(df_eda_2$prop_blackhisp) # initial status for continuous variables
summary(df_eda_2$prop_dependents) # initial status for continuous variables
