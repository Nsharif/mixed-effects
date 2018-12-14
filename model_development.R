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
anova(model_1, model_2)

print(VarCorr(model_2),comp="Variance")
as.data.frame(VarCorr(model_2),comp="Variance")

## model 3. unconditional growth model with one time invariant parameter in the level 2 predictors
model_3 <- lmer(pen_rate 
                ~ c_effective_date_year*firmsize_GT200_LTE500
                + (c_effective_date_year | region_account_number)
                , df_eda_2
                , REML = FALSE)
summary(model_3)
anova(model_2, model_3)

print(VarCorr(model_3),comp="Variance")
as.data.frame(VarCorr(model_3),comp="Variance")

## model 4. unconditional growth model with time invariant and time varying predictors
model_4 <- lmer(pen_rate 
                ~ c_effective_date_year*firmsize_GT200_LTE500 
                + c_kp_prop_blackhisp
                + c_prop_dependents
                + (c_effective_date_year | region_account_number)
                , df_eda_2
                , REML = FALSE)
summary(model_4)
anova(model_3, model_4)

## model 5a. adding discontinuity (pex) only in elevation (and not slope)
model_5a <- lmer(pen_rate 
                 ~ c_effective_date_year*firmsize_GT200_LTE500 
                 + c_kp_prop_blackhisp
                 + c_prop_dependents
                 + pex
                 + (c_effective_date_year | region_account_number)
                 , df_eda_2
                 , REML = FALSE)

summary(model_5a)
anova(model_4, model_5a)

## model 5b. adding discontinuity (pex) only in slope (and not elevation)
model_5b <- lmer(pen_rate
                 ~ c_effective_date_year*firmsize_GT200_LTE500 
                 + c_kp_prop_blackhisp
                 + c_prop_dependents
                 + postpex
                 + (c_effective_date_year | region_account_number)
                 , df_eda_2
                 , REML = FALSE)

summary(model_5b)
anova(model_5a, model_5b)

## model 5c. adding discontinuity (pex) in both elevation and slope
model_5c <- lmer(pen_rate
                 ~ c_effective_date_year*firmsize_GT200_LTE500
                 + c_kp_prop_blackhisp
                 + c_prop_dependents
                 + pex
                 + postpex
                 + (c_effective_date_year | region_account_number)
                 , df_eda_2
                 , REML = FALSE)

summary(model_5c)
anova(model_5a, model_5c)

## model 6. unconditional growth model with additional time-invariant and variant parameters
model_6 <- lmer(pen_rate
                ~ c_effective_date_year*firmsize_GT200_LTE500
                + c_kp_prop_blackhisp
                + c_prop_dependents
                + pex
                + SC
                + CO 
                + GA 
                + HI 
                + MA
                + NW
                + (c_effective_date_year
                   | region_account_number)
                , df_eda_2
                , REML = TRUE)

summary(model_6)
anova(model_5c, model_6)

print(VarCorr(model_6),comp="Variance")
as.data.frame(VarCorr(model_6),comp="Variance")

## model 7. unconditional growth model with additional time-invariant and variant parameters
## included random effects for the time variant predictors
model_7 <- lmer(pen_rate
                ~ c_effective_date_year*firmsize_GT200_LTE500
                + c_kp_prop_blackhisp
                + c_prop_dependents
                + pex
                + SC
                + CO 
                + GA 
                + HI 
                + MA
                + NW
                + (c_effective_date_year + c_kp_prop_blackhisp + c_prop_dependents
                   | region_account_number)
                , df_eda_2
                , REML = FALSE)

summary(model_7)
anova(model_6, model_7)

print(VarCorr(model_7),comp="Variance")
as.data.frame(VarCorr(model_7),comp="Variance")

coef(model_7) # coefficients of both RE and FE
ranef(model_7) # coefficents for only RE
ran_model_7 <- as.data.frame(random.effects(model_7)) # random effects

ran_model_7_intercept <- ran_model_7 %>%  # random effects intercept
  filter(term == '(Intercept)') %>%
  select(grp, condval) %>%
  rename(intercept = condval, region_account_number = grp) 

ran_model_7_time <- ran_model_7 %>% # random effects time
  filter(term == 'c_effective_date_year') %>% 
  select(grp, condval) %>%
  rename(time = condval, region_account_number = grp) 

ran_model_7_prop_blackhisp <- ran_model_7 %>% # random effects proportion black and hisp
  filter(term == 'c_kp_prop_blackhisp') %>% 
  select(grp, condval) %>%
  rename(prop_blackhisp = condval, region_account_number = grp) 

ran_model_7_prop_dependents <- ran_model_7 %>% # random effects proportion dependents
  filter(term == 'c_prop_dependents') %>% 
  select(grp, condval) %>%
  rename(prop_dependents = condval, region_account_number = grp) 

ran_model_7 <- ran_model_7_intercept %>% # random effects
  left_join(.,ran_model_7_time, by = 'region_account_number') %>%
  left_join(.,ran_model_7_prop_blackhisp, by = 'region_account_number') %>%
  left_join(.,ran_model_7_prop_dependents, by = 'region_account_number')

head(ran_model_7)
summary(df_eda_2$kp_group_tenure) # initial status for continuous variables
summary(df_eda_2$prop_blackhisp) # initial status for continuous variables
summary(df_eda_2$prop_dependents) # initial status for continuous variables

## evaluate forecast accuracy using RMSFE
# RMSFE model 1
model_1_rmsfe <- rmsfe(fitted(model_1),df_eda_2$pen_rate)
model_1_rmsfe

# RMSFE model 2
model_2_rmsfe <- rmsfe(fitted(model_2),df_eda_2$pen_rate)
model_2_rmsfe

# RMSFE model 3
model_3_rmsfe <- rmsfe(fitted(model_3),df_eda_2$pen_rate)
model_3_rmsfe

# RMSFE model 4
model_4_rmsfe <- rmsfe(fitted(model_4),df_eda_2$pen_rate)
model_4_rmsfe

# RMSFE model 5a
model_5a_rmsfe <- rmsfe(fitted(model_5a),df_eda_2$pen_rate)
model_5a_rmsfe

# RMSFE model 5b
model_5b_rmsfe <- rmsfe(fitted(model_5b),df_eda_2$pen_rate)
model_5b_rmsfe

# RMSFE model 5c
model_5c_rmsfe <- rmsfe(fitted(model_5c),df_eda_2$pen_rate)
model_5c_rmsfe

# RMSFE model 6
model_6_rmsfe <- rmsfe(fitted(model_6),df_eda_2$pen_rate)
model_6_rmsfe

# RMSFE model 7
model_7_rmsfe <- rmsfe(fitted(model_7),df_eda_2$pen_rate)
model_7_rmsfe

