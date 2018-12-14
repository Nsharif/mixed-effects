##### Author: Naveed Sharif
##### Data Source: Kaiser Permanente
##### Topic: Mixed Effects Model
##### Section: Data Preperation
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

## functions
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

rmsfe <- function(y_hat,y_actual){
  round_y_hat <- round(y_hat,3)
  result <- (((y_hat - y_actual)^2)^0.5)
  expected_result <- mean(result)
  round_result <- round(expected_result,3)
  return(round_result)
}

## load dataframe into enviornment
df_eda <- read.csv("C:\\Users\\Naveed\\Desktop\\Employment\\Kaiser\\Data\\KP_NationalAccount.csv", stringsAsFactors = TRUE)
glimpse(df_eda, list.len=200)
mean(is.na(df_eda$kp_c_rate_lowest))

## remove observations that are outside of the wave length for the lda study
df_eda %>% # we need at least 2 waves of data per each group
  group_by(region_account_number) %>% 
  summarise(group_avg_count_obs = n()) %>%
  ggplot(aes(group_avg_count_obs)) +
  geom_histogram()

df_eda_2 <- df_eda %>% # 599 groups are apart of the study
  group_by(region_account_number) %>% 
  summarise(group_avg_count_obs = n()) %>%
  filter(group_avg_count_obs >= 2) %>%
  mutate(dummy = 1)

df_eda_2 <- left_join(df_eda, df_eda_2, by = 'region_account_number')
glimpse(df_eda_2)
sum(is.na(df_eda_2$dummy))

df_eda_2 <- df_eda_2 %>%
  filter(dummy == 1)

df_eda_2 %>% # check to confirm
  group_by(region_account_number) %>% 
  summarise(group_avg_count_obs = n()) %>%
  ggplot(aes(group_avg_count_obs)) +
  geom_histogram()

df_eda_2 <- within(df_eda_2, remove(dummy))

## bin firm size
hist(df_eda_2$eligible)

df_eda_2$firm_size <- ifelse(df_eda_2$eligible <= 50, 'LTE50',
                             ifelse((df_eda_2$eligible > 50 & df_eda_2$eligible <= 200),'GT50_LTE200',
                                    ifelse((df_eda_2$eligible > 200 & df_eda_2$eligible <= 500),'GT200_LTE500',
                                           ifelse((df_eda_2$eligible > 500 & df_eda_2$eligible <= 1000),'GT500_LTE1000',
                                                  ifelse((df_eda_2$eligible > 1000 & df_eda_2$eligible <= 3000),'GT1000_LTE3000','GT3000')))))

prop.table(table(df_eda_2$firm_size))

## round penetration rate
df_eda_2$pen_rate <- round(df_eda_2$pen_rate,3)

## center continuous variables used in the model
summary(df_eda_2$log_mrn_avg_riskscore)
summary(df_eda_2$kp_group_tenure)
summary(df_eda_2$prop_blackhisp)
summary(df_eda_2$prop_dependents)
summary(df_eda_2$eligible)

df_eda_2$c_effective_date_year <- (df_eda_2$effective_date_year-2012)
df_eda_2$c_log_mrn_avg_riskscore <- (df_eda_2$log_mrn_avg_riskscore-mean(df_eda_2$log_mrn_avg_riskscore))
df_eda_2$c_kp_group_tenure <- (df_eda_2$kp_group_tenure-mean(df_eda_2$kp_group_tenure))
df_eda_2$c_kp_prop_blackhisp <- (df_eda_2$prop_blackhisp-mean(df_eda_2$prop_blackhisp))
df_eda_2$c_prop_dependents <- (df_eda_2$prop_dependents-mean(df_eda_2$prop_dependents))

df_eda_2$c3q_log_mrn_avg_riskscore <- (df_eda_2$log_mrn_avg_riskscore-0.45108)
df_eda_2$c3q_kp_group_tenure <- (df_eda_2$kp_group_tenure-26.20)
df_eda_2$c3q_kp_prop_blackhisp <- (df_eda_2$prop_blackhisp-0.4700)
df_eda_2$c3q_prop_dependents <- (df_eda_2$prop_dependents-0.370)

df_eda_2$c1q_log_mrn_avg_riskscore <- (df_eda_2$log_mrn_avg_riskscore-(-0.01005))
df_eda_2$c1q_kp_group_tenure <- (df_eda_2$kp_group_tenure-9.60)
df_eda_2$c1q_kp_prop_blackhisp <- (df_eda_2$prop_blackhisp-0.2300)
df_eda_2$c1q_prop_dependents <- (df_eda_2$prop_dependents-0.250)

## create dummy variables for the effective year and firm size (cleaner labels in summary table)
df_eda_2 <- as.data.frame(df_eda_2)
df_eda_2 <- cbind(df_eda_2,dummies::dummy(df_eda_2$region, sep = "_"))

for( i in colnames(df_eda_2)){
  colnames(df_eda_2)[which(colnames(df_eda_2)==i)] = gsub("df_eda_2_","",(i))
}

df_eda_2 <- cbind(df_eda_2,dummies::dummy(df_eda_2$firm_size, sep = "_"))

for( i in colnames(df_eda_2)){
  colnames(df_eda_2)[which(colnames(df_eda_2)==i)] = gsub("df_eda_2_","firmsize_",(i))
}

## adding discontinuity variables based on the pex variable
df_eda_2 <- df_eda_2 %>%
  group_by(region_account_number) %>%
  mutate(x=ifelse(lag(pex)==1,effective_date_year-lag(effective_date_year),0)) %>%
  mutate(postpex=cumsum(coalesce(x,0))+x*0) %>%
  mutate(postpex=ifelse(is.na(postpex),0,postpex)) %>%
  select(-x)

hist(df_eda_2$postpex)
df_eda_2 <- as.data.frame(df_eda_2)



