library(MASS)
library(psych)
library(nlme)
library(openxlsx)
library(sqldf)
library(stringr)
library(dplyr)
library(readxl)
library(ivreg)

data = as.data.frame(read_excel("C:/Users/yushy/Documents/데이터/pannel2022/24th_var07.xlsx",col_names = TRUE,na="NA",.name_repair = 'universal' ))

df <- data[complete.cases(data$house_workhour), ] 
df2 <- data[complete.cases(data$house_workhour), ] 
#확인
summary(df$house_workhour) 
hist(df$house_workhour)
#house_workhour 로그화
#df = transform(df, house_workhour_log = log(house_workhour + 1))
#sum(is.na(df$house_workhour_log))
#hist(df$house_workhour_log)

#workhour 정규화
df = transform(df, house_workhour = scale(house_workhour))
sum(is.na(df$house_workhour_z))
hist(df$house_workhour_z)

par( mfrow = c(1,3))
qqnorm(df$house_workhour, main="Q-Q plot of workhour")
qqline(df$house_workhour)
 
qqnorm(df$house_workhour_log, main="Q-Q plot of workhour_log")
qqline(df$house_workhour_log)

qqnorm(df$house_workhour_z, main="Q-Q plot of workhour_z")
qqline(df$house_workhour_z)

##y는 정규화를 해주기로..!
drop(df$house_workhour_log)


#-----------------------------------------------#
#-----------------------------------------------#
#-----------------------------------------------#

table(df$job_difficulty)
#0 : 9380 1:1837

hist(df$job_difficulty,seq=)
table(df$house)


#연속형 변수들 정규화
#asset_income trans_income realtyasset_variable	finasset_variable
#house_income_variable = 가구근로소득
df = transform(df, house_income_variable = scale(log(house_income_variable+1)))
df = transform(df, asset_income = scale(log(asset_income+1)))
df = transform(df, trans_income = scale(log(trans_income+1)))
df = transform(df, realtyasset_variable = scale(realtyasset_variable))
df = transform(df, finasset_variable = scale(finasset_variable))

#나이는 로그
df = transform(df, age = log(age+1))

#확인
psych::describe(df)


#glm 돌려보자고

tmt_res = glm(house_workhour~1,data = df, family='gaussian')

# Call:  glm(formula = house_workhour ~ 1, family = "gaussian", data = df)
# 
# Coefficients:
#   (Intercept)  
# -8.411e-05  
# 
# Degrees of Freedom: 11206 Total (i.e. Null);  11206 Residual
# Null Deviance:	    11210 
# Residual Deviance: 11210 	AIC: 31810

tmt_res2 = glm(house_workhour~trt,data = df, family='gaussian')

# Call:  glm(formula = house_workhour ~ trt, family = "gaussian", data = df)
# 
# Coefficients:
#   (Intercept)          trt  
# 0.03934     -0.81675  
# 
# Degrees of Freedom: 11206 Total (i.e. Null);  11205 Residual
# Null Deviance:	    11210 
# Residual Deviance: 10870 	AIC: 31460

tmt_res3 = glm(house_workhour~job_difficulty+house_income_variable+
                 number_household+trt+age+gender+edu+asset_income+
                 trans_income+realtyasset_variable+finasset_variable,
               data = df, family='gaussian')

l_bound = glm(house_workhour~1,data=df)
u_bound = glm(house_workhour~job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable
              ,data=df)
step(glm(house_workhour~1,data=df),direction = 'forward',
     scope = list(lower=l_bound,upper=u_bound))

sum(is.na(df))
df=na.omit(df)

#로그 변환 안했을 때
# glm(formula = house_workhour ~ job_difficulty + house_income_variable + 
#       number_household + trt + age + gender + edu + asset_income + 
#       trans_income + realtyasset_variable + finasset_variable, 
#     family = "gaussian", data = df2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -428.54   -90.30   -18.63    75.11  1013.06  
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)            2.372e+02  1.357e+01  17.487  < 2e-16 ***
#   job_difficulty        -4.212e+01  4.342e+00  -9.701  < 2e-16 ***
#   house_income_variable -4.701e-06  3.682e-06  -1.277  0.20177
# number_household       4.572e+01  1.265e+00  36.134  < 2e-16 ***
#   trt                   -5.221e+01  6.710e+00  -7.782 7.78e-15 ***
#   age                   -2.942e+00  1.183e-01 -24.869  < 2e-16 ***
#   gender                 1.481e+00  3.492e+00   0.424  0.67150
# edu                   -1.659e+00  1.191e+00  -1.393  0.16374
# asset_income          -7.225e-03  6.196e-03  -1.166  0.24360
# trans_income          -2.256e-02  7.924e-03  -2.847  0.00443 **
#   realtyasset_variable  -2.263e-04  3.466e-05  -6.530 6.84e-11 ***
#   finasset_variable      4.806e-04  1.854e-04   2.592  0.00956 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 21176.08)
# 
# Null deviance: 338386163  on 11206  degrees of freedom
# Residual deviance: 237066177  on 11195  degrees of freedom
# AIC: 143447
# 
# Number of Fisher Scoring iterations: 2

#이상치가 안 잡혀서 모델 결과가 아예 다르게 나옴.


fres = glm(house_workhour ~ number_household + age + trans_income + 
             house_income_variable + realtyasset_variable + job_difficulty + 
             trt + asset_income + gender + finasset_variable+edu,
           data=df, family='gaussian')

fres2 = glm(house_workhour ~ number_household + age + trans_income + 
              house_income_variable + realtyasset_variable + job_difficulty + 
              trt + asset_income + gender + finasset_variable,
            data=df, family='gaussian')


#2lsl
m_iv <- ivreg(house_workhour ~ trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable |
                iv_trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable, data = df2)

m_iv2 <- ivreg(house_workhour ~ trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable |
                iv_trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable, data = df)

