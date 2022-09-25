rm(list=ls(all=TRUE))

install.packages("modelsummary", type="source")
#install.packages('mvtnorm',type='mac.binary')

library(modelsummary)
library(readxl)
library(MatchIt)
library(mvtnorm)
library(DOS2)
library(ivreg)

labor24<-read_excel("/Users/minkyoung/Documents/data/Labor/data/24th_var07.xlsx")
## checking covariate balance between treated and control subjects. 
labor24=na.omit(labor24)

labor24 = transform(labor24, house_workhour = scale(house_workhour))
labor24 = transform(labor24, house_income_variable = scale(log(house_income_variable+1)))
labor24 = transform(labor24, asset_income = scale(log(asset_income+1)))
labor24 = transform(labor24, trans_income = scale(log(trans_income+1)))
labor24 = transform(labor24, realtyasset_variable = scale(realtyasset_variable))
labor24 = transform(labor24, finasset_variable = scale(finasset_variable))
labor24 = transform(labor24, age = log(age+1))





tmt_res3 = glm(house_workhour~job_difficulty+house_income_variable+
                 number_household+trt+age+gender+edu+asset_income+
                 trans_income+realtyasset_variable+finasset_variable,
               data = labor24, family='gaussian')


m_ols <- lm(house_workhour ~ trt + job_difficulty+house_income_variable+
              number_household+trt+age+gender+edu+asset_income+
              trans_income+realtyasset_variable+finasset_variable,
            data = labor24)
summary(m_ols)


m_iv <- ivreg(house_workhour ~ iv_trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable |
                iv_trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable, data = labor24)

m_iv <- ivreg(house_workhour ~ trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable |
                iv_trt + job_difficulty+house_income_variable+
                number_household+trt+age+gender+edu+asset_income+
                trans_income+realtyasset_variable+finasset_variable, data = labor24)


summary(m_iv)

modelplot(m_list)

modelplot(m_list, coef_omit = "Intercept|experience")


m_list <- list(OLS = m_ols, IV = m_iv)
msummary(m_list)



summary.labor24.exp = aggregate(labor24[,3:14], list(labor24$trt), FUN = mean) 
colnames(summary.labor24.exp)[1] = "Treatment"
summary.labor24.exp

ps.fit = glm(trt ~ house_income_variable + number_household + age +
               gender + edu + asset_income + trans_income + realtyasset_variable + finasset_variable, family=binomial, x=T, data=labor24)
summary(ps.fit)


est.ps = predict(ps.fit, type = "response")
hist(est.ps[labor24$trt==0], col = rgb(1,0,0,0.2), xlab = "Propensity Score", main = "Tr")
hist(est.ps[labor24$trt==1], col = rgb(0,0,1,0.2), add = T)





summary(labor24)
