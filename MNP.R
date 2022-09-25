stock_mnp = read.csv('C:/Users/User/Desktop/논문/stock.csv',header = T, sep=",",stringsAsFactors=T)

#반응변수 중 필요 없는 0(기타)제외
stock_mnp = subset(stock_mnp, purpose!="0")

#모델에 들어가는 변수 중 factor로 변환
stock_mnp$home = as.factor(stock_mnp$home)
stock_mnp$job = as.factor(stock_mnp$job)
stock_mnp$house_t=as.factor(stock_mnp$house_t)

#income 표준화
stock_mnp = transform(stock_mnp,income = scale(income))


stock_mnp$purpose=as.character(stock_mnp$purpose)

#SN=Start N, EN= End N
SN=17000
EN=40000

#기존의 mnp 모델에 넣고 돌려보기
library(MNP)

#res1 <- mnp(purpose ~ age  + house_t + home + job + income  + home*house_t + house_t * job  , data = stock_mnp,verbose = TRUE,
 #           n.draws = 50000)
res1<-mnp(purpose ~ age  + house_t + home + job + income, 
          data = stock_mnp,verbose = TRUE,n.draws = EN)

res2 <- mnp(purpose ~ age  + house_t + home + job + income  + home*house_t + house_t * job  , data = stock_mnp,verbose = TRUE,
            cov.start = matrix(0.5, ncol=3, nrow=3) + diag(0.5, 3),n.draws = EN ,burnin = 5000)

res3 <- mnp(purpose ~ age  + house_t + home + job + income  + home*house_t + house_t * job  , data = stock_mnp,verbose = TRUE,
            cov.start = matrix(0.9, ncol=3, nrow=3) + diag(0.1, 3),n.draws = EN ,burnin = 5000)

#플롯 그리는 부분
chain1=mcmc(res1$param[SN:EN,1], start=SN)
chain2=mcmc(res1$param[SN:EN,2], start=SN)
chain3=mcmc(res3$param[SN:EN], start=SN)


plot(SN:EN,chain1[SN:EN],type='l',main = 'res1 mcmc samples') + abline(h=0.5,col='blue')
plot(SN:40000,chain2[SN:EN],type='l',main = 'res2 mcmc samples') + abline(h=0.5,col='blue')
plot(SN:EN,chain3[SN:EN],type='l',main = 'res3 mcmc samples') + abline(h=-4,col='blue')



library(coda)
#imai 교수님 MNP 패키지 설명 문서 중, 수렴 진단을 위한 부분
res.coda <- mcmc.list(chain1=mcmc(res1$param[25001:50000], start=25001),
                      chain2=mcmc(res2$param[25001:50000], start=25001),
                      chain3=mcmc(res3$param[25001:50000], start=25001))

gelman.plot(res.coda, transform = TRUE)