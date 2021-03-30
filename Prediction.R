library(mgcv)
insurance<-read.csv("insurance3r2.csv",header=T)
attach(insurance)
insurance$coverage[factor(children) == 0]<-0
insurance$coverage[children > 0]<-1
attach(insurance)
insurance$smoker=as.factor(insurance$smoker)
insurance$coverage=as.factor(insurance$coverage)
insurance$insuranceclaim=as.factor(insurance$insuranceclaim)

model=gam(charges~s(age,bs='cr')+bmi+steps+smoker+s(bmi,bs='cr',by=smoker)
          +s(steps,bs='cr',by=smoker,k=5)+s(age,bs='cr',by=coverage)+s(bmi,bs='cr',by=coverage))

summary(model) #86.3%
gam.check(model)
plot(model,se=F,scale=0,pages=1)

####################### CONFIDENCE INTERVALS (predict.gam) ###################
#First example (not smoker)
#with children covered by insurance
newdata=data.frame(age=37,bmi=30.30,steps=3006,smoker=0,coverage=1)
set.seed(100)
pred1 <- predict.gam(model,newdata,se.fit=TRUE) #7178
alpha=0.05
lwr=pred1$fit-pred1$se.fit*qt(1-(alpha/2),nrow(insurance)) #6327
lvl=pred1$fit
upr=pred1$fit+pred1$se.fit*qt(1-(alpha/2),nrow(insurance)) #8028
#real charges: 7537 -> the insurance holder is paying fairly

#Second example (smoker)
newdata2=data.frame(age=54,bmi=30.685,steps=4004,smoker=1,coverage=0)
set.seed(100)
pred2 <- predict.gam(model,newdata2,se.fit=TRUE) #38505
alpha=0.05
lwr=pred2$fit-pred2$se.fit*qt(1-(alpha/2),nrow(insurance)) #37121
lvl=pred2$fit
upr=pred2$fit+pred2$se.fit*qt(1-(alpha/2),nrow(insurance)) #39890
#real charges: 40213 -> he should pay a little less

#Third example(normal weight)
newdata3=data.frame(age=32,bmi=23.00,steps=10001,smoker=0,coverage=0)
set.seed(100)
pred3 <- predict.gam(model,newdata3,se.fit=TRUE) #5668
alpha=0.05
lwr=pred3$fit-pred3$se.fit*qt(1-(alpha/2),nrow(insurance)) #4848
lvl=pred3$fit 
upr=pred3$fit+pred3$se.fit*qt(1-(alpha/2),nrow(insurance)) #6987
#real charges:4772 -> it is just outside the range: he should pay a little more

#Fourth example(high bmi)
newdata4=data.frame(age=51,bmi=41.30,steps=3003,smoker=0,coverage=0)
set.seed(100)
pred4 <- predict.gam(model,newdata4,se.fit=TRUE) #11006
alpha=0.05
lwr=pred4$fit-pred4$se.fit*qt(1-(alpha/2),nrow(insurance)) #10116
lvl=pred4$fit
upr=pred4$fit+pred4$se.fit*qt(1-(alpha/2),nrow(insurance)) #11897
#real charges: 15602 -> he pays too much

################ SOME FAMOUS EXAMPLES ####################
#Lizzo
newdata_lizzo=data.frame(age=32,bmi=38.80,steps=4000,coverage=0,smoker=0)
set.seed(100)
pred_lizzo <- predict.gam(model,newdata_lizzo,se.fit=TRUE)
alpha=0.05
lwr=pred_lizzo$fit-pred_lizzo$se.fit*qt(1-(alpha/2),nrow(insurance))
lvl=pred_lizzo$fit
upr=pred_lizzo$fit+pred_lizzo$se.fit*qt(1-(alpha/2),nrow(insurance))

cbind(lwr,lvl,upr) #4928 5721 6514
#real charges: 5400 -> she is paying fairly

#Beyonce
newdata_beyonce=data.frame(age=39,bmi=21.40,steps=6000,coverage=1,smoker=1)
set.seed(100)
pred_beyonce <- predict.gam(model,newdata_beyonce,se.fit=TRUE)
alpha=0.05
lwr=pred_beyonce$fit-pred_beyonce$se.fit*qt(1-(alpha/2),nrow(insurance))
lvl=pred_beyonce$fit
upr=pred_beyonce$fit+pred_beyonce$se.fit*qt(1-(alpha/2),nrow(insurance))

cbind(lwr,lvl,upr) #16630 18692 20753
#real charges: 21320 -> she is paying a little more

#George Clooney
newdata_clooney=data.frame(age=59,bmi=24.0,steps=6000,coverage=1,smoker=1)
set.seed(100)
pred_clooney <- predict.gam(model,newdata_clooney,se.fit=TRUE)
alpha=0.05
lwr=pred_clooney$fit-pred_clooney$se.fit*qt(1-(alpha/2),nrow(insurance))
lvl=pred_clooney$fit
upr=pred_clooney$fit+pred_clooney$se.fit*qt(1-(alpha/2),nrow(insurance))

cbind(lwr,lvl,upr) #26488 28170 29854
#real charges: 25980 -> he is paying a little less


