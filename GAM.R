library(mgcv)

insurance<-read.csv("insurance3r2.csv",header=T)
attach(insurance)

insurance$coverage[factor(children) == 0]<-0
insurance$coverage[children > 0]<-1
attach(insurance)

insurance$smoker = as.factor(insurance$smoker)
insurance$insuranceclaim = as.factor(insurance$insuranceclaim)
insurance$coverage = as.factor(insurance$coverage)
insurance$steps = as.factor(insurance$steps)

#######################################################
#### MODEL SELECTION WITH GAM - BACKWARD PROCEDURE ####
#######################################################

#Initial model
model_gam_complete_initial=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
                               + smoker + insuranceclaim  + coverage 
                               + s(age,bs='cr',by=smoker)+s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
                               + s(age,bs='cr',by=insuranceclaim) + s(bmi,bs='cr',by=insuranceclaim)+ s(steps,bs='cr',by=insuranceclaim,k=5) 
                               + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage) + s(steps,bs='cr',by=coverage,k=5)
                               + s(age,bmi,bs='tp') + s(steps,bmi,bs='tp',k=5) + s(age,steps,bs='tp', k=5))

summary(model_gam_complete_initial) #R2 = 86.3%

# Check if the residuals are normal
hist(model_gam_complete_initial$residuals)
qqnorm(model_gam_complete_initial$residuals)
shapiro.test(model_gam_complete_initial$residuals) 
# Not normal! We choose the best model looking for the R2 parameter and considering the p-value
# of the variables only to choose the removal order


#Remove the interaction between quantitative variables
model_gam1=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
                + smoker + insuranceclaim  + coverage 
                + s(age,bs='cr',by=smoker)+s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
                + s(age,bs='cr',by=insuranceclaim)+s(bmi,bs='cr',by=insuranceclaim) + s(steps,bs='cr',by=insuranceclaim,k=5)
                + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage) + s(steps,bs='cr',by=coverage,k=5))

summary(model_gam1) #R2 = 86.4%


#Remove dummy variables insuranceclaim and coverage
model_gam2=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
               + smoker 
               + s(age,bs='cr',by=smoker)+s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
               + s(age,bs='cr',by=insuranceclaim)+s(bmi,bs='cr',by=insuranceclaim) + s(steps,bs='cr',by=insuranceclaim,k=5)
               + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage) + s(steps,bs='cr',by=coverage,k=5))

summary(model_gam2) #R2 = 86.4%


#Remove the interaction between quntitative variables and insuranceclaim
model_gam3=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
               + smoker 
               + s(age,bs='cr',by=smoker)+s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
               + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage) + s(steps,bs='cr',by=coverage,k=5))

summary(model_gam3) #R2 = 86.4%


#Remove interaction between steps and coverage
model_gam4=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
               + smoker 
               + s(age,bs='cr',by=smoker)+s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
               + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage))

summary(model_gam4) # R2 = 86.3%


#Remove age:smoker
model_gam5=gam(charges ~ s(age, bs='cr') + s(bmi,bs='cr') + s(steps,bs='cr',k=5)
               + smoker 
               + s(bmi,bs='cr',by=smoker) + s(steps,bs='cr',by=smoker,k=5)
               + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage))

summary(model_gam5)  #R2 = 86.2%


#Notice that the variables bmi and steps are linear. So we shift them to the linear part of the model


#### FINAL MODEL ####
# The model we choose in order to make prediction is:

model_gam_complete=gam(charges ~ s(age, bs='cr') + bmi + steps + smoker + s(bmi,bs='cr',by=smoker) 
                       + s(steps,bs='cr',by=smoker,k=5) + s(age,bs='cr',by=coverage) 
                       + s(bmi,bs='cr',by=coverage))

summary(model_gam_complete) #R2 = 86.3%


###########################################
######### PLOT OF THE REGRESSORS #########
###########################################

#The linear terms are added as smooth to generate their plots.

model_gam_complete=gam(charges ~ s(age,bs='cr') + s(bmi,bs='cr') +s(steps,bs='cr',k=5) +smoker + s(bmi,bs='cr',by=smoker) 
                       + s(age,bs='cr',by=coverage) + s(bmi,bs='cr',by=coverage)+ s(steps,bs='cr',by=smoker,k=5))
summary(model_gam_complete)

# Plot of the regressors in order to understand the behaviour of each variable

plot.gam(model_gam_complete,se=F,scale = 0 ,pages = 1)

y=plot.gam(model_gam_complete,se=F,scale = 0 ,pages = 1)

#6610 -> intercept
#26.09 -> intercept of bmi
#18960 -> intercept of smoker
#267 -> intercept of age
#667 -> intercept of coverage
# ~0 -> intercept of steps

#Plot of BMI
ylimit=c(6000,50000)
plot(y[[2]]$x, y[[2]]$fit+y[[4]]$fit+6610+26.09+18960,ylim=ylimit,type = "l", xlab = 'bmi', ylab = 'charges') 
lines(y[[2]]$fit+6610+26.09, col='red')
lines(y[[2]]$fit+y[[4]]$fit+y[[6]]$fit+6610+26.09+18960+667, col='green')
lines(y[[2]]$fit+y[[6]]$fit+6610+26.09+667, col='blue')
legend(15,51000,c("bmi","bmi+smoker", "bmi+coverage", "bmi+smoker+coverage"),text.col=c('red','black','blue','green'), box.col='white')
#Plot of AGE
plot(y[[1]]$x, y[[1]]$fit+y[[5]]$fit+6610+267+667,type = "l", xlab = 'age', ylab = 'charges' ) 
lines(y[[1]]$fit+6610+267, col='red') 
legend(18,16000,c("age","age+coverage"),text.col=c('red','black'), box.col='white')

#Plot of STEPS
yl <- c(25000, 32500)

plot(y[[3]]$x, y[[3]]$fit+y[[7]]$fit+6610+18960, ylim=(yl), type = "l", xlab = 'steps', ylab = 'charges' )
lines(y[[3]]$x,y[[3]]$fit+6610+18960 ,col='red')
legend(7000,32000,c("steps","steps+smoker"),text.col=c('red','black'), box.col='white')


