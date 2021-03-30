insurance <-read.csv("insurance3r2.csv",header=T)
attach(insurance)

library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(GGally)
library(MASS)
library(rgl)
library(DepthProc)

################################ Data visualization ####################################################

pairs(~ age + bmi + charges, data = insurance, pch = 16) #distribution of our continues variable

ggpairs(data = insurance, title ="Relationships between predictors & response",mapping = ggplot2::aes(colour=factor(smoker)), lower = list(continuous=wrap("points", size=1)))
#red for not smoker, blue for smoker
#Remarks:
#bmi is quite symmetric -> median with Tukey depth and Mahalanobis depth are similar
#there aren't differences between men and women in none of the considered variables
#we can see an evident difference between smoker and non-smoker when the response is charges
pplots<-ggpairs(data = insurance, title ="Relationships between predictors & response",mapping = ggplot2::aes(colour=factor(smoker)), lower = list(continuous=wrap("points", size=2)))
pplots[8,1]
pplots[8,3]

ggpairs(data = insurance, title ="Relationships between predictors & response",mapping = ggplot2::aes(colour=factor(insuranceclaim)), lower = list(continuous=wrap("points", size=1)))
#red for insurancecliam=0, blue for insuranceclaim=1
pplots<-ggpairs(data = insurance, title ="Relationships between predictors & response",mapping = ggplot2::aes(colour=factor(insuranceclaim)), lower = list(continuous=wrap("points", size=2)))
pplots[8,1]
pplots[8,3]
#also insuranceclaim seems to be significant for our response charges


#bagplot of our continuous distributed data
mixcont=cbind(bmi, age, charges)
aplpack::bagplot.pairs(mixcont)
#there are some outliers with bmi

####### CHARGES VS BMI 
plot(bmi,charges, xlab="bmi",ylab="charges",main="charges vs bmi")
#Tukey depth
chvsbmi=cbind(bmi,charges)
depthMedian(chvsbmi,depth_params = list(method='Tukey')) #bmi:30.80 charges: 9414.92
depthContour(chvsbmi,depth_params = list(method='Tukey'),
             graph_params = list(main="Charges vs bmi: Tukey",xlab ="bmi", ylab="charges"),levels =20)
depthPersp(chvsbmi,depth_params = list(method='Tukey'))
#mahalanobis depth
depthMedian(chvsbmi,depth_params = list(method='Mahalanobis')) #bmi:30.80 charges:13390.56
depthContour(chvsbmi,depth_params = list(method='Mahalanobis'))
depthPersp(chvsbmi,depth_params = list(method='Mahalanobis'))

#bagplot and OUTLIER DETECTION
bgplot=aplpack::bagplot(chvsbmi,xlab="bmi",ylab="charges",main="Bagplot charges vs bmi")
outl<-bgplot$pxy.outlier #we have 2 outliers in the lower right-hand corner 
#these outliers are, respectively, instance 848 and 1318 in the dataset:
out1<-insurance[insurance$bmi == outl[1][1], ]
out2<-insurance[insurance$bmi == outl[2][1], ]
#These outliers have low charges and high bmi
#As expected, both are not smokers, indeed smoking raises the costs of insurance.
#They come from the same region (southeast), they are both males
#out2 has higher bmi than out1 but lower charge: this could be because out1's age is 23>18= out2's age
#and because out1 has one children.

####### CHARGES VS AGE 
plot(age, charges,xlab="age",ylab="charges",main="charges vs age")
#Tukey depth
chvsage=cbind(age,charges)
depthMedian(chvsage,depth_params = list(method='Tukey')) #age: 41 charges: 7954.517   
depthContour(chvsage,depth_params = list(method='Tukey'),
             graph_params = list(main="Charges vs age: Tukey",xlab ="age", ylab="charges"),levels =20)
depthPersp(chvsage,depth_params = list(method='Tukey'))

#bagplot and OUTLIER DETECTION
bgplot=aplpack::bagplot(chvsage,xlab="age",ylab="charges",main="Bagplot charges vs age")
outl<-bgplot$pxy.outlier #there aren't outliers

############################# test ANOVA between charges and children #######################################
#ANOVA between charges and children
# H0: tau1 = tau2 = tau3 = tau4 = tau5 = tau6 = 0
# H1: (H0)^c

g <- 6
n <- dim(insurance)[1]
boxplot(charges~children,col=rainbow(g),main='Original Data')

# Parametric test:
fit <- aov(charges ~ children)
summary(fit)

shapiro.test(fit$residuals) #not gaussian 

# Permutation test:
# Test statistic: F stat
#I am extracting the F value from the summary
T0 <- summary(fit)[[1]][1,4]

permutazione <- sample(1:n) 
charges_perm <- charges[permutazione]
fit_perm <- aov(charges_perm ~ children)
summary(fit_perm)

boxplot(charges_perm~children,col=rainbow(g),main='Permuted Data')

# CMC to estimate the p-value
set.seed(1998)
B <- 1000 
T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  charges_perm <- charges[permutation]
  fit_perm <- aov(charges_perm ~ children)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #1% we reject the null hp.

#### ANOVA between 1-2-3-4-5 children
# H0: tau1 = tau2 = tau3 = tau4 = tau5 = 0
# H1: (H0)^c
Xfigli <- insurance %>%
  filter(insurance$children!=0)
attach(Xfigli)
g <- 5
n <- dim(Xfigli)[1]

boxplot(charges~children,col=rainbow(g),main='Original Data')

# Parametric test:
fit <- aov(charges ~ children)
summary(fit)

shapiro.test(fit$residuals) #not gaussian

# Permutation test:
# Test statistic: F stat
#I am extracting the F value from the summary
T0 <- summary(fit)[[1]][1,4]

permutazione <- sample(1:n) 
charges_perm <- charges[permutazione]
fit_perm <- aov(charges_perm ~ children)
summary(fit_perm)

boxplot(charges_perm~children,col=rainbow(g),main='Permuted Data')

# CMC to estimate the p-value
set.seed(1998)
B <- 1000 # Number of permutations
T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  charges_perm <- charges[permutation]
  fit_perm <- aov(charges_perm ~ children)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #33% we can accept H0

detach(Xfigli)

############ test 1 ############
#H0: man and women pay on average the same

#Mann-Whitney U test
Xmale <- insurance %>%
  filter(insurance$sex==1)
Xfemale <- insurance %>%
  filter(insurance$sex==0)
Charge <- insurance$charges
Charges_male <- Xmale$charges
Charges_female <- Xfemale$charges

boxplot(charges~sex,col=rainbow(2), names= factor(c('female', 'male'))) #Many outliers

n1 <- length(Charges_male)
n2 <- length(Charges_female)
n  <- length(Charge)

ranks.Charge <- rank(Charge)

R1 <- sum(ranks.Charge[insurance$sex==1])
U1 <- R1 - n1*(n1+1)/2  # Nr of wins of the 1st sample

R2 <- sum(ranks.Charge[insurance$sex==0])
U2 <- R2 - n2*(n2+1)/2  # Nr of wins of the 2nd sample

n1*n2 # Nr of contests

# MC computation of the p-value
# Generation of U1 and U2 under the null hypothesis

set.seed(1998)
B <- 1000
U1.sim <- numeric(B)
U2.sim <- numeric(B)
for (k in 1:B)
{
  ranks.temp <- sample(1:n)
  R1.temp <- sum(ranks.temp[1:n1])
  R2.temp <- sum(ranks.temp[(n1+1):(n1+n2)])
  U1.temp <- R1.temp - n1*(n1+1)/2
  U2.temp <- R2.temp - n2*(n2+1)/2
  U1.sim[k] <- U1.temp
  U2.sim[k] <- U2.temp
}

hist(U1.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

hist(U2.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

U.star <- max(U1, U2)

p.value <- 2 * sum(U1.sim >= U.star)/B
p.value #73%, we can accept H0: male and female pay on average the same

#Permutation test
x_pooled <- c(Charges_male,Charges_female)

n=length(Charge)
n1=length(Charges_male)
n2=length(Charges_female)

permutation <- sample(1:n) 
x_perm <- x_pooled[permutation] 
Charges_male_perm <- x_perm[1:n1]
Charges_female_perm <- x_perm[(n1+1):n]

# Test statistic: absolute difference between the two means
T0 <- abs(median(Charges_male) - median(Charges_female)) #we use a more robust estimator
T0

set.seed(1998)
B <- 1000 
T_stat <- numeric(B)

for(perm in 1:B){
  # permutation:
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # test statistic:
  T_stat[perm] <- abs(median(x1_perm) - median(x2_perm))
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #95% 

#we accept the null hp, charges of male and female have the same distribution

######################################## test 2 ######################################################################
#HO:smoker and not smoker pay on average the same

Xsmoker <- insurance %>%
  filter(insurance$smoker==1)
Xnotsmoker <- insurance %>%
  filter(insurance$smoker==0)
Charge <- insurance$charges
Charges_smoker <- Xsmoker$charges
Charges_notsmoker <- Xnotsmoker$charges

boxplot(charges~ smoker, col=rainbow(2)) #some outliers for no smoker

#Mann-Whitney U test
n1 <- length(Charges_smoker)
n2 <- length(Charges_notsmoker)
n  <- length(Charge)

ranks.Charge <- rank(Charge)

R1 <- sum(ranks.Charge[insurance$smoker==1])
U1 <- R1 - n1*(n1+1)/2  # Nr of wins of the 1st sample

R2 <- sum(ranks.Charge[insurance$smoker==0])
U2 <- R2 - n2*(n2+1)/2  # Nr of wins of the 2nd sample

n1*n2 # Nr of contests

# MC computation of the p-value
# Generation of U1 and U2 under the null hypothesis

set.seed(1998)
B <- 1000
U1.sim <- numeric(B)
U2.sim <- numeric(B)
for (k in 1:B)
{
  ranks.temp <- sample(1:n)
  R1.temp <- sum(ranks.temp[1:n1])
  R2.temp <- sum(ranks.temp[(n1+1):(n1+n2)])
  U1.temp <- R1.temp - n1*(n1+1)/2
  U2.temp <- R2.temp - n2*(n2+1)/2
  U1.sim[k] <- U1.temp
  U2.sim[k] <- U2.temp
}

hist(U1.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

hist(U2.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

U.star <- max(U1, U2)

p.value <- 2 * sum(U1.sim >= U.star)/B
p.value      #0, we reject H0

#Permutation test
x_pooled <- c(Charges_smoker,Charges_notsmoker)

n=length(Charge)
n1=length(Charges_smoker)
n2=length(Charges_notsmoker)

permutation <- sample(1:n) 

x_perm <- x_pooled[permutation] 
Charges_smoker_perm <- x_perm[1:n1]
Charges_notsmoker_perm <- x_perm[(n1+1):n]

# Test statistic: absolute difference between the two means
T0 <- abs(mean(Charges_smoker) - mean(Charges_notsmoker))
T0

set.seed(1998)
B <- 1000 # Number of permutations
T_stat <- numeric(B)

for(perm in 1:B){
  # permutation:
  permutation <- sample(1:n)
  x_perm <- x_pooled[permutation]
  x1_perm <- x_perm[1:n1]
  x2_perm <- x_perm[(n1+1):n]
  # test statistic:
  T_stat[perm] <- abs(mean(x1_perm) - mean(x2_perm))
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30) 
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat))#cumulative distribution function of the permuted distribution
abline(v=T0,col=3,lwd=2)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #0, we reject H0

#smoker pay on average more than not smoker


###################### test 3 ###################################################################
#3°test:
#H0: people with insuranceclaim=1 and people with insuranceclaim=0 pay on average the same

Xclaim <- insurance %>%
  filter(insurance$insuranceclaim==1)
Xnoclaim <- insurance %>%
  filter(insurance$insuranceclaim==0)
Charges_claim <- Xclaim$charges
Charges_noclaim <- Xnoclaim$charges
Charge<-insurance$charges

boxplot(Charge ~ insuranceclaim, col=rainbow(2)) #there are some outliers

n1 <- length(Charges_claim)
n2 <- length(Charges_noclaim)
n  <- length(Charge)

ranks.Charge <- rank(Charge)

R1 <- sum(ranks.Charge[insurance$insuranceclaim==1])
U1 <- R1 - n1*(n1+1)/2  # Nr of wins of the 1st sample

R2 <- sum(ranks.Charge[insurance$insuranceclaim==0])
U2 <- R2 - n2*(n2+1)/2  # Nr of wins of the 2nd sample

n1*n2 # Nr of contests

# MC computation of the p-value
# Generation of U1 and U2 under the null hypothesis

set.seed(1998)
B <- 1000
U1.sim <- numeric(B)
U2.sim <- numeric(B)
for (k in 1:B)
{
  ranks.temp <- sample(1:n)
  R1.temp <- sum(ranks.temp[1:n1])
  R2.temp <- sum(ranks.temp[(n1+1):(n1+n2)])
  U1.temp <- R1.temp - n1*(n1+1)/2
  U2.temp <- R2.temp - n2*(n2+1)/2
  U1.sim[k] <- U1.temp
  U2.sim[k] <- U2.temp
}

hist(U1.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

hist(U2.sim)
abline(v = c(U1, U2), col='red')
abline(v = n1*n2/2, lwd=3)

U.star <- max(U1, U2)

p.value <- 2 * sum(U1.sim >= U.star)/B
p.value #0, I reject H0



#################### test ANOVA between charges and regions ###################################################
# H0: tau1 = tau2 = tau3 = tau4 = 0
# H1: (H0)^c

attach(insurance)
g <- 4
n <- dim(insurance)[1]

boxplot(charges~region,col=rainbow(g),main='Original Data')

# Parametric test:
fit <- aov(charges ~ region)
summary(fit)
shapiro.test(fit$residuals) #not gaussian

# Permutation test:
# Test statistic: F stat
#I am extracting the F value from the summary
T0 <- summary(fit)[[1]][1,4]

# what happens if we permute the data?
permutazione <- sample(1:n) 
charges_perm <- charges[permutazione]
fit_perm <- aov(charges_perm ~ region)
summary(fit_perm)

boxplot(charges_perm~region,col=rainbow(g),main='Permuted Data')

# CMC to estimate the p-value
set.seed(1998)
B <- 1000 # Number of permutations
T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  charges_perm <- charges[permutation]
  fit_perm <- aov(charges_perm ~ region)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #82%, we accept H0, there aren't differences in the distribution of charges between regions

############### test ANOVA between charges and steps ############################################

plot(charges~steps) #categorical
table(steps)
#I join the value which are closer
x  = c( 3000, 4000, 5000, 8000,10000 , 10011 ) #grid between min and max
step_new = cut( steps, breaks = x, include.lowest = TRUE, right = FALSE ) 

boxplot(charges~step_new)#many outliers

# H0: tau1 = tau2 = tau3 = tau4 = tau5= 0
# H1: (H0)^c

tapply( charges, step_new, mean )
g <- 5
n <- dim(insurance)[1]

anova=aov(charges~step_new)
summary(anova) #evidence for different means

shapiro.test(anova$residuals) #normality assumption doesn't stand

#permutation
T0 <- summary(anova)[[1]][1,4]
T0

# what happens if we permute the data?
permutazione <- sample(1:n) 
charges_perm <- charges[permutazione]
fit_perm <- aov(charges_perm ~ step_new)
summary(fit_perm)

# CMC to estimate the p-value
set.seed(1998)
B <- 1000 # Number of permutations
T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  charges_perm <- charges[permutation]
  fit_perm <- aov(charges_perm ~ step_new)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}


hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

# p-value
p_val <- sum(T_stat>=T0)/B
p_val #I reject the null hp, the number of steps affect charges
