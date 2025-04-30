#Continued discussion of two-sample testing by applying the techniques derived in the previous
#to several examples. 
#Also investigating how a resampling procedure known as the bootstrap can be used in the 
#small sample setting. 


names(cars)
summary(cars)

library(knitr)
kable(car_speeds, caption="Data Table")

##data vectors
speed.up<-car_speeds$speed[car_speeds$direction=='Up']
speed.down<-car_speeds$speed[car_speeds$direction=='Down']

#boxplot
par(mar=c(1,1,1,1))
boxplot(speed.up, speed.down, col='green', names=c('Speed Up', 'Speed Down'))

names(car_speeds)
summary(car_speeds)

#Some guy recorder the speeds of cars driving past his house, where the speed limit read 20mph.
#He recorded every car for a two-month period and the speeds are recorded under two conditions
#corresponding to the direction, either 'Up' or 'Down'.

#Let's investigate how the speed of cars driving past his house differ across the two directions.

summary(speed.up)
summary(speed.down)

#standard deviation of the mean in the up direction
sqrt(var(speed.up))/sqrt(length(speed.up))

#standard deviation of the mean in the down direction
sqrt(var(speed.down))/sqrt(length(speed.down))

#The distribution of speed appears shifted towards smaller values in the 'Down' direction on
#the one hand. On the other hand, the two samples of data appear to exhibit a reasonable degree
#of overlap in data values. 

#Compute a 95% Confidence Interval for mean(up) - mean(down)

alpha<-0.05
lower<-mean(speed.up)-mean(speed.down)-qnorm(1-(alpha/2))*sqrt((var(speed.up)/length(speed.up))+(var(speed.down)/length(speed.down)))
upper<-mean(speed.up)-mean(speed.down)+qnorm(1-(alpha/2))*sqrt((var(speed.up)/length(speed.up))+(var(speed.down)/length(speed.down)))
c(lower,upper)

#We can further compute the p-value testing for H0: mean(up) = mean(down)
#versus H1: mean(up) /= mean(down)

T.obs<-(mean(speed.up)-mean(speed.down))/sqrt((var(speed.up)/length(speed.up))+(var(speed.down)/length(speed.down)))
T.obs

p.value<-2*(1-pnorm(T.obs))
p.value

#If the null hypothesis H0: mean(up) = mean(down) is true, then the probability of observing a
#result (test statistic) at least as extreme as that observed is extremely low. 

#Given that the probability is extremely low, we have very strong evidence against H0 and it
#appears as though the mean speeds associated with the two directions are different. 

#In particular the 95% Confidence Interval of (1.879, 3.190) indicates that the speed of cars
#in the up direction is higher than that in the down direction. 

#Adjusting this comparison for other factors such as car type and road condition, if such data
#are available, can be done using a regression model.



##NEXT EXAMPLE: STEREOGRAMS
#A stereogram is an image that looks like a random set of dots. If viewed in a certain way the
#viewer can see within this image a three-dimensional image by looking at the dots while 
#defocusing their eyes. Thus there is an embedded image inside the image. 

#An experiment was performed to determine whether knowledge of the embedded image affected the
#time required for subjects to fuse or recognize the embedded image. 

#One group of subjects (group NV) in this experiment randomized to receive no information or
#just verbal information about the shape of the embedded object. 

#A second group (group VV) randomized to receive both verbal information and visual information
#(specifically a drawing of the object).

#The experimenters measured how many seconds it took for the subject to report that he or she saw
#the 3D image. This is called the fusion time. 

#A relevant research would be to ask if subjects who receive both verbal and visual information
#have a smaller mean fusion time than those who receive only verbal or now information. 

stereograms<-read.table(file='C:/Users/Mik/Documents/STAT359/data/stereograms.txt', sep="", header=TRUE)
names(stereograms)
summary(stereograms)

#Let us examine the data in order to investigate how the fusion time differs between the two 
#experimental groups. Testing at a level alpha = 0.05.

time.NV<-stereograms$fusion_time[stereograms$group=='NV']
time.VV<-stereograms$fusion_time[stereograms$group=='VV']
summary(time.VV)
summary(time.NV)

#standard deviation of the mean in the up direction
sqrt(var(time.NV))/sqrt(length(time.NV))
#standard deviation of the mean in the down direction
sqrt(var(time.VV))/sqrt(length(time.VV))

#boxplot
boxplot(time.NV, time.VV, col='green', names=c('Time-NV', 'Time-VV'))
title('Stereogram Fusion Times')

#We test the hypothesis H0: mean(NV) = mean(VV) against a one-sided alternative 
#H1: mean(NV) > mean(VV)

T.obs<-(mean(time.NV)-mean(time.VV))/sqrt((var(time.NV)/length(time.NV))+(var(time.VV)/length(time.VV)))
T.obs

#The p-value is the probability of observing a test statistic at least as extreme as that
#observed, assuming that the null hypothesis is true. 
#We have P(T >= T.obs) = 1 - PHI(T.obs) where T~N(0,1)

1-pnorm(T.obs)

#Testing at level alpha=0.05, we reject the null hypothesis in favor of the alternative 
#suggesting that the mean fusion time is reduced when subjects receive both verbal and visual 
#information relative to the case when subjects receive only verbal or no information.

#Computing a 95% Confidence Interval for mean(NV) -  mean(VV) we obtain
alpha<-0.05
lower<-mean(time.NV)-mean(time.VV)-qnorm(1-(alpha/2))*sqrt((var(time.NV)/length(time.NV))+(var(time.VV)/length(time.VV)))
upper<-mean(time.NV)-mean(time.VV)+qnorm(1-(alpha/2))*sqrt((var(time.NV)/length(time.NV))+(var(time.VV)/length(time.VV)))
c(lower, upper)


##NEXT EXAMPLE: BUY FROM A FRIEND - DASL (The Bootstrap)

#A researcher at Cornell wanted to know how friendship would affect simple sales. 

#She randomly divided subjects into two groups and gave each group descriptions of items they
#might want to buy.

#One group was told to imagine buying from a friend whom they expect to see again. The other 
#group was told to imagine buying from a stranger. 

#The data are the prices offered by the experiment participants. 

buy.friend<-read.table(file='C:/Users/Mik/Documents/STAT359/data/buy_from_a_friend.txt', sep="", header=TRUE, na.strings="NA")
names(buy.friend)
summary(buy.friend)
library(knitr)
kable(buy.friend, caption='Data Table')

attach(buy.friend)
#remove missing values
Stranger<-Stranger[!is.na(Stranger)]

#We would like to investigate how the sales prices differ in the two groups so as to determine
#if friendship plays a role in price in this context

boxplot(Friend, Stranger, col='purple',names=c('Price-Friend','Price-Stranger'))
title('Sale Prices Offered')

#There appears to be a strong shift in the distribution of prices with Friend group
#having higher prices. 

#The sample median is $282.5 for the Friend group and $225 for the Stranger group.

#Suppose we wanted a 95% C.I. for the difference in population medians. 

#The interval Y1(bar) - Y2(bar) +/- Z(alpha/2)*sqrt((s1^2)/m + (s2^2)/n) applies for large
#sample and for the difference in population means. 

#For small sample situations and a more general statistic such expressions for confidence
#intervals are not easily derived. We can use the bootstrap to obtain a confidence interval. 

#The bootstrap is a re-sampling procedure that estimates the variance of a statistic under the
#sampling distribution with the variance of that statistic under the empirical distribution.

#The empirical distribution of sampled values x1, x2, ... , xn is the distribution that places
#probability mass 1/n at each data point. 

#We can easily sample from this distribution by sampling from the data with replacement. 

#By taking a large number of such samples, known as bootstrap sample, we can approximate
#the variance of the statistic under the empirical distribution arbitrarily well. 

#The error arising from approximating the sampling distribution with empirical distribution
#can not be reduced by taking a larger number of bootstrap samples. 

#Suppose ThetaHat(x1,...,xn) is an estimator of theta, where theta is a property of the 
#sampling distribution.

#The bth bootstrap sample, b=1,...,B, is obtained by sampling from the data with replacement,
#resulting in a replicate dataset x*(b,1),...,x*(b,n). These replicate datasets are draws 
#from the empirical distribution. 

#From the replicate datasets we compute values of the estimator ThetaHat, b=1,..,B.

#The 100(1-alpha)% bootstrap C.I. is obtained from the alpha/2 and 1-alpha/2 quantiles
#of the bootstrap sampled estimators (ThetaHat(b,alpha/2), ThetaHat(b,1-alpha/2))

# Bootstrap Confidence Interval for ??
# Draw X???1,.,X???n by sampling from the data with replacement.
# Compute ??^(X???1,.,X???n).
# Repeat steps 1 and 2, B times, to obtain ??^???1,.,??^???B.
# Bootstrap 100(1?????) confidence interval: (??^?????/2,??^???1?????/2) obtained from the ??/2 and 1?????/2 quantiles.

#We obtain 95% bootstrap C.I. for the difference of the two population medians as follows:


#observed estimator
theta.hat<-median(Friend)-median(Stranger)

#number of bootstrap replicates
B<-10000
theta.boot<-rep(0,B)
for (i in 1:B)
{
  #Re-sample the friend data
  Friend.boot<-sample(x=Friend, size=length(Friend), replace=TRUE)
  #Re-sample the stranger data
  Stranger.boot<-sample(x=Stranger, size=length(Stranger), replace=TRUE)
  #compute the bootstrap replicate of the estimator
  theta.boot[i]<-median(Friend.boot) - median(Stranger.boot)
}

alpha<-0.05
#lower bound
lower.bound<-quantile(theta.boot,probs=alpha/2)
upper.bound<-quantile(theta.boot,probs=1-alpha/2)
#estimate of the difference in medians
theta.hat

#95% C.I.
c(lower.bound, upper.bound)

#The bootstrap interval does not contain the value 0. Both the estimate and interval indicate
#higher median price associated with the friend group.

#The bootstrap interval constructed above is an approximate 100(1-alpha) C.I.

#Let us conduct a simulation study to investigate the coverage probability of the bootstrap
#95% C.I. for a single population median for different sample sizes n=5,15,30.

start.time<-Sys.time()
#sample size over which to run the simulation study
sample.size<-seq(5,40,5)
cov.prob<-rep(0,length(sample.size))
#for each sample size we will generate n.sim replicate datasets to estimate coverage probability
n.sim<-100 #n.sim<-5000 uses 5000 simulations for greater stability in actual results
#for each replicate data set we will based the bootstrap interval on B bootstrap samples

B<-1000
n<-5
coverage.indicator<-rep(0,n.sim)
n.iter<-1

for(n in sample.size)
{
  for (j in 1:n.sim)
  {
    #generate data from a standard exponential distribution where the median is log(2)
    x<-rexp(n=n,rate=1) #the median of this distribution is log(2)
    median.boot<-rep(0,B)
    
    for(b in 1:B)
    {
      x.boot<-sample(x=x, size=length(x), replace=TRUE)
      median.boot[b]<-median(x.boot)
    }
    
  lower.median<-quantile(median.boot, probs=0.025)
  upper.median<-quantile(median.boot, probs=0.975)
  coverage.indicator[j]<-as.numeric((lower.median<=log(2))&&(log(2)<=upper.median))
  }
  
cat('Completed iteration for n =', n, '\n')
cov.prob[n.iter]<-mean(coverage.indicator)
n.iter<-n.iter+1
}

end.time<-Sys.time()
time.taken1<-end.time-start.time

##prob<-read.table(file='File that is generated by above. Dont know how to go about that')

#The coverage probabilities are all reasonably close to the nominal coverage of 0.95 but
#also suggest a slight under coverage.

#The under coverage arises from the approximation of the sampling distribution with the
#empirical distribution. 

#The code for the simulation study runs slow because it has three nested loops. We can reduce
#the computation time by vectorizing the inner loop. 



start.time<-Sys.time()
#load required library
library(robustbase)
#sample size over which to run the simulation study

sample.size<-seq(5,40,5)
cov.prob<-rep(0,length(sample.size))
#for each sample size we will generate n.sim replicate datasets to estimate coverage probability
n.sim<-100 #n.sim<-5000 uses 5000 simulations for greater stability in actual results
#for each replicate data set we will based the bootstrap interval on B bootstrap samples

B<-1000
coverage.indicator<-rep(0,n.sim)
n.iter<-1

for(n in sample.size)
{
  for (j in 1:n.sim)
  {
    #generate data from a standard exponential distribution where the median is log(2)
    x<-rexp(n=n,rate=1) #the median of this distribution is log(2)
    
    #generate all of the bootstrap samples in one shot
    #each column contains one replicated dataset
    x.boot<-matrix(data=sample(x=x, size=B*length(x), replace=TRUE), nrow=length(x), ncol=B)
    boot.interval<-quantile(colMedians(x.boot),probs=c(0.025,0.975))

    lower.median<-boot.interval[1]
    upper.median<-boot.interval[2]
    coverage.indicator[j]<-as.numeric((lower.median<=log(2))&&(log(2)<=upper.median))
    
  }

  cat('Completed iteration for n =', n, '\n')
  cov.prob[n.iter]<-mean(coverage.indicator)
  n.iter<-n.iter+1
}

end.time<-Sys.time()
time.taken2<-end.time-start.time


as.numeric(time.taken1)/as.numeric(time.taken2)
#Vectorizing the innermost loop leads to roughly a 20-fold improvement in the computation time!


#EXAMPLE: Distribution of the maximum
#Suppose X1,...,Xn ~(iid) Uniform(0,theta)
#Let the estimator of theta be thetaHat=Max{X1,...,Xn} and suppose we are interested in the
#distribution of thetaHat. 

#We can derive it exactly in this case. Consider the CDF: 
#F(t)=Pr(??^???t)=Pr(Max{X1,.,Xn}???t)=Pr(X1???t,X2???t,.,Xn???t)=Pr(X???t)n where X???Uniform(0,??).

#Noting that Pr(X???t)=t?? we have that F(t)=(t??)n and from this we can obtain the density 
#function of ??^ as f(t)=F'(t)=?????nntn???1,t???[0,??].

#Suppose ??=1 and n=50 then the density function for ??^=Max{X1,.,Xn} will be 
#f(t)=50t49 for t???[0,1].

t<-seq(0,1,0.001)
f<-50*(t^49)
plot(t,f,xlab='t',ylab='f(t)',main='Density of Max(X1,...,X50)',type='l')

#Next we approximate the distribution of ??^=Max{X1,.,Xn} from a single dataset
#using the bootstrap. 

#Simulate the dataset
x<-runif(n=50,min=0,max=1)
# observed value of the estimate
theta.hat<-max(x)
# bootstrap samples
B<-5000
x.boot<-matrix(sample(x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
theta.boot<-apply(x.boot,2,max)
hist(theta.boot,freq=FALSE,xlim=c(0,1),main='Bootstrap Approximation - Density of Max(X1,...,X50)')
