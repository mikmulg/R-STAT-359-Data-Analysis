#SKEWNESS AND KURTOSIS

#The variability associated with the random variable X is defined by 
# VAR[X] = E[(X-E[X])^2]

#We estimate this quantity from a sample of observations x1,...,xn with
#the sample variance s^2=1/(n???1)???(n,i=1)(xi???x¯)^2.

#The mean represents a measure of location and the variance is a measure
#of spread. In addition to these properties, which are associated with
#the first two moments of the population distribution, we may consider
#properties associated with higher order moments. 

#The skewness associated with the random variable X is defined by 
#Skew[X]=E[((X-mean)/sd)^3]

#Skew[X]>0 indicates that the right tail of the distribution of X is no
#longer than the left tail and this will be reflected in the shape
#of the density function. 

#load the skew-normal package
library('sn')

#plot and example of a positively skewed density function
x<-seq(-4,4,0.1)
y<-dsn(x,alpha=5)
plot(x,y,type='l',col='blue',lwd=3)
title('Density of a right-skewed distribution')


#f(x)=2??(x)??(??x),??,x???R,
#??(x)=(1/???2??)exp{???x^2/2}
#??(x)=???(x,??????)??(u)du

#Skew[X]<0 indicates that the left tail of the distribution of X is
#no longer than the right tail of the distribution and this will
#be reflected in the shape of the density function. 

x<-seq(-4,4,0.1)
y<-dsn(x,alpha=-5)
plot(x,y,type='l',col='blue',lwd=3)
title('Density of a left-skewed distribution')

#Skew[X]=0 corresponds to symmetric distributions.

x<-seq(-4,4,0.1)
y<-dsn(x,alpha=0)
plot(x,y,type='l',col='blue',lwd=3)
title('Density of a N(0,1) distribution')

#The population skewness can be estimated from a sample of
#observations y1,...,yn using the sample skewness
#Skew^=???(n,i=1)(yi???y¯)^3/ns^3

#As with the previous examples we can use this estimator along 
#with the bootstrap to obtain a confidence interval for the 
#population skewness in order to investigate the symmetry of the 
#population distribution that underlies a given dataset.

stereograms<-read.table(file ='C:/Users/Mik/Documents/STAT359/data/stereograms.txt', sep="",header=TRUE)

time.NV<-stereograms$fusion_time[stereograms$group=='NV']
time.VV<-stereograms$fusion_time[stereograms$group=='VV']
boxplot(time.NV,time.VV,col='green',names=c('Time - NV','Time - VV'))
title('Stereogram Fusion Times')


qqnorm(time.NV,main='QQ-Plot: No/Verbal Information')

qqnorm(time.VV,main='QQ-Plot: Verbal/Visual Information')

#Examination of the data suggests positive skewness in the case of both samples.

# function to compute the sample skew
skew<-function(x){
  m3<-sum((x-mean(x))^3)/length(x)
  s3<-sqrt(var(x))^3
  m3/s3  
}

skew.hat.NV<-skew(time.NV)
skew.hat.NV

skew.hat.VV<-skew(time.VV)
skew.hat.VV

#We construct bootstrap 95% C.I. for the population skewness corresponding 
#to each sample. 

#In the process we use the apply() function in R

x<-time.NV ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
skew.boot.sampled<-apply(x.boot,2,skew)
boot.interval<-quantile(skew.boot.sampled,probs=c(0.025,0.975))
skew.hat.NV

boot.interval

x<-time.VV ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
skew.boot.sampled<-apply(x.boot,2,skew)
boot.interval<-quantile(skew.boot.sampled,probs=c(0.025,0.975))
skew.hat.VV

boot.interval

#In both cases the 95% C.I. excludes zero with positive lower bound
#suggesting positive population skewness in both cases. 

#We not also that the C.I.'s for both groups overlap, so that there is not
#sufficient evidence of a difference in the population skewness associated
#with the NV and VV groups. 

#EXAMPLE: SLALOM TIMES 2014

slalom2014<-read.table(file ='C:/Users/Mik/Documents/STAT359/data/slalom2014.txt', sep="",header=TRUE)

#The men's giant slalom skiing event consists of two runs whose times are 
#added together for a final score. 

#The data give the giant slalom times in the 2014 Winter Olympics at Sochi. 

names(slalom2014)
attach(slalom2014)

#Examine the giant slalom times of the listed participants. 

summary(Time_sec)

sqrt(var(Time_sec)) #reasonably strong degree of variability
Time.skew.est<-skew(Time_sec)
Time.skew.est

boxplot(Time_sec, col='blue')
title('Giant Slalom Times')

hist(Time_sec, main='Giant Slalom Times')

qqnorm(Time_sec, main='QQ Plot Normal - Normal')

#The data show positive skew with a long right tail and the distribution
#is clearly asymmetric. 

#This sort of distribution is somewhat typical of event time data generally.

#We obtain a 95% C.I. for the skew using the bootstrap. 

x<-Time_sec ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
skew.boot.sampled<-apply(x.boot,2,skew)
boot.interval<-quantile(skew.boot.sampled,probs=c(0.025,0.975))
hist(skew.boot.sampled, main='Empirical Distribution for Skew.hat',xlab='Sampled Values')
abline(v=Time.skew.est, col='red') ## arguments can be a and b, h, or v

Time.skew.est

boot.interval




#KURTOSIS

#While the skewness is a measure of the asymmetry of a distribution 
#associated with the third moment, the kurtosis is a measure associated
#with the fourth moment of a distribution and measures the peakedness of 
#a distribution as well as the heaviness of its tails. 

#Kurt[X]=E[((X-mean)/sd)^4]-3

#The kurtosis is defined so that is takes the value 0 for a normal distribution.

#This is sometimes called excess kurtosis. Thus when considering kurtosis, a 
#normal distribution is taken as a reference. 

#Distributions with positive kurtosis have heavier than normal tails. An
#example would be a t-distribution. 

x<-seq(-5,5,.01)
y.norm<-dnorm(x,mean=0,sd=1)
y.t1<-dt(x,df=1)
y.t2<-dt(x,df=2)
y.t3<-dt(x,df=3)
y.t4<-dt(x,df=4)
y.t10<-dt(x,df=10)

# set up the plot area
plot(c(min(x),max(x)),c(min(y.norm,y.t1,y.t2,y.t3,y.t4,y.t10),max(y.norm,y.t1,y.t2,y.t3,y.t4,y.t10)),type="n",
     xlab="x",ylab="Density function: f(x)")
title("Comparing the Normal and t-distribution")
lines(x,y.norm)
lines(x,y.t10,lty=2,col="red")
lines(x,y.t4,lty=3,col="red")
lines(x,y.t3,lty=4,col="red")
lines(x,y.t2,lty=5,col="red")
lines(x,y.t1,lty=6,col="red")

#Distributions with negative kurtosis have thinner than normal tails and 
#will have a flatter shape in the middle. An example would be a uniform
#distribution (tails drop to zero beyond a finite interval).

#The population kurtosis can be estimated from a sample of observations 
#y1,...,yn using the sample kurtosis. 

#Kurt^=???(n,i=1)(((yi???y¯)^4)/(ns)^4)???3

kurtosis<-function(x) {
  m4<-sum((x-mean(x))^4)/length(x)
  s4<-var(x)^2
  m4/s4 - 3  
}

x.norm<-rnorm(1000)
x.norm.kurt<-kurtosis(x.norm) ## sample estimate of the kurtosis
x.norm.kurt

x<-x.norm ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
kurt.boot.sampled<-apply(x.boot,2,kurtosis)
boot.interval<-quantile(kurt.boot.sampled,probs=c(0.025,0.975))
hist(kurt.boot.sampled, main='Empirical Distribution for kurt.hat',xlab='Sampled Values')
abline(v=x.norm.kurt, col='red') ## arguments can be a and b, h, or v

x.norm.kurt

boot.interval

x.t<-rt(1000,df=4)
x.t.kurt<-kurtosis(x.t) ## sample estimate of the kurtosis
x.t.kurt

x<-x.t ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
kurt.boot.sampled<-apply(x.boot,2,kurtosis)
boot.interval<-quantile(kurt.boot.sampled,probs=c(0.025,0.975))
hist(kurt.boot.sampled, main='Empirical Distribution for kurt.hat',xlab='Sampled Values')
abline(v=x.t.kurt, col='red') ## arguments can be a and b, h, or v

x.t.kurt

boot.interval

#Example: examine the kurtosis of the stereogram fusion times in the No/Verbal
#and Verbal/Visual groups

kurt.time.NV<-kurtosis(time.NV)
kurt.time.VV<-kurtosis(time.VV)
kurt.time.NV

kurt.time.VV

x<-time.NV ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
kurt.boot.sampled<-apply(x.boot,2,kurtosis)
boot.interval<-quantile(kurt.boot.sampled,probs=c(0.025,0.975))
hist(kurt.boot.sampled, main='NV Group: Empirical Distribution for kurt.hat',xlab='Sampled Values')
abline(v=kurt.time.NV, col='red') ## arguments can be a and b, h, or v

kurt.time.NV

boot.interval


x<-time.VV ## data for bootstrapping
B<-15000
x.boot<-matrix(data=sample(x=x,size=B*length(x),replace=TRUE),nrow=length(x),ncol=B)
kurt.boot.sampled<-apply(x.boot,2,kurtosis)
boot.interval<-quantile(kurt.boot.sampled,probs=c(0.025,0.975))
hist(kurt.boot.sampled, main='VV Group: Empirical Distribution for kurt.hat',xlab='Sampled Values')
abline(v=kurt.time.VV, col='red') ## arguments can be a and b, h, or v

kurt.time.VV

boot.interval

#Thus for the stereogram fusion times, both the samples indicate positive
#skewness while the bootstrap C.I.'s for the kurtosis contain zero. 

