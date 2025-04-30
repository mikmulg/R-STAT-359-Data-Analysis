##QUANTILE PLOTS

#Review some of the basic probability distributions that we will see in 
#the course and discuss simulating from these in R as well as comparing
#distributions using quantile-quantile plots

#Chi-square Distribution
#If Z???N(0,1) we say that the random variable defined by X=Z2 is ??2(1).

#More generally if Z1,.,Zk are independent N(0,1) then X=???ki=1Z2i 
#is said to be ??2(k).

#The density function of a ??2(k) distribution takes the form
#f(x)=12k/2??(k/2)x(k/2)???1exp{???x/2},x???0

## make several plots of the Chi-Square density function
x<-seq(0.01,12,0.01)
y.df2<-dchisq(x,df=2)
y.df3<-dchisq(x,df=3)
y.df4<-dchisq(x,df=4)
plot(c(0,12),c(0,max(y.df2,y.df3,y.df4)),type='n', ylab='Density Function',xlab='x')
title('Density of Chi-Square(df)')
lines(x,y.df2,col='blue')
lines(x,y.df3,col='green')
lines(x,y.df4,col='red')
legend(x=c(6,9),y=c(0.4,.2),legend=c('df=2','df=3','df=4'),fill=c('blue','green','red'))


#If X?????2(4) compute P(X???4) and if Y?????2(3) compute P(Y???4).

x.prob<-1-pchisq(q=4,df=4)
x.prob

y.prob<-1-pchisq(q=4,df=3)
y.prob

#If X and Y are independent how can we compute P(X+Y???4)?

#We have that X=???(4,i=1)Z(2,i) and Y=???(3,i=1)W(2,i) where the Z's and W's 
#are all mutually independent N(0,1) random variables.

#We then have X+Y=???(4,i=1)Z(2,i) + ???(3,i=1)W(2,i) and this representation 
#is that of a ??(2,7) random variable.

x.y.prob<-1-pchisq(q=4,df=7)
x.y.prob

#If X?????(2,(4)) compute the median and 0.7 quantile of the distribution.

#Noting that the median is the 0.5 quantile we want the solutions to the 
#following equations P(X???q0.5)=0.5 for the median and P(X???q0.7)=0.7
#for the 0.7 quantile.

q5<-qchisq(p=0.5,df=4)
q5

q7<-qchisq(p=0.7, df=4)
q7

#Simulation from the ??2 distribution

y.chisq2<-rchisq(n=1000,df=2)
y.chisq3<-rchisq(n=1000,df=3)
y.chisq4<-rchisq(n=1000,df=4)
y.chisq7<-rchisq(n=1000,df=7)
par(mfrow=c(2,2))
hist(y.chisq2,main='Chi-Square(2) Draws')
hist(y.chisq3,main='Chi-Square(3) Draws')
hist(y.chisq4,main='Chi-Square(4) Draws')
hist(y.chisq7,main='Chi-Square(7) Draws')


#t-distribution
#If Z???N(0,1) and W?????2(n) with Z and W assumed independent then the
#random variable defined by X=Z/???(W/n) defines a tn distribution.

#The tn distriubtion is shaped like the normal distribution but 
#it has heavier tails. 


# create a plot comparing the densities of the normal, t and chi-square distributions
x<-seq(-5,5,.01)
y.norm<-dnorm(x,mean=0,sd=1)
y.t1<-dt(x,df=1)
y.t2<-dt(x,df=2)
y.t10<-dt(x,df=10)



# set up the plot area
plot(c(min(x),max(x)),c(min(y.norm,y.t1,y.t2,y.t10),max(y.norm,y.t1,y.t2,y.t10)),type="n",
     xlab="x",ylab="Density function: f(x)")
title("Comparing the Normal and t-distribution")
lines(x,y.norm)
lines(x,y.t10,col="red")
lines(x,y.t2,col="green")
lines(x,y.t1,col="blue")
legend(x=c(2,4),y=c(.4,.25),legend=c('Normal','df=10','df=2','df=1'),fill=c('black','red','green','blue'))

#If X???t3 compute P(X???4) and if Y???t10 compute P(Y???4).

x.prob<- 1 - pt(q=4, df=3)
x.prob

y.prob<- 1 - pt(q=4, df=10)
y.prob

#Determination of the 0.975 quantile (q0.975) of the t5 distribution

qt(p=0.975,df=5)

qnorm(p=0.975)

#Simulation from the tn distribution

y.t1<-rt(n=1000,df=1)
y.t2<-rt(n=1000,df=2)
y.t10<-rt(n=1000,df=10)
y.norm<-rnorm(n=1000,mean=0,sd=1)


par(mfrow=c(2,2))
hist(y.t1,main='t(1) Draws')
hist(y.t2,main='t(2) Draws')
hist(y.t10,main='t(10) Draws')
hist(y.norm,main='N(0,1) Draws')

#POISSON DISTRIBUTION 
#A discrete distribution that is often used for count data taking 
#values x=0,1,2,3,.

#Probabilities are computed as  P(X=k)=exp{?????}??k/k!,??>0,k=0,1,2,.

#The distribution has E[X]=Var[X]=?? which is a restriction that one 
#needs to keep in mind when using the Poisson distribution for real data.

#Probabilities, quantiles and simulations in R: ppois(), qpois(), rpois().

#We will see the Poisson distribution when we discuss regression models 
#for count data (Poisson log-linear models).

#BINOMIAL DISTRIBUTION
#A discrete distribution that is often used for binary data or data
#taking proportion (x/n) values. 

#X~Bin(n,p) then probabilities are calculated as P(X=k) = (n k)p^k(1-p)^(n-k)
#where p???[0,1]

#The distribution has E[X]=np and Var[X]=np(1???p).

#Probabilities, quantiles, and simulations in R: pbinom(), qbinom(), rbinom().

#We will see the binomial distribution when we discuss regression models
#for binary and data and proportions (logistic regression).

# simulate 1000 values from a Poisson distribution with E[X] = 3
x.poisson3<- rpois(n=1000, lambda=3)
# simulate 1000 values from a binomial distribution with E[X] = 
x.binom3<-rbinom(n=1000,size=10,prob=0.3)
par(mfrow=c(2,1))
hist(x.poisson3, main='Simulated Poisson(3) Values')
hist(x.binom3, main='Simulated Bin(10,0.3) Values')

#QUANTILE-QUANTILE PLOTS

#Suppose we have a sample of data y1,...,yn and we want to qualitatively
#assess the fit of this sample to some theoretical distribution (most often
#a normal distribution).

#We can sort the data in ascending order leading to the so called 
#order-statistics y(1),y(2),...,y(n).

#We next consider the theoretical distribution of interest and consider a 
#hypothetical sample of size n, X1,...,Xn and the distribution of the 
#order statistics under repeated sampling X(1),...,X(n).

#To compare the data to the theoretical distribution we can plot the sample
#order statistics y(1),y(2),...,y(n) against the expected order statistics
#E[X(1)],.,E[X(n)].

#A good fit to the theoretical distribution will result in a roughly linear
#plot; whereas, deviations from linearity can indicate departures from the
#theoretical distribution in a manner that can be visualized. 

#We often want to make comparisons to the normal distribution. 

x.norm<-rnorm(n=200,mean=3,sd=2)
qqnorm(x.norm,main='QQ Plot Normal - Normal')

#A linear plot is expected when the data are approximately normally distributed.

x.t4<-rt(n=200,df=3)
qqnorm(x.t4,main='QQ Plot t4 - Normal')

#In this case the plot bends down in the left tail and bends up in the right
#tail because the sample quantiles are larger in magnitude than those expected
#under a normal distribution.

#This is an indication that the sampling distribution has heavier tails than
#the normal distribution. 

x.chisq3<-rchisq(n=200,df=3)
qqnorm(x.chisq3,main='QQ Plot Chisq(3) - Normal')

#In this case the plot is non-linear with a 'bow' shape. The plot bends up 
#in the right tail and also bends up in the left tail indicating that the 
#right tail is long relative to the left tail. This is a positively or 
#right skewed distribution. 

x.uniform2<-runif(n=200,min=-1.5,max=1.5)
qqnorm(x.uniform2,main='QQ Plot Unif(-1.5,1.5) - Normal')

#In this case the plot bends down in the right tail and bends up in the 
#left tail indicating that both tails are lighter than those of the 
#normal distribution. 

#STEROGRAMS EXAMPLE
#Recall the response data in this case are the fusion times, that is, the 
#time for the subject to recognize the image within the sterograms. 

#One group of subjects (group NV) in this experiment were randomized to 
#receive no information or just verbal information about the shape of 
#the embedded object. 

#A second group (group VV) were randomized to receive both verbal info. and 
#visual info. (specifically, a drawing of the object).

#Data in the form of event times or reaction times are often asymmetric in 
#their distribution. 

stereograms<-read.table(file ='C:/Users/Mik/Documents/STAT359/data/stereograms.txt', sep="",header=TRUE)
time.NV<-stereograms$fusion_time[stereograms$group=='NV']
time.VV<-stereograms$fusion_time[stereograms$group=='VV']
boxplot(time.NV,time.VV,col='green',names=c('Time - NV','Time - VV'))
title('Stereogram Fusion Times')

qqnorm(time.NV,main='QQ-Plot: No/Verbal Information')

qqnorm(time.VV,main='QQ-Plot: Verbal/Visual Information')

#Both QQ-plots have a bow shape indicating an asymmetric distribution with
#right tail longer than the left tail. 

#The sample of fusion times associated with subjects receiving verbal or 
#visual information appears to have a greater degree of asymmetry than the
#sample associated with subjects receiving no verbal information. 

