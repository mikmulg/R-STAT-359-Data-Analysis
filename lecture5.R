##More on Two Sample Testing: t-tests

#We have examined two sample testing for testing the equality of two 
#population means in the large sample setting. 

#In the small sample setting we have discussed how the bootstrap can be used
#to obtain a C.I. for a parameter representing the difference between two
#populations (Ex. the difference in population medians)

#An alternatic approach for testing the equality of population means in the 
#small sample setting is the two sample t-test. 

#Relative to the large sample case, the t-test is developed based on making
#stronger assumptions on the sampling distribution. 

#The key assumption is that the data arise from a normal distribution. 
#Additional assumptions are made on the variance of the two population 
#distributions as well as whether the samples are independent or paired.


#Together these assumptions lead to three procedures based on the 
#t-distribution:
#1.Pooled t-test (assumption of equal variance)
#2.Welch t-test (no assumption of equal variance)
#3.Paired t-test (two samples are dependent; data arises in pairs)


#In the first case, we assume that Xi???(iid)N(??1,??2),i=1,.,m in the first 
#sample and Yi???(iid)N(??2,??2),i=1,.,n in the second sample. We further 
#assume that the data in the two samples are independent.

#We note that this model for m+n observations has three parameters ??1, ??2
#and ??2. The variance is assumed to be ??2 in both populations.

#Under the normal assumption we have, for any sample size, 
#X¯???N(??1,??2/m) and Y¯???N(??2,??2/n) with X¯ and Y¯ being independent.

#We thus have X¯???Y¯???N(??1?????2,??2(1m+1n))and therefore,
#(X¯???Y¯???(??1?????2))/?????((1/m)+(1/n))???N(0,1).

#The common population variance ??2 is unknown and must be estimated. We 
#can estimate it using a weighted average of the sample variance obtained 
#from the two samples:
#s(2,p)=((m???1)s(2,1)+(n???1)s(2,2))/m+n???2
#with weights w1=(m???1)/m+n???2 and w2=(n???1)/m+n???2 depending on each of the 
#sample sizes.

#The weighting is appropriate since if one sample is larger than the other
#then the estimate of ??2 arising from that sample will be more reliable. 

#Unlike the large sample case, when we replace ?? with the estimate sp we 
#have to account for the additional variability introduced in the 
#denominator and the distribution is no longer normal. 

#Under the assumptions of normality of the observations, independence and
#constant variance we can show that: 
#T = (X¯???Y¯???(??1?????2))/sp???((1/m)+(1/n)) ??? t(m+n???2).

#In this case p-values and C.I's can be computer as in the large sample
#case with Z?? replaced with tm+n???2,??, the new distribution of the pivotal 
#quantity.

#The distributional result underlying the pivotal rests on the 
#distribution of the sample variance in the normal case, where for a 
#random sample it is the case that (m???1)s21/??2?????2m???1 and similarly, 
#(n???1)s21/??2?????2n???1 independently and that s2 is independent of X¯ under 
#independent normal sampling.

#Given this it follows that (m???1)s21/??2+(n???1)s22/??2?????2m+n???2 independent 
#of X¯???Y¯???N(??1?????2,??2((1/m)+(1/n))).

#Now letting U=(X¯???Y¯???(??1?????2))/?????((1/m)+(1/n))
#and V=???[(m???1)s21/??2+(n???1)s22/??2]1/(m+n???2) we note that t=U/V is the ratio
#of a N(0,1) distribution with a ??2m+n???2 divided by its degrees of freedom, 
#with U and V being independent.

#The unknown parameter ?? cancels in the ratio and we have that
#t=X¯???Y¯???(??1?????2)sp???((1/m)+(1/n))???tm+n???2.

#The test statistic for testing H0:??1?????2=???? is thus 
#(X¯???Y¯???????)/sp???(1/m)+(1/n) and the null distribution is tm+n???2.

#Starting with 
#Pr(???tm+n???2,??2 ??? (X¯???Y¯???(??1?????2))/sp???((1/m)+(1/n)) ??? tm+n???2,??2)=1?????
#and solving for ??1?????2 in the middle we obtain
#(X¯???Y¯) ± tm+n???2, ??2sp???((1/m)+(1/n))
#as the 100(1?????) confidence interval for ??1?????2.

#This interval is valid for any sample size provided that the data
#are independently drawn and normally distributed with the same variance
#in the two populations underlying the samples. 

#EXAMPLE: LATENT HEAT OF FUSION ICE

#Two methods, A and B, are used to determine the latent heat of fusion of
#ice. Investigators want to determine if and by how much the two methods
#differ. 

ice<-read.table(file='C:/Users/Mik/Documents/STAT359/data/latent_heat.txt', sep="", header=TRUE)
library(knitr)
kable(ice, caption='Data Table')

#The data are the change in total heat from ice at -0.72 degrees C to water
#0 degrees C in calories per gram of mass. 

methodA<-ice$Method_A[!is.na(ice$Method_A)]
methodB<-ice$Method_B[!is.na(ice$Method_B)]
boxplot(methodA, methodB, names=c('Method A', 'Method B'), col='blue')

#The distributions in the two samples appear to be reasonanly well seperated. 

t.test(methodA, methodB, alternative="two.sided", mu=0, var.equal = TRUE)

#There is strond evidence against the null hypothesis that the population
#means associated with Method A and Method B are equal. 

#The 95% C.I. (0.01669058, 0.06734788) gives an indication of the 
#estimated degree to which the mean change in total heat from Method A 
#is higher than that of Method B. 

qqnorm(methodA, main='Method A Data')
qqnorm(methodB, main='Method B Data')

#The plots do not indicate any serious departures from normality
#though it is a matter of judgement when the sample size is this small. 

var(methodA)
var(methodB)

#Examination of the box-plots and the numerical values of the variance
#suggests that a procedure that does not assume equality of variance
#might be an alternative to consider. Regardless, examination of the sample
#data suggests that the result with regards to population difference 
#will not change. 

#Dropping the assumption of equal variance in both populations yields a 
#four parameter model Xi???(iid)N(??1,??(2,1),i=1,.,n in the first sample and
#Yi???iidN(??2,??(2,2),i=1,.,m in the second sample.

#The four parameter model provides more flexibility but has an additional
#parameter to estimate. The samples are therefore not pooled when 
#estimating the variance. 

#The Pivotal in this case is (X¯???Y¯???(??1?????2))/(???(s(2,1)/m) +(s(2,2)/n))
#where the denominator is an estimate of ???(??(2,1)/m)+(??(2,2)/n).

#Note that this is the same pivotal that was used in developing the large
#sample procedures; however, in this case the distribution may not be well
#approximated by a N(0,1).

#The distribution is approximately t with the degrees of freedom obtained
#by: df=((s2of1/m + s2of2/n)^2)/(((s2of1/m)^2)/(m-1) + ((s2of2/n)^2)/(n-1))
#rounded to the nearest integer. 




#Considering the latent heat example again without the assumption of
#equal variance. 

t.test(methodA, methodB, alternative = "two.sided", mu=0, var.equal = FALSE)

#Comparing to the results of the pooled t-test: 

t.test(methodA, methodB, alternative = "two.sided", mu=0, var.equal = TRUE)


##EXAMPLE: STUDY OF IRON RETENTION

#Investigators are interested in determining if two forms of iron (Fe2+, Fe3+)
#are retained differently in order to determine if either would make a 
#better dietary supplement. 

#An experiment is conducted where 108 mice are divided randomly into 6 groups
#with 18 mice in each group. 

#Three groups are give Fe2+ in three different concentrations. 10.2, 1.2, 0.3
#millimolar. The other three are give Fe3+ at the same concentrations. 

#For each mouse, an initial amount of iron is given and then a measurement
#representing the percent retained is obtained. 

#We will examine a comparison of two out of the six groups corresponding
#to Fe2+ (1.2) and Fe3+ (1.2) in order to examine the evidence of a 
#difference in mean iron retention between groups. 

iron<-read.table(file='C:/Users/Mik/Documents/STAT359/data/iron.txt', sep="", header=TRUE)
kable(iron, caption='Data Table')

boxplot(iron$Fe3, iron$Fe2, names=c('Fe3+', 'Fe2+'), col='blue', main='Percent Retained')

#The two samples appear to overlap to a very large degree. 
qqnorm(iron$Fe3)
qqnorm(iron$Fe2)

#Both the boxplots and the qqplots indicate right-skewed distributions
#suggesting potential problems with the assumption of normality. 

#In the case of skewed positive valued data, a log transformation can 
#be useful in restoring symmetry. The t-test is then applied to the 
#log transformed data. 

boxplot(log(iron$Fe3), log(iron$Fe2), names=c('Fe3+', 'Fe2+'), col='blue', main='Percent Retained - Log Transformed')

qqnorm(log(iron$Fe3))
qqnorm(log(iron$Fe2))

#The log-transformed data appears to be more symmetric and we can apply 
#a t-test to these data. Note, the null hypothesis now corresponds to 
#the means of the log-transformed data. 

t.test(log(iron$Fe3), log(iron$Fe2), alternative="two.sided", mu=0, var.equal = TRUE)

#At this particular concentration, there is not sufficient evidence to 
#suggest that the two forms of iron are retained differently. 

#The 95% C.I. for the difference in mean log percentage retained between
#the Fe2+ and Fe3+ groups is (-0.6071510, 0.2295115).

#In order to decide with of either the pooled or Welch t-test to apply
#one can informally look at the variability of the samples as we have been. 

#More formally, one can precede a t-test with a test comparing the 
#variance of the population distributions. 

#In this case we have H0:??(2,1)=??(2,2) and typically it will be the two-sided
#alternative that is of interest.

#One can apply the variance test and based on the results apply the pooled
#t-test of Welch t-test. 

#The test is based on the F-distribution. Let X?????2m and Y?????2n with X and Y i
#ndependent random variables. Then Z=(X/m)/(Y/n) defines the Fm,n-distribution.
#This family of distributions is indexed by the two parameters m,n???1.

x<-seq(0,5,.01)
dF5_5<-df(x,df1=5,df2=5)
dF5_20<-df(x,df1=5,df2=20)
dF10_20<-df(x,df1=10,df2=20)
dF15_15<-df(x,df1=15,df2=15)

# set up the plot area
plot(c(min(x),max(x)),c(min(dF5_5,dF5_20,dF10_20,dF15_15),max(dF5_5,dF5_20,dF10_20,dF15_15)),type="n",
     xlab="x",ylab="Density function: f(x)")
title("Densities of the F distribution")
lines(x,dF5_5)
lines(x,dF5_20,col="red")
lines(x,dF10_20,col="green")
lines(x,dF15_15,col="blue")
legend(x='topright',legend=c('F5_5','F5_20','F10_20','F15_15'),fill=c('black','red','green','blue'))

#The F-test for H0:??21=??22 is based on the statistic
#T=max{s21,s22}/min{s21,s22} which, assuming the data are independent 
#and normally distributed and H0 is true has a n F(m???1,n???1) distribution.


##EXAMPLE: LATENT HEAT OF FUSION OF ICE

var.test(methodA, methodB)

#There is not sufficient evidence indicating the population variances
#are unequal. 

#A 95% C.I. for the ratio of the population variances is (0.1251097,2.1052687)


##EXAMPLE: STUDY OF IRON RETENTION

var.test(log(iron$Fe3), log(iron$Fe2))

#There is not sufficient evidence indicating the population of variances
#are unequal. 

#A 95% C.I. for the ratio of the population variances is (0.4929279,3.5227266)