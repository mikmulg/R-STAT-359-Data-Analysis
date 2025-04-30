smoking<-read.table(file='C:/Users/Mik/Documents/STAT359/data/smoking.txt', sep="", header=TRUE)

##PAIRED DATA: PARAMETRIC AND NON-PARAMETRIC METHODS

#For two sample comparisons, we have thus far assumed that the observations
#are independent.

#It is often the case that the data arise in pairs it is expected that
#the observations constituting each pair will be correlated. 

#This is the case when studies match subjects according to various 
#characteristics, twin or sibling studies, studies that randomize
#treatment within a given subject to left/right eye, littermates, etc.

#Another important example is the case when studies make 'before' and 
#'after' measurements and these two measurements constitute pairs. 


#Pairing/matching when designing an experiment can lead to greater
#efficiency in some settings. 

#Consider pairs (Xi,Yi),i=1,.,n where ??1=E[Xi] and ??2=E[Yi] and 
#interest, as before, is with the difference in population means 
#??1?????2.

#With paired data the observations within each pair will not in 
#general be independent so we will have Cov[Xi,Yi]=??xy 
#(our previous assumption was ??xy=0).

#Our estimator of the unknown parameter of interest will again be 
#X¯???Y¯ which is unbiased E[X¯???Y¯]=??1?????2.

#In the paired case the variance of the estimator is as follows:
#Var[X¯???Y¯]=Var[X¯]+Var[Y¯]???2Cov[X¯,Y¯]
#=(1/n)*(??(2,1) + ??(2,2) ??? 2??XY??1??2) where ??XY is the within-pair 
#correlation for each (Xi,Yi).

#In the unpaired case when we take two random samples each of size n 
#we have E[X¯???Y¯]=??1?????2 and Var[X¯???Y¯]=(1/n)*(??21+??22)

#We see that in cases where the within pair correlation is positive
#??XY > 0 the estimator X¯???Y¯ will have smaller variance in the 
#paired design. 

#This decrease in variance will lead to greater power for hypothesis 
#testing when using a paired design. 

#To handle paired data we will simply form the differences Di=Xi???Yi
#so that E[Di]=??D=??1?????2 and Var[Di]=??2D unknown.

#Dealing with differences reduces the problem to a one sample problem
#and we will consider both parametric and non-parametric approaches. 

#In the parametric case, if we assume that the differences are random
#sample from a normal distribution we can use a one-sample t-test to 
#test H0:??D=??1?????2=0.

#The test is based on the pivotal (D¯?????D)/sD¯???tn???1 leading to the 
#100(1?????)% confidence interval D¯±tn???1,(??/2)sD¯ for ??1?????2 and the test 
#statistic is T=D¯/sD¯.

#This simple procedure is known as the paired t-test. It relies on the
#assumption that the differences are independent and normally 
#distributed. 

##EXAMPLE: EFFECT OF CIGARETTE SMOKING ON PLATELET AGGREGATION

#An experiment is conducted to examine the effect of cigarette 
#smoking on platelet aggregation. 

#The amount of platelet aggregation (measured as a maximum percentage)
#is obtained from blood samples taken from 11 subjects both before
#and after smoking a cigarette. 

#In this case each pair of measurements is collected on the same 
#subject and we expect positive correlation as a result. 

#The data are thus paired before-after measurements of platelet
#aggregation. 

library(knitr)
kable(smoking, caption='Smoking Study Platelet Aggregation')

summary(smoking)

plot(smoking$Before, smoking$After, xlab="Before", ylab = "After")

#estimating the correlation
cor(smoking$Before, smoking$After)

#Plotting the pairs and computing the sample correlation shows a 
#strong positive correlation within pairs. 

#Plotting side-by-side boxplots to compare samples in paired designs
#as we have been in independent two sample designs can be misleading
#as these plots ignore the pairing. 

#Applying a two sample t-test is thus inappropriate here. We apply 
#a paired t-test. 

t.test(smoking$Before, smoking$After, paired = TRUE)

#The p-value suggests strong evidence against the null hypothesis
#(H0:??1=??2) and the 95% confidence interval (-15.63,-4.91) indicates
#that the percentage platelet aggregation increases after smoking. 

#Recall that the paired t-test assumes normality of the differences.

D<-smoking$Before - smoking$After
qqnorm(D)

D

##THE SIGNED RANK TEST

#As an alternative to the paired t-test one can apply a non-parametric
#test which does not rely on the assumption that the differences
#are normally distributed. 

#The key idea to derive test statistics that are based, not on the
#data values themselves, but on the ranks of the data values. 

#When based on ranks, the test procedure becomes robust to outliers. 

#The test statisitic is computed as follows: 

smoking$D<-smoking$Before - smoking$After
smoking$D.abs<-abs(smoking$D)
smoking$Rank<-rank(smoking$D.abs)
smoking$SignedR<-sign(smoking$D)*smoking$Rank
kable(smoking)

#The statistic is then the sum of those ranks having positive sign.
#Here we have W+ = 1

#We compare the observed value of the signed rank statistic W+ to 
#its distribution under the null hypothesis. 

#Under the null there is no difference in the two conditions so 
#roughly half of the differences will be positive. Therefore, the 
#sum of the positive ranks will not be too large or too small. 

#Under the null hypothesis, each of the 2n assignments of signs to 
#ranks is equally likely with probability then 1/2n.

#Each assignment will lead to some not necessarily unique value of 
#W+. Tabulating the unique values and their associated probabilities, 
#k/2n where k is the number of assignments of signs to ranks that 
#yields a particular value of the statistic then yields the null 
#distribution.

#It is instructive to derive E[W+] and Var[W+] under the null
#hypothesis.

#We can write W+=???(n,k=1)kIk where Ik is an indicator that the kth 
#largest Di is assigned a positive sign.

#Under the null hypothesis we have Ik???iidBernoulli(1/2) so that
#E[Ik]=1/2 and Var[Ik]=1/4.

#We then have
#E[W+]=(1/2)???(n,k=1)k=(n(n+1))/4
#Var[W+]=???(n,k=1)k^2Var[Ik]=(1/4)???(n,k=1)k^2
#=(1/4)*(n(n+1)(2n+1))/6 = (n(n+1)(2n+1))/24
#as the mean and variance of W+ under H0.

n<-nrow(smoking)
E.W<-n*(n+1)/4
V.W<-n*(n+1)*(2*n+1)/24
E.W
sqrt(V.W)

#It appears as though the observed value of W+ = 1 is quite extreme
#relative to the mean and standard deviation under the null hypothesis.

wilcox.test(smoking$Before, smoking$After, paired=TRUE)

#The p-value also indicates reasonable strong evidence against the 
#null hypothesis that smoking as no effect on platelet aggregation. 


##EXAMPLE: MEASURING MERCURY LEVELS IN FISH

#A study is conducted in order to compare two methods for 
#measuring the mercury levels in fish. 

#The first method is known as selective reduction while the second
#method is the permanagante method. 

#The mercury levels of 25 juvenile black marlin is measured using
#both techniques. 

fish<-read.table(file='C:/Users/Mik/Documents/STAT359/data/fish.txt', sep="", header=TRUE)

library(knitr)
kable(fish, caption='Mercury Study of Fish')

summary(fish)

plot(fish$Selective_Reduction, fish$Permanganate, xlab='Selective Reduction', ylab='Permanganate')

cor(fish$Selective_Reduction, fish$Permanganate)

wilcox.test(fish$Selective_Reduction, fish$Permanganate, paired=TRUE)

#There is no strong evidence against the null hypothesis and it 
#appears that there is no difference in the performance of the two 
#methods of measurement. 

t.test(fish$Selective_Reduction, fish$Permanganate, paired=TRUE)

D<-fish$Selective_Reduction - fish$Permanganate
qqnorm(D)

#The paired t-test yields similiar conclusions though with a 
#smaller p-value. 

#It is not clear that the paired t-test is appropriate in this case
#as a result of 3 observations in the left tail of the sample. 





##from what I understand: parametric -> use t-test (based on 
#assumption that it's normally distributed)
#non-parametric -> use the wilcox t-test

#parametric relies on assumption that data is normally distributed
#for the paired t-est and independent

