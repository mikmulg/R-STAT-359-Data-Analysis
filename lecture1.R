worms<-read.table(file='C:\Users\Mik\Documents\STAT359\data', sep="", header=TRUE, na.strings="NA")
names(worms)
attach(worms)
summary(worms)
library(knitr)
kable(worms, caption='Data Table')
mean(Area)
Area[3]
Area[Area<3]
Slope[Slope<3]
Area[Area>mean(Area)]
length(Area[Area>mean(Area)])
worms[2,2]
worms[4,4]
worms[Area>3 & Slope<3,]
worms[Damp=="TRUE"]
worms[Vegetation=="Shrub",]
worms[Damp=="TRUE",]
worms[Soil.pH =="4.1",]


kable(memory, caption="Data Table")
boxplot(memory$ginkgo, memory$Placebo, col='blue', names=c('Ginkgo', 'Placebo'))
title('Memory Score')
summary(memory)

ginkgo<-memory$ginkgo
Placebo<-memory$Placebo
ginkgo<-ginkgo[!is.na(ginkgo)]
Placebo<-Placebo[!is.na(Placebo)]
length(ginkgo)
length(Placebo)

mean(ginkgo)
mean(Placebo)

median(ginkgo)
median(Placebo)


sample.median<-function(y)
{
  n<-length(y)
  y.ordered<-sort(y)
  odd<-n%%2
  if (odd==1){
    index<-ceiling(n/2)
    return.value<-y.ordered[index]
  }
  else{
    index1<-n/2
    index2<-n/2 - 1
    return.value<-(y.ordered[index1]+y.ordered[index2])/2
  }
return.value
}

sample.median(ginkgo)
sample.median(Placebo)

y<-rnorm(n=20, mean=2, sd=1)
hist(y)
boxplot(y)

y2<-y
index.of.max<-which.max(y2)
y2[index.of.max]<-10000

par(mar=c(1,1,1,1))
boxplot(y,y2)

boxplot(y,y2,outline=F)

c(mean(y), mean(y2))
c(median(y), median(y2))

var(ginkgo)
var(Placebo)

(1/(length(ginkgo)-1))*sum((ginkgo-sum(ginkgo)/length(ginkgo))^2)


alpha<-0.05
lower<-mean(ginkgo)-mean(Placebo)-qnorm(1-(alpha/2))*sqrt((var(ginkgo)/length(ginkgo))+(var(Placebo)/length(Placebo)))
upper<-mean(ginkgo)-mean(Placebo)+qnorm(1-(alpha/2))*sqrt((var(ginkgo)/length(ginkgo))+(var(Placebo)/length(Placebo)))
c(lower,upper)

T.obs<-((mean(ginkgo)-mean(Placebo))/sqrt((var(ginkgo)/length(ginkgo))+(var(Placebo)/length(Placebo))))
T.obs

pnorm(0)

pnorm(1.1, mean=2, sd=sqrt(2))


1-pnorm(T.obs)
