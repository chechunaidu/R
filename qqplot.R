library(dslabs)
library(tidyverse)
data(heights)
p<-heights %>% filter(sex=="Male") %>% ggplot(aes(sample=scale(height)))+geom_qq()+geom_abline()
p
p<-seq(0.05,0.95,0.05)
sample_quantiles<-quantile(heights$height,p)
theoratical_quantiles<-qnorm(p,mean=mean(heights$height),sd=sd(heights$height))

qplot(theoratical_quantiles,sample_quantiles)+geom_abline()

#qqplot are useful to find out that whether given data is normally distrubuted or not.

#barplots
data(murders)
murders%>%ggplot(aes(region))+geom_bar()
#we often use the propotion
tab<-murders%>%count(region)%>%mutate(proportion=n/sum(n))
tab%>%ggplot(aes(region,proportion))+geom_bar(stat = "Identity")

heights%>%ggplot(aes(height))+geom_histogram(binwidth = 1,col="black",fill="blue")+xlab("heights in inches")+ggtitle("Histogram")

#density plots
heights%>%filter(sex=="Female")%>%ggplot(aes(height))+geom_density(fill="blue")

#qqplot
params<-heights%>%filter(sex=="Male")%>%summarise(mean=mean(height),sd=sd(height))
heights%>%filter(sex=="Male")%>%ggplot(aes(sample=height))+geom_qq(dparams = params)+geom_abline()

#images
x<-expand.grid(x=1:12,y=1:10)%>%mutate(z=1:120)
ima<-x%>%ggplot(aes(x,y,fill=z))+geom_raster()
ima
ima+scale_fill_gradientn(colors = terrain.colors(10))

heights%>%qplot(sex,height,data=.,geom = "boxplot")

x<-heights%>%filter(sex=="Male")%>%pull(height)

qplot(x,geom="density")
