library(dslabs)
library(tidyverse)
library(ggthemes)
library(ggrepel)
data(murders)
r<-murders%>%summarise(rate=sum(total)/sum(population)*10^6)%>%pull(rate)
p<-murders%>%ggplot(aes(population/10^6,total,label=abb))+geom_point(aes(col=region),size=3)+geom_text(nudge_x = 0.05)+geom_text_repel()+scale_x_log10()+scale_y_log10()+xlab("Population in millions(log scale)")+ylab("Total murders(log scale)")+ggtitle("us gun murders in 2010")
p+geom_abline(intercept = log10(r),lty=2,color="darkgrey")+scale_color_discrete(name="Region")+theme_economist()

