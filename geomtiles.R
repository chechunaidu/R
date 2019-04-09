library(RColorBrewer)
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
dat<-us_contagious_diseases%>%filter(!state%in%c("Hawaii","Alaska")&disease=="Measles")%>%mutate(rate=count/population*10000*52/weeks_reporting)%>%
  mutate(state = reorder(state,rate))

dat%>%filter(state=="California"&!is.na(rate))%>%ggplot(aes(year,rate))+
  geom_line()+ylab("cases for 10000")+geom_vline(xintercept = 1963,col="blue")

dat%>%ggplot(aes(year,state,fill=rate))+geom_tile(color="grey50")+scale_x_continuous(expand = c(0,0))+
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9,"Reds"),trans="sqrt")+geom_vline(xintercept = 1963,col="blue")+
  theme_minimal()+theme(panel.grid = element_blank())+ggtitle("Measels")+xlab("")+ylab("")


avg <- us_contagious_diseases%>%filter(disease=="Measles")%>%group_by(year)%>%summarise(us_rate=sum(count,na.rm = TRUE)/sum(population,na.rm = TRUE)*10000)

avf<-dat%>%filter(!is.na(rate))%>%ggplot()+geom_line(aes(year,rate,col=state),show.legend = FALSE,alpha=0.2,size=1)+
  geom_line(mapping = aes(year,us_rate),data=avg,size=1,color="black")+scale_y_continuous(trans="sqrt",breaks=c(5,25,125,300))+
  ggtitle("Cases per 10000 by state")+xlab("")+ylab("")+geom_text(data=data.frame(x=1955,y=55),mapping = aes(x,y,label="US average"),color="black")+geom_vline(xintercept = 1963,col="blue")
avf
