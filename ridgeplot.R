library(ggridges)
library(tidyverse)
library(dslabs)
data(gapminder)
gapminder<-gapminder%>%mutate(dollars_per_day=gdp/population/365)
gapminder<-gapminder%>%mutate(group = case_when(
  region %in% c("Western Europe", "Northern Europe","Southern Europe",
                "Northern America", "Australia and New Zealand") ~ "West",
  region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
  continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
  TRUE ~ "Others"))


gapminder <- gapminder %>%
  mutate(group = factor(group,
                        levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
p<-gapminder%>%filter(year==1970&!is.na(dollars_per_day))%>%ggplot(aes(dollars_per_day,group))+scale_x_continuous(trans = "log2")

p+geom_density_ridges()
p+geom_density_ridges(jittered_points=TRUE)
p+geom_density_ridges(jittered_points=TRUE,position = position_points_jitter(width = 0.05,height = 0),point_shape='|',point_size=3,point_alpha=1,alpha=0.7)
past_year <- 1970
present_year<-2010
wes <- gapminder%>%filter(year %in% c(past_year,present_year)&!is.na(dollars_per_day))%>%mutate(west=ifelse(group=="West","West","Developing"))%>%
  ggplot(aes(dollars_per_day))+geom_histogram(binwidth = 1,color="black")+scale_x_continuous(trans="log2")+facet_grid(year~west)
wes


country_list1<-gapminder%>%filter(year==1970 & !is.na(dollars_per_day))%>%pull(country)
country_list2<-gapminder%>%filter(year==2010 & !is.na(dollars_per_day))%>%pull(country)
country_list<-intersect(country_list1,country_list2)
gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%ggplot(aes(group,dollars_per_day))+geom_boxplot()+theme(axis.text.x=element_text(angle=90,hjust = 1))+scale_y_continuous(trans="log2")+xlab(" ")+facet_grid(.~year)
#to print boxplots side to side we use 
gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%mutate(year=factor(year))%>%
  ggplot(aes(group,dollars_per_day,fill=year))+geom_boxplot()+
  scale_y_continuous(trans="log2")+
  xlab("")

gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%ggplot(aes(dollars_per_day))+
  geom_density(fill="grey")+
  scale_x_continuous(trans="log2")+
  facet_grid(.~year)
gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%mutate(group=ifelse(group=="West","West","Developing"))%>%
  ggplot(aes(dollars_per_day,fill=group))+scale_x_continuous(trans="log2")+geom_density(alpha=0.2)+facet_grid(year~.)

piss<-gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%mutate(group=ifelse(group=="West","West","Developing"))%>%ggplot(aes(dollars_per_day,y=..count..,fill=group))+
  scale_x_continuous(trans="log2",limit=c(0.125,300))
piss+geom_density(alpha=0.2,bw=0.75)+facet_grid(year~.)
gapminder%>%filter(year%in%c(1970,2010)&!is.na(dollars_per_day))%>%ggplot(aes(dollars_per_day,group))+scale_x_continuous(trans="log2")+
  geom_density_ridges(adjust=1.5)+facet_grid(.~year)

gapminder%>%filter(year%in%c(1970,2010)&country%in%country_list)%>%group_by(year)%>%mutate(weight=population/sum(population)*2)%>%ungroup()%>%
  ggplot(aes(dollars_per_day,fill=group))+scale_x_continuous(trans="log2",limits = c(0.125,300))+
  geom_density(alpha=0.2,bw=0.75,position = "stack")+facet_grid(year~.)
