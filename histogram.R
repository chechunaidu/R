library(dslabs)
library(tidyverse)
data(gapminder)

gapminder<-gapminder%>%mutate(gdpperday=gdp/population/365)
p<-gapminder%>%filter(year==1970&!is.na(gdp))%>%ggplot(aes(gdpperday))+geom_histogram(binwidth = 1,color="black")
p
gapminder%>%filter(year==1970&!is.na(gdp))%>%ggplot(aes(log2(gdpperday)))+geom_histogram(binwidth = 1,color="black")

q<-gapminder%>%filter(year==1979&!is.na(gdp))%>%ggplot(aes(region,gdpperday))+geom_point()
#if we plot above graph we get graph having x-axis labels overlapping each other
q+theme(axis.text.x = element_text(angle=90,hjust = 1))
#if we plot above graph we get a graph having labels orderd in alphabetical order. we use reorder function to reorder them according to our wish
r<-gapminder%>%filter(year==1970&!is.na(gdp))%>%mutate(region=reorder(region,gdpperday,FUN=median))%>%ggplot(aes(region,gdpperday))+geom_point()+theme(axis.text.x = element_text(angle = 90,hjust=1))
#above plot is used to reorder the plot according to the median of the gdpperday of the region.
r
r+scale_y_continuous(trans = "log2")


