library(tidyverse)
library(dslabs)
data(gapminder)
gapminder %>% as_tibble()

gapminder%>%filter(year==2015&country%in%c("Sri Lanka","Turkey"))%>%select(country,infant_mortality)

filter(gapminder,year==1960)%>%ggplot(aes(fertility,life_expectancy))+geom_point()

filter(gapminder,year==1960)%>%ggplot(aes(fertility,life_expectancy,color=continent))+geom_point()

#faceting
filter(gapminder,year%in%c(1960,2012))%>%ggplot(aes(fertility,life_expectancy,color=continent))+geom_point(size=2)+facet_grid(continent~year)

filter(gapminder,year%in%c(1960,2012))%>%ggplot(aes(fertility,life_expectancy,color=continent))+geom_point(size=2)+facet_grid(.~year)
#facet wrap
years<-c(1962,1980,1990,2000,2012)
continents<-c("Europe","Asia")
gapminder%>%filter(year%in%years&continent%in%continents)%>%ggplot(aes(fertility,life_expectancy,col=continent))+geom_point()+facet_wrap(~year)

