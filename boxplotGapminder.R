library(dslabs)
library(tidyverse)
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
p <- gapminder %>%
  filter(year == 1970 & !is.na(gdp)) %>%
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")

p+geom_point(alpha=0.5)


