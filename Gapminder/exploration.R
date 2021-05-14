library(tidyverse)
library(dslabs)
library(plyr)
#we use gapminder data set in data science library

data("gapminder")

#our data set have 10545 row and 9 column

dim(gapminder)

# let visualise the population and the and the gross domestic product , not that there is some not availible value

pop_gdp <- gapminder %>% filter(!is.na(gdp)) %>%  ddply(~continent , summarise , total_pop = sum(population) , total_gdp = sum(gdp)/sum(population)*1000 , countries = length(country))

#based on the output Asia semble to have the highest population and oceania the top gdp






