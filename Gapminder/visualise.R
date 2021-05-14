
gapminder %>% filter(year %in% c(2013 ,2014)) %>% group_by(continent) %>% ggplot(aes(continent,population , fill = factor(continent))) + 
  geom_violin() + facet_wrap(~year) + scale_y_log10()

#let see the mean population of every continets

gapminder %>% filter(year == 2014) %>% ggplot() + geom_pointrange(aes(continent , population ) ,fun.min = min ,fun.max = max , fun = mean , stat = "summary") + scale_y_log10()

# accurding to this plot asia have the highest population mean nut this is due by china and india 

#now let take two country in Africa , America and Europe continent to campare their life expectancy in 2014 


compar <- gapminder %>% filter(year == 2014 & country %in% c("Nigeria","Morocco","United States","Brazil","France","United Kingdom")) %>% select(country , life_expectancy) %>% arrange(life_expectancy)


gapminder %>% filter(country %in% c("Nigeria","Morocco","United States","Brazil","France","United Kingdom")) %>% ggplot(aes(year,life_expectancy , col = factor(country))) + geom_line() 

#now move on to visualize the life expectancy and the fertility 


gapminder %>% filter(year %in% c(1960,2000,2015)&!is.na(life_expectancy)) %>% ggplot(aes(fertility,life_expectancy,col = continent)) + geom_point() + facet_grid(.~year)

#we see from this pot that in days after independance africa and asia semble to have the same life expectancy but we can see how much asia countries improve aver africa countries to be more close to western countruies


#let us campare africa and asia improvement

gapminder %>% filter(year %in% c(1960 ,2000,2015)&continent %in% c("Asia","Africa")) %>% ggplot(aes(fertility,life_expectancy,col = continent)) +
  geom_point() + facet_grid(year~.)


#according to this plot does a relationsheap between fertility and life expectancy? machine learning tool can answers this questio


#visualise multimodal distribution


gapminder$dollars_per_day <- gapminder$gdp/ gapminder$population*1000

gapminder %>% filter(year == 1970&!is.na(gdp)) %>% ggplot(aes(region,dollars_per_day)) + geom_point() + theme(axis.text.x = element_text(angle = 45))

#by default region is order by alphabetic order, we can chang this by rorder function

gapminder %>% filter(year == 1970&!is.na(gdp)) %>% mutate(region = reorder(region,dollars_per_day,FUN = median)) %>% ggplot(aes(region,dollars_per_day)) + geom_point() + theme(axis.text.x = element_text(angle = 45))
  
#compare the gross domestic product of each group of region , first we creat these groupes

gapminder <- gapminder %>% mutate(group = case_when(
  region %in% c("Western Europe","Norther America","Australia and New Zealand")~"West",
  region %in% c("South-Eastern Asia","Eastern Asia")~"East Asia",
  region %in% c("Caribbean", "Central America", "South America") ~ "Latin America", 
  continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
  TRUE ~ "Others"))
 

gapminder <- gapminder %>% mutate(group = factor(group ,levels = c("West","East Asia","Latin America","Sub-Saharan Africa")))


#now we make a violin in order to compare their dollars per day

gapminder %>% ggplot(aes(group , dollars_per_day,fill = group)) + geom_violin() + theme(axis.text.x = element_text(angle = 90)) + scale_y_log10()

#now make a ridge plot

library(ggridges)

gapminder %>% filter(year == 2000) %>% ggplot(aes(dollars_per_day,group,fill = group)) + geom_density_ridges() + scale_x_continuous("log2")












