#position,aligned lenght,angle,aera,brightness,color hue
#alway start by 0 when we want to make a barplot because it is proportional to the lenght
#for th position plot we can start by zero
#use area instead of radius
#Use transformations when warranted to ease visual interpretation.
#The log transformation is useful for data with multiplicative changes. The logistic transformation is useful for fold changes in odds. The square root transformation is useful for count data.

  library(tidyverse)
  library(dslabs)
  
  data(mpg)  

mpg %>% ggplot(aes(x = displ ,y = hwy ))  + geom_point() + facet_wrap(.~class,nrow = 2)
 head(mpg)
 mpg %>% ggplot(aes(x = displ ,y = hwy ))  + geom_point() + facet_grid(.~cyl)
 
 mpg %>% ggplot(aes(x = displ ,y = hwy ))  + geom_smooth(aes(linetype = drv,col = drv )) + geom_point(aes(color = drv))

 gapminder %>% filter(country %in% c( "Togo","Ghana") & !is.na(gdp)) %>% mutate(dollars_per_day = gdp/population/365) %>% ggplot(aes(year,dollars_per_day,color = country)) +
   geom_line()
 
 mpg %>% ggplot(aes(x = displ ,y = hwy ))  + geom_smooth(aes(col = drv,linetype = drv),se = TRUE,show.legend = TRUE) + geom_point(aes(color = drv))

 diamonds %>% ggplot(aes(cut)) + geom_bar(fill = "blue",col = "black") 

 ?geom_histogram
 ?stat_bin

 ?geom_summary
?geom_boxplot 
 
 