library(tidyverse)
library(HistData)
 data("Prostitutes")
names(Prostitutes) 
data("temp_carbon")

?Prostitutes
  head(Prostitutes)
  Prostitutes %>%  group_by(Year) %>% mutate(pros_per_year = mean(count) ) %>% 
ggplot(aes(Year,pros_per_year)) + geom_point()
# compare the number of prostitute per year

  Prostitutes %>% filter(Year %in% c(1813,1850)) %>% ggplot(aes(y = count,col = factor(Year))) +
  geom_boxplot() 

# temp and carbon emission
  carbon_ocean <- c(temp_carbon$ocean_anomaly,temp_carbon$carbon_emissions)
  temp_carbon %>%  ggplot(aes(carbon_emissions,ocean_anomaly)) + geom_point() 
  
# times series plot of carbone emission
 head(temp_carbon)

  temp_carbon %>% ggplot(aes(carbon_emissions)) + geom_density(fill = "blue",col = "black")


data("gapminder")
gapminder %>% filter(year == 2012 & continent == "Africa") %>%
  ggplot(aes(fertility,life_expectancy,color = region)) + geom_point()

df <- data.frame(gapminder %>% filter(continent == "Africa" & year == 2012 & fertility <= 3 & life_expectancy >= 70) %>% 
                 select(region,country) ) 
df

# vietnam vs usa
   tab <- gapminder %>% filter(year %in% seq(1960,2010) & country %in% c("Vietnam","United States"))
tab %>% ggplot(aes(year,life_expectancy,color = country)) + geom_line()

# cambodia time series plot

gapminder %>% filter(year %in% seq(1960,2010) & country == "Cambodia") %>% 
  ggplot(aes(year,life_expectancy)) + geom_line()

daydollars <- gapminder %>% filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp / population / 365)

  daydollars %>% ggplot(aes(dollars_per_day)) + geom_density(fill = "blue") + scale_x_continuous(trans = "log2")

library(colorspace)

  gapminder %>% filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp)) %>% 
     mutate(dollars_per_day = gdp / population / 365) %>% ggplot(aes(dollars_per_day,fill = region)) +
    geom_density(bw = 0.5,position = "stack") + facet_grid(year~.) + scale_x_continuous(trans = "log2")

 gapminder_Africa_2010 <-  gapminder %>% filter(year == 2010 & continent == "Africa" & !is.na(gdp)) %>% mutate(dollars_per_day = gdp / population / 365)
  
 gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality,col = region)) + geom_point()

 gapminder_Africa_2010 %>% ggplot(aes(infant_mortality,dollars_per_day,color = region)) + geom_point()


 gapminder_Africa_2010 %>%  ggplot(aes(dollars_per_day,infant_mortality,col = region,label = country)) +
   geom_point() + geom_text(nudge_y = 2) + scale_x_continuous(trans = "log2")
 
 
 gapminder %>% filter(continent == "Africa" & year %in% c(1970,2010) & !is.na(gdp)) %>% 
   mutate(dollars_per_day = gdp / population / 365) %>% 
   ggplot(aes(dollars_per_day,infant_mortality,color = region,label = country)) +
   geom_point() + geom_text() + scale_x_continuous(trans = "log2") + facet_grid(year~.)
 z <- rnorm(1000)
       
 library(dslabs)
 data("gapminder")
 library(tidyverse)
 
 west <- c("Western Europe","Northern Europe","Southern Europe", "Northern America","Australia and New Zealand")
 dat <- gapminder %>%
   filter(year%in% c(2010, 2015) & region %in% west &
            !is.na(life_expectancy) & population > 10^7)
 dat %>%
   mutate(location = ifelse(year == 2010, 1, 2),
          location = ifelse(year == 2015 &
                              country %in% c("United Kingdom","Portugal"),
                            location+0.22, location), hjust = ifelse(year == 2010, 1, 0)) %>%
   mutate(year = as.factor(year)) %>%
   ggplot(aes(year, life_expectancy, group = country)) + geom_line(aes(color = country), show.legend = FALSE) + geom_text(aes(x = location, label = country, hjust = hjust),
                                                                                                                          show.legend = FALSE) + xlab("") +
   ylab("Life Expectancy")      
 
  library(tidyverse)
  library(dslabs) 

 data("gapminder") 
color_blind <-   scale_color_manual( values = c("#000000", "#E69F00", "#56B4E9", "#009E73 ", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
 gapminder %>% filter(year == 2015 & !is.na(population)) %>%  mutate(continent = reorder(continent,population,FUN = median))  %>% 
  ggplot(aes(continent,population)) + geom_col() + color_blind
 color_blind_friendly_cols <-
   c("#999999", "#E69F00", "#56B4E9", "#009E73",
     "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
 color_blind_friendly_cols 
 
 #slop chart
 
 West <- c("Northern Europe","Western Europe","Northern America","Australia New Zealand","Southern Europe")

 dat <- gapminder %>% filter(year %in% c(2015,2010) & !is.na(life_expectancy) & region %in% West & population > 10^7 )
 
 dat %>% mutate(location = ifelse(year == 2010, 1,2),
                location = ifelse(year == 2015 & country %in% c("United Kingdom","Portugal"),location + 0.22,location) ,
                  hjust = ifelse(year == 2010,1,0)) %>% mutate(year = as.factor(year)) %>% 
    ggplot(aes(year,life_expectancy,group = country)) + geom_line(aes(color = country)) + geom_text(aes(x = location ,label = country,hjust = hjust)) +
   xlab(" ") + ylab("life expectancy")

 
 
 
 west <- c("Western Europe","Northern Europe","Southern Europe", "Northern America","Australia and New Zealand")
 dat <- gapminder %>%
   filter(year%in% c(2010, 2015) & region %in% west &
            !is.na(life_expectancy) & population > 10^7)
 dat %>%
   mutate(loca = ifelse(year == 2010, 1, 2),
          loca = ifelse(year == 2015 &
                              country %in% c("United Kingdom","Portugal"),
                            loca + 0.22, loca),  hjust = ifelse(year == 2010, 1, 0) ) %>%
   mutate(year = as.factor(year)) %>%
   ggplot(aes(year, life_expectancy, group = country)) + geom_line(aes(color = country), show.legend = FALSE) + geom_text(aes(x = loca, label = country,hjust = hjust),
                                                                                                                          show.legend = FALSE) + xlab("") +
   ylab("Life Expectancy")
 
 # slope chart plot for several position
 
 dat1 <- gapminder %>%
   filter(year %in% c(2005,2010, 2015) & region %in% west &
            !is.na(life_expectancy) & population > 10^7)
 dat1 %>% mutate(location = ifelse(year == 2005,1,3)  ,
                 location = ifelse(year == 2015 & country %in% c("United Kingdom","Portugal"),location + 0.22,location)) %>%  mutate(year = as.factor(year)) %>% 
   ggplot(aes(year,life_expectancy,group = country)) + geom_line(aes(col = country),show.legend = FALSE) + geom_text(aes(x = location ,label = country))
   
# MA plot or turkeey mean-difference

 annee = c(2009,2020)
 life_ex <- c(0.12,0.23)

data("outlier_example")
str(outlier_example) 
str(gapminder) 

IQR(outlier_example)


data("heights") 

  heights %>% ggplot(aes(sex,height,fill = sex)) + geom_boxplot()

  
  library(HistData)
  data("GaltonFamilies")
  
  x <- Galton$child  

  mean_md_mad <- c(mean(x),median(x),mad(x))  
  mean_md_mad[4] <- sd(x)
  names(mean_md_mad) <- c("mean","median","mad","standard_deviation")
x_with_error <- x[1]*10

mean_md_mad_error <- c(mean(x_with_error),median(x_with_error),mad(x_with_error),sd(x_with_error))  
mean_md_mad
 mad(x_with_error)

 data("reported_heights") 
str(reported_heights) 
head(reported_heights)


gapminder %>% filter(region %in% "Northern Africa" & year %in% c(2010,2015) & !is.na(life_expectancy)) %>% 
  mutate(location = ifelse(year == 2010,1,2), 
         hjust = ifelse(year == 2010 ,1,0),
         location = ifelse(year == 2010 & country == "Libya",location - 0.1,location )) %>%
  mutate(year = as.factor(year)) %>% 
  ggplot(aes(year,life_expectancy,group = country)) + geom_line(aes(col = country ) ,show.legend = FALSE) +
  geom_text(aes(x = location,label = country,hjust = hjust))


  install.packages("Lahman")
  library(dslabs)
  install.packages("nycflights13")
  
  library(tidyverse)
  ggplot(data = mpg)  
  
  
  
  
  
  
  