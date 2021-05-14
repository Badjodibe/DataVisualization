library(tidyverse)
 library(dslabs)
 data("gapminder")
 years<-c("1962","1970","1990","2012")
 continents<-c("Africa","Europe")
 countries<-c("Germany","South Korea")
 
 #time serie plot and labeling not legend
 
 #data for labeling
  labels<-data.frame(country=countries,x=c(1970,1960),y=c(70,50))
 gapminder%>%filter(country%in%countries&!is.na(gdp))%>%ggplot(aes(year,life_expectancy,col=country))+geom_line()+geom_text(data = labels,aes(x,y,label=country))+theme(legend.position = "none")
 
 #Togo informations
 
 togo<-gapminder%>% filter(country=="Togo")
 togo_summarize<-togo%>%summarise(max_fertility=max(fertility,na.rm = TRUE),max_life_expectancy=max(life_expectancy,na.rm = TRUE))
 togo_summarize 
 togo_year_max_expectation<-togo%>%filter(life_expectancy==61.9)%>%select(year)
togo_year_max_expectation

#transformations
#log transformation convert multiplicative to addition
#we can use it either befor plotting or use scale on axis


gapminder<-gapminder%>%mutate(dollar_per_day=gdp/population/365)
head(gapminder)

gapminder%>%filter(year==1970&!is.na(gdp))%>%ggplot(aes(dollar_per_day))+geom_histogram(binwidth = 2,col="black")+scale_x_continuous(trans = "log2")

#boxplot
#we rotate axis label by changing the theme element_text
#reorder change the order of the factor varial based on numerique value
#

gapminder%>%filter(year==1970&!is.na(gdp))%>%mutate(region=reorder(region,dollar_per_day,FUN = median))%>%ggplot(aes(dollar_per_day,region,fill=continent))+
  geom_boxplot()+theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  scale_x_continuous(trans = "log2")+geom_point(show.legend = FALSE)


#comparing distributions

west<-c("Western Europe","Southern Europe","Northern Europe","Northern America","Australia and New Zealand")
past_year<-1970
present_year<-2010
year_combine<-c(past_year,present_year)
gapminder%>%filter(year%in%year_combine&!is.na(gdp))%>%mutate(group=ifelse(region%in%west,"West","Developping"))%>%ggplot(aes(dollar_per_day,fill=factor(year)))+
  geom_boxplot()+facet_grid(year~group)+scale_x_continuous(trans = "log2")

#boxplot of the income of west versus developing

gapminder%>%filter(year%in%year_combine&!is.na(gdp))%>%mutate(region=reorder(region,dollar_per_day,FUN = median))%>%ggplot(aes(dollar_per_day,region,fill=factor(year)))+
  geom_boxplot()+scale_x_continuous(trans = "log2")+facet_grid()

#time serie plot
 data("GaltonFamilies")
 library(tidyverse )
 library(ggthemes)
 library(ggrepel)
 library(HistData)
 data("Quarrels")
 data("GaltonFamilies")
 library(gridExtra)
 library(dslabs)
 data("murders")
 head(Quarrels)
 
 head(GaltonFamilies)

 child_plot<-GaltonFamilies%>%filter(midparentHeight>=75)%>%ggplot(aes(gender,childHeight))+
   geom_boxplot(aes(group=gender))+geom_jitter(aes(col=gender))
   xlab("gender  ")+ylab("childheight")+ggtitle("gender versus childHeight")+
   theme_economist()   
child_plot 

childNumber_per_family<-GaltonFamilies%>%ggplot(aes(as.integer(family),children))+
  geom_point()

childNumber_per_family

GaltonFamilies%>%summarise(mean_of_children=mean(children),stantard_deviation_children=sd(children),children_max=max(children),min_children=min(children))

length(unique(GaltonFamilies$family))
x<-GaltonFamilies$mother
y<-GaltonFamilies$father
qplot(x,y)

GaltonFamilies%>%ggplot(aes(x=midparentHeight,y=childHeight,fill=gender,group=gender))+GeomBar
ggplot2::ge

GaltonFamilies%>%group_by(gender)%>%summarize(mal_percent=length(gender$male)/length(gender),female_percent=length(female)/length(gender))

length(GaltonFamilies$gender)

#cdf do not convey all the information we need such as the centre of the distributions
#,is the distributions symetrique so we referred histogramme

male_edf<-GaltonFamilies%>%filter(gender=="male")%>%select(childHeight)%>%unlist()%>%ecdf()

curve(male_edf,40,90)

male_edf(70)

hist_mal1<-GaltonFamilies%>%filter(gender=="male")%>%ggplot(aes(childHeight))+geom_histogram(binwidth  = 1)+ggtitle("binwidth 1")
hist_mal2<-GaltonFamilies%>%filter(gender=="male")%>%ggplot(aes(childHeight))+geom_histogram(binwidth  = 0.5)+ggtitle("binwidth 0.5")
hist_mal3<-GaltonFamilies%>%filter(gender=="male")%>%ggplot(aes(childHeight))+geom_histogram(binwidth  = 0.1)+ggtitle("binwidth 0.1")
 
grid.arrange(hist_mal3,hist_mal1,hist_mal2,ncol=3)
 
 #smooth density is obtain by curving of the top of the histogram with very small bin
 #it is relative
 
murders_hist<-murders%>%ggplot(aes(region,population))+geom_histogram(binwidth = 2)+scale_y_continuous(trans = "log10")
murders_hist 

index<-heights$sex=="Male"
mal_height<-heights$height[index]

#sd divide by lenght(x)-1


average_male<-sum(mal_height)/length(mal_height)
standard_deviaton_mal<-sqrt(sum((average_male-mal_height)^2)/length(mal_height))
standard_deviaton_mal
sd(mal_height)
 
geom_curve(aes(dnorm(x,mean = average_male,sd=standard_deviaton_mal)),50,80)

#standard unit,we compute it in R by the scale function
#quantille-quantille plot let us know how well a distribution is approximaly mormal
#to obtain a quantille test,we firt define the proportion p
#a vector of quantille  corresponding to each proportion
#define the theorical quantille
#and finally we plot theoricale quantille versus sample quantille

x<-rnorm(1000)
proportion<-seq(0,0.95,0.05)
sample_quantille<-quantile(x,proportion)
theorical_quantille<-qnorm(proportion,mean = mean(x),sd=sd(x))
 plot(theorical_quantille,sample_quantille,pch="*")+geom_abline()

 #we can do this plot by standard unit and it is easier
 
 z<-scale(x)
 su_sample_quantille<-quantile(z,proportion)
su_theorical_quantille<-qnorm(proportion)

plot(su_theorical_quantille,su_sample_quantille,pch="*")+geom_abline()

#in practice we use ggplot2

heights%>%filter(sex=="Male")%>%ggplot(aes(sample=scale(height)))+geom_qq()+geom_abline()

#percentile and quartile are specifi case of quantile

murders%>%mutate(rate= total/population*100000)%>%ggplot(aes(sample=rate))+geom_qq()+geom_qq_line()

head(murders)

#stractification is when we divide the observation into a groupe


GaltonFamilies%>%ggplot(aes(gender,childHeight,fill=gender))+geom_violin()+
  scale_x_discrete(name="Gender")

head(heights)
male<-heights$height[heights$sex=="Male"]
female<-heights$height[heights$sex=="Female"]
 head(female)
 
measure<-c(mal_count=length(male),female_count=length(female))
measure

p<-seq(0.01,0.09,0.02)

male_percentile<-quantile(male,p)
female_percentile<-quantile(female,p)

male_percentile
female_percentile

per_data<-data.frame(male_percentile,female_percentile)

per_data

x<-male
tail(x)

pro_sample<-mean(x<=72)-mean(x<=69)

sd_mal<-sd(x)
average_male<-mean(x)

pro_theorical<-pnorm(72,mean = average_male,sd=sd_mal)-pnorm(69,mean = average_male,sd_mal)

pro_sample/pro_theorical

#geometry bar

murders%>%ggplot(aes(region,fill=region))+geom_bar()

tab<-murders%>%count(region)%>%mutate(proportion=n/sum(n))

#to count we use count function with n is the number of count

tab%>%ggplot(aes(region,proportion,fill=region))+geom_bar(stat = "identity")

library(tidyverse)
heights %>% filter(sex == "Female") %>% ggplot(aes(height)) +
  geom_density(col = "black",fill = "blue",bw=2) + ggtitle("Histogram of Female height")

#bw or adjust


heights %>% group_by(sex)  %>% ggplot(aes(height)) + geom_histogram(fill = "blue",col = "black",binwidth = 1) + facet_grid(.~sex)

gapminder %>% as_tibble()

gapminder %>% filter(country %in% c("Sri Lanka","Poland","Malaysia","Pakistan","Thailand","Turkey","South Korea","South Africa","Russia") 
) %>% select(country,infant_mortality)
   
load(file = "~/RStudio-files/data_visualisation.RData")

gapminder %>% as_tibble()

gapminder %>% filter(year %in% c(1962,1970,1980,1990,2000,2015) & continent %in% c("Europe","Asia")) %>% ggplot(aes(fertility,life_expectancy,col = continent)) + geom_jitter() + facet_wrap(~year)

gapminder %>% filter(continent == "Africa" & life_expectancy < 50 & year == 2012) %>%  select(country,life_expectancy)


#times series plot,is a plot where time is in the x axis and on the y axis the other interest

gapminder %>% filter(country == "United States" & !is.na(fertility)) %>% ggplot(aes(year,fertility)) +
  geom_line(col = "blue")
countries <- c("Germany","South Korea")

gapminder %>% filter(country %in% countries & !is.na(fertility)) %>% ggplot(aes(year,fertility,col = country)) +
  geom_line() + geom_text(data = labels,aes(x,y,label = country)) + theme(legend.position = "none")

# to label a plot we determine the position of the label and 

labels <- data.frame(country = countries,x = c(1963,1964),y = c(2.7,6.3))

#transformations

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% ggplot(aes(dollars_per_day)) + geom_histogram(binwidth = 1,col = "black")

length(gapminder$dollars_per_day < 10)

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% mutate(region = reorder(region,dollars_per_day,FUN = median)) %>% ggplot(aes(region,dollars_per_day)) +
  geom_point() + theme(axis.text.x = element_text(hjust = 1,angle = 90),axis.title.x = element_text(color = "blue")) + scale_y_continuous(trans = "log2")

gapminder <- gapminder %>% mutate(group = case_when(
  region %in% c("Northen Europe","Northen America","Southern Europe","Australia and New Zealand","Western Europe") ~ "West",
  region %in% c("Eastern Asia","South-Eastenr Asia") ~ "East Asia",
  region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
  continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa",
  region == "Northern Africa" ~ "Northern Africa",
  TRUE ~ "Other"
))
head(gapminder)

gapminder <- gapminder %>% mutate(group = factor(group, levels = c("East Asia","West","Latin America","Sub-Saharan Africa","Other","Northern Africa")))

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% mutate(group = reorder(group,dollars_per_day,FUN = median)) %>% ggplot(aes(group,dollars_per_day,fill = group)) + 
  geom_boxplot() + scale_y_continuous(trans = "log2") + geom_point(alpha = 0.5)

install.packages("ggridges")
library(ggridges)

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% mutate(group = reorder(group,dollars_per_day,FUN = median)) %>%
  ggplot(aes(dollars_per_day,group,fill = group)) + geom_density_ridges(jittered_points = TRUE,position = position_points_jitter(width = 0.05,height = 0), point_shape = '|',point_alpha = 1,alpha = 0.7,point_size = 3) + scale_x_continuous(trans = "log2")

present_year <- 2010

year_combine <- c(past_year,present_year)

gapminder %>% filter(year %in% c(1972,2010)  & !is.na(gdp)) %>%
  mutate(West = ifelse(group == "West","West","Deevelpping")) %>% ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, col = "black", fill = "yellow") + facet_grid(West~year) + scale_x_continuous(trans = "log2")

#make two different group , remake this plot with data available

country1 <- gapminder %>% filter(year == past_year & !is.na(dollars_per_day)) %>% pull(country) 
country2 <- gapminder %>% filter(year == present_year & !is.na(dollars_per_day)) %>% pull(country)

country_list <- intersect(country2,country1)

gapminder %>% filter( country %in% country_list & year %in% c(1972,2010)  & !is.na(gdp)) %>% mutate(West = ifelse(group == "West","West","Developping")) %>%
  ggplot(aes(group,dollars_per_day,fill = factor(year))) + geom_boxplot()  +
  theme(axis.text.x = element_text(angle = 90,hjust = 1))   + scale_y_continuous(trans = "log2")

# if we fill we a numeric number convert into factor ggplot will color next

#now we want to see the gap between rich and poor country


gapminder %>% filter(country %in% country_list & !is.na(gdp) & year %in% c(1972,2010)) %>% 
  mutate(West = ifelse(group == "West","West","Developping")) %>% ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") + scale_x_continuous(trans = "log2") + facet_grid(year~West)

index <- gapminder$country[gapminder$group == "West"]
 length(unique(index))
length(unique(gapminder$country[gapminder$group == "Developping"])) - length(unique(index))

#accessing computing  variable with geom_density,to access them we us "..variable.."

gapminder %>% filter(country %in% country_list & !is.na(gdp) & year %in% c(1970,2010)) %>% 
  mutate(group = ifelse(group == "West","West","Developping")) %>% ggplot(aes(dollars_per_day,y = ..count.. ,fill = group)) +
  geom_density(alpha = 0.2,bw = 0.75) + scale_x_continuous(trans = "log2",limits = c(0.125,300)) + facet_grid(year~.)

# density rigid plot

library(ggridges)

gapminder %>% filter(country %in% country_list & year %in% c(1970,2010) & !is.na(dollars_per_day)) %>%
 ggplot(aes(x = dollars_per_day,y = group,fill = group )) + geom_density_ridges(adjust = 0.5) + facet_grid(.~year) +
  scale_x_continuous(trans = "log2", limits = c(0.125,300))

# stack plot


gapminder %>% filter(year %in% c(1970,2010) & country %in% country_list & !is.na(dollars_per_day)) %>%
  group_by(year) %>% mutate(weight = population/sum(population*2)) %>% ungroup() %>%
  ggplot(aes(dollars_per_day,fill = group)) + scale_x_continuous(trans = "log2") +
  geom_density(bw = 0.75,alpha = 0.2,position = "stack") + facet_grid(year~.)

gapminder <- gapminder %>% mutate(group = case_when(
  .$group == "West" ~ "West",
  .$continent == "Africa" & .$region !="Northern Africa"  ~ "Sub-Saharan Africa",
  .$group == "Northern Africa"~ "Northern Africa",
  .$group == "East Asia" ~ "East Asia",
  .$region == "Southern Asia" ~ "Southern Asia",
  .$group == "Latin America" ~ "Latin Ameica",
  .$region %in% c("Polynesia","Malenesia","Micronesia") ~ "Pacific Islands"))
head(gapminder)
survival_income <- gapminder %>% filter(year %in% c(1970,2010) & !is.na(gdp) & !is.na(group ) & !is.na(infant_mortality)) %>%
  group_by(group) %>% summarise(income = sum(gdp)/sum(population)/365,infant_survival = 1 - sum(infant_mortality/1000*population)/sum(population))
head(gapminder)

survival_income %>% arrange(income)


survival_income




gapminder <- gapminder %>% mutate(group = case_when(
  region %in% c("Western Europe", "Northern Europe","Southern Europe", "Northern America", "Australia and New Zealand") ~ "West",
  region %in% "Northern Africa" ~ "Northern Africa",
  region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
  region == "Southern Asia"~ "Southern Asia",
  region %in% c("Central America", "South America", "Caribbean") ~ "Latin America", continent == "Africa" & region != "Northern Africa" ~ "Sub-Saharan Africa", region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))



surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) &
           !is.na(infant_mortality) & !is.na(group)) %>% group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,infant_survival_rate =
              1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)


surv_income %>% ggplot(aes(income,infant_survival_rate,col = group,label = group)) +
  scale_x_continuous(trans = "log2",limits = c(0.25,150)) +
  scale_y_continuous(trans = "logit",limits = c(0.875, .9981), breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 5, show.legend = FALSE) 

library(tidyverse)
library(dslabs)
data("gapminder")

gapminder %>% filter(country %in% c("Togo","Benin","Ghana","Cote d'Ivoire") & !is.na(gdp)) %>% mutate(dollard_per_day = gdp/population*100000*365)  %>% ggplot(aes(year,gdp,col = country)) +
  geom_line() + scale_y_continuous(trans = "log2")
head(gapminder$gdp)

library(HistData)
attach(Galton)

sunflowerplot(parent~child)

qplot(log(parent),log(child),geom = c("point","smooth"))
