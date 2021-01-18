library(tidyverse)
library(lubridate)
library(ggpubr)
carsale = read.csv("Car_sales.csv") #read data
colSums(is.na(carsale)) #find NAs
carsale <- carsale[rowSums(is.na(carsale)) == 0,] # remove NAs

carsale <- carsale %>%
  mutate(launchTime = mdy(Latest_Launch)) # change chr to mdy format

#question 1: car price
a<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Engine_size)) +
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Engine_size))

b<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Horsepower)) +
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Horsepower))

c<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Wheelbase))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Wheelbase))

d<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Width))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Width))

e<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Length))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Length))

f<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Curb_weight))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Curb_weight))

g<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Fuel_capacity))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Fuel_capacity))

h<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Fuel_efficiency))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Fuel_efficiency))

i<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = Power_perf_factor))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = Power_perf_factor))

j<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Price_in_thousands, x = launchTime))+
  geom_smooth(mapping = aes(y = Price_in_thousands, x = launchTime))

ggarrange(a,b,c,d,e,f,g,h,i,j)

#question 2: resale price
carsale <- carsale %>%
  mutate(resale_percent = X__year_resale_value / Price_in_thousands) # calculate percent value

a<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Engine_size)) +
  geom_smooth(mapping = aes(y = resale_percent, x = Engine_size))

b<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Horsepower)) +
  geom_smooth(mapping = aes(y = resale_percent, x = Horsepower))

c<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Wheelbase))+
  geom_smooth(mapping = aes(y = resale_percent, x = Wheelbase))

d<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Width))+
  geom_smooth(mapping = aes(y = resale_percent, x = Width))

e<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Length))+
  geom_smooth(mapping = aes(y = resale_percent, x = Length))

f<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Curb_weight))+
  geom_smooth(mapping = aes(y = resale_percent, x = Curb_weight))

g<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Fuel_capacity))+
  geom_smooth(mapping = aes(y = resale_percent, x = Fuel_capacity))

h<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Fuel_efficiency))+
  geom_smooth(mapping = aes(y = resale_percent, x = Fuel_efficiency))

i<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = Power_perf_factor))+
  geom_smooth(mapping = aes(y = resale_percent, x = Power_perf_factor))

j<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = resale_percent, x = launchTime))+
  geom_smooth(mapping = aes(y = resale_percent, x = launchTime))

ggarrange(a,b,c,d,e,f,g,h,i,j)

# question 3: car sale
a<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Engine_size)) +
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Engine_size))

b<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Horsepower)) +
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Horsepower))

c<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Wheelbase))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Wheelbase))

d<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Width))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Width))

e<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Length))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Length))

f<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Curb_weight))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Curb_weight))

g<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Fuel_capacity))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Fuel_capacity))

h<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = Fuel_efficiency))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Fuel_efficiency))

i<-ggplot(data = carsale) +
  geom_point(mapping = aes(y= Sales_in_thousands, x = Power_perf_factor))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = Power_perf_factor))

j<-ggplot(data = carsale) +
  geom_point(mapping = aes(y = Sales_in_thousands, x = launchTime))+
  geom_smooth(mapping = aes(y = Sales_in_thousands, x = launchTime))

ggarrange(a,b,c,d,e,f,g,h,i,j)