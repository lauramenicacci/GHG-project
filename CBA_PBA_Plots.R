################ GRAPHS FOR PBA CBA + US AND CHINA TRENDS

library(ggplot2)
library(dplyr)
library(hrbrthemes)

df = owid_co2_data
df$co2[df$country == 'World']


#annual production plot

co2_prdo_annual = df$co2[df$country == 'World']

date = df$year[df$country == 'World']
df_2 = data.frame(cbind(date,co2_prdo_annual))

year = df_2$date[df_2$date >= 1990 & df_2$date <= 2018]

co2_production = df_2$co2_prdo_annual[df_2$date >= 1990 & df_2$date <= 2018]

df_def = data.frame(cbind(year,co2_production))

df_def %>%
  tail(29) %>%
  ggplot( aes(x=year, y=co2_production)) +
  geom_line( color="lightblue") +
  geom_point(shape=21, color="black", fill="lightblue", size=2)+
  xlab('Year')+
  ylab('Million tonnes of  CO2 production')+
  ggtitle("Overall trend of production-based CO2 emissions") + theme_minimal()


#annual consumption plot

co2_cons = df$consumption_co2[df$country == 'World']

df_cons = data.frame(cbind(date,co2_cons))


co2_consumption = df_cons$co2_cons[df_cons$date >= 1990 & df_cons$date <= 2018]

df_def_cons = data.frame(cbind(year,co2_consumption))


df_def_cons %>%
  tail(29) %>%
  ggplot( aes(x=year, y=co2_consumption)) +
  geom_line( color="pink") +
  geom_point(shape=21, color="black", fill="pink", size=2) +
  xlab('Year')+
  ylab('Million tonnes of  CO2 consumption')+
  ggtitle("Overall trend of consumption-based CO2 emissions") + theme_minimal()



#US
co2_prdo_annual_US = df$co2[df$country == 'United States']

date = df$year[df$country == 'United States']
df_2_US = data.frame(cbind(date,co2_prdo_annual_US))

year_US = df_2_US$date[df_2_US$date >= 1990 & df_2_US$date <= 2018]

co2_production_US = df_2_US$co2_prdo_annual_US[df_2_US$date >= 1990 & df_2_US$date <= 2018]



co2_cons_US = df$consumption_co2[df$country == 'United States']

df_cons_US = data.frame(cbind(date,co2_cons_US))


co2_consumption_US = df_cons_US$co2_cons_US[df_cons_US$date >= 1990 & df_cons_US$date <= 2018]

df_def_cons_US = data.frame(cbind(year,co2_consumption_US))


# Make a basic graph
plot(co2_consumption_US ~ year, type="b" , bty="l" , xlab="Year" , ylab="CO2 emissions (Million Tonnes)" , col='lightblue' , lwd=2 , pch=17, main = 'CO2 emissions in the US per year')
lines(co2_production_US ~ year, col='pink' , lwd=2 , pch=19 , type="b" )

# Add a legend
legend("topleft", 
       legend = c("Overall trend of consumption-based CO2 emissions in the US", "Overall trend of production-based CO2 emissions in the US"), 
       col = c('lightblue', 
               'pink'), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.01))



#consumption/production

cons_prod_US = co2_consumption_US/co2_production_US

cons_prod_US_df = data.frame(cbind(year,cons_prod_US))
cons_prod_US_df %>%
  tail(29) %>%
  ggplot( aes(x=year, y=cons_prod_US)) +
  geom_line( color="orchid") +
  geom_point(shape=21, color="black", fill="orchid", size=2) +
  xlab('Year')+
  ylab('Relative change in CO2 emissions (Mt)')+
  ggtitle("CO2 emission changes according to CBA/PBA ratio in the US") + theme_minimal()

#China
co2_prdo_annual_China = df$co2[df$country == 'China']

date = df$year[df$country == 'China']
df_2_China = data.frame(cbind(date,co2_prdo_annual_China))

year_China = df_2_China$date[df_2_China$date >= 1990 & df_2_China$date <= 2018]

co2_production_China = df_2_China$co2_prdo_annual_China[df_2_China$date >= 1990 & df_2_China$date <= 2018]



co2_cons_China = df$consumption_co2[df$country == 'China']

df_cons_China = data.frame(cbind(date,co2_cons_China))


co2_consumption_China = df_cons_China$co2_cons_China[df_cons_China$date >= 1990 & df_cons_China$date <= 2018]

df_def_cons_China = data.frame(cbind(year,co2_consumption_China))




# Make a basic graph
plot(co2_consumption_China ~ year, type="b" , bty="l" , xlab="Year" , ylab="CO2 emissions (Million Tonnes)" , col='lightblue' , lwd=2 , pch=17,ylim = c(2200,10500), main = 'CO2 emissions in China per year')
lines(co2_production_China ~ year, col='pink' , lwd=2 , pch=19 , type="b" )

# Add a legend
legend("topleft", 
       legend = c("Overall trend of consumption-based CO2 emissions in China", "Overall trend of production-based CO2 emissions in China"), 
       col = c('lightblue', 
               'pink'), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.01))


#consumption/production

cons_prod_China = co2_consumption_China/co2_production_China

cons_prod_China_df = data.frame(cbind(year,cons_prod_China))
cons_prod_China_df %>%
  tail(29) %>%
  ggplot( aes(x=year, y=cons_prod_China)) +
  geom_line( color="orchid") +
  geom_point(shape=21, color="black", fill="orchid", size=2) +
  xlab('Year')+
  ylab('Relative change in CO2 emissions (Mt)')+
  ggtitle("CO2 emission changes according to CBA/PBA ratio in China") + theme_minimal()
