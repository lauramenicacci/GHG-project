

total <- owid %>%
  filter(country=="World") %>%
  filter(year==2018) %>%
  select(consumption_co2)

with_share <- owid %>%
  filter(year==2018) %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(share=(consumption_co2/36646.14)*100)

top_share<-with_share%>%arrange(desc(gdp_per_capita))%>%slice(1:10)%>%select(country, share)

top_share %>%
  arrange(desc(share))





total_production <- owid %>% filter(year>=1990 & year<=2018)%>%
  filter(country=="World") %>%
  select(co2)

with_share_production <- owid %>%
  filter(year==2018) %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(share=(cumulative_co2/1625014)*100)

top_share_production <- with_share_production %>%
  arrange(desc(gdp_per_capita)) %>%
  slice(1:10)%>%select(country, share)

top_share_production %>%
  arrange(desc(share))

#%>% slice(1:20)
#mutate_all(~replace(., is.na(.), 0))



#cum growing in terms of production in absolute terms excluding top 10 emitters
owid %>% filter(!country %in% c("World", "Africa", "North America", "Europe (excl. EU-28)", "South America", "Asia","Asia (excl. China & India)", "European Union (28)", "Europe","Europe (excl. EU-27)", "European Union (27)", "High-income countries", "Upper-middle-income countries", "Lower-middle-income countries","Low-income countries", "North America (excl. USA)", "Oceania", "Antarctica"))%>% 
  filter(!country %in% c("China", "United States","Russia","India","Japan","Germany", "Iran", "South Korea", "Saudi Arabia", "Indonesia"))%>%
  filter(year %in% c(1990, 2018))%>% select(country, year, cumulative_co2, continent)%>% 
  pivot_wider(names_from = year, values_from = cumulative_co2)%>%mutate(diff=`2018`-`1990`)%>%
  arrange(desc(diff))%>% filter(!is.na(`2018`) & !is.na(`1990`))%>% slice(1:20)


#cum growth in terms of production in Asia without top 10 emitters
owid %>% filter(!country %in% c("World", "Africa", "North America", "Europe (excl. EU-28)", "South America", "Asia","Asia (excl. China & India)", "European Union (28)", "Europe","Europe (excl. EU-27)", "European Union (27)", "High-income countries", "Upper-middle-income countries", "Lower-middle-income countries","Low-income countries", "North America (excl. USA)", "Oceania", "Antarctica"))%>%
  filter(!country %in% c("China", "United States", "Russia","India","Indonesia", "Japan", "South Korea", "Iran", "Saudi Arabia"))%>% 
  filter(continent=="Asia")%>%filter(year %in% c(1990, 2018))%>% select(country, year, cumulative_co2, continent)%>% 
  pivot_wider(names_from = year, values_from = cumulative_co2)%>%mutate(diff=`2018`-`1990`)%>%arrange(desc(diff))%>% 
  filter(!is.na(`2018`) & !is.na(`1990`))


#trying out emissions per capita
df_population<-owid%>%select(population, country, year)%>%filter(year %in% c(1990, 2018))%>%pivot_wider(names_from = year, values_from = population)
df_production_2<-left_join(df_base, df_population, by="country")

df_production_2<-df_production_2%>%
  mutate(growth_rate_pcapita=(`2018.x`- `1990.x`)/(`2018.y`-`1990.y`))%>%
  rename(pop_18=`2018.y`)%>% rename(pop_90=`1990.y`)%>%rename(emiss_18=`2018.x`)%>%rename(emiss_90=`1990.x`)%>% arrange(desc(growth_rate_pcapita))


df_production_2