

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
