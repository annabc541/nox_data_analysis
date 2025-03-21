dat_air_masses %>% 
  filter(air_masses != "Local pollution",
         no_ppt > 0) %>% 
  mutate(african = central_africa + sahel + west_africa + sahara) %>% 
  ggplot(aes(europe,no_ppt)) +
  geom_point()


global_shipping_emissions = read.csv("timeSeries_16_21_global.csv") %>% 
  mutate(date = ymd(Date)) %>% 
  select(date,typeMerge,buildPeriod,NOx)

global_shipping_emissions1 = global_shipping_emissions %>% 
  group_by(date) %>%
  summarise(nox = sum(NOx))

global_shipping_emissions %>% 
  group_by(date) %>%
  summarise(nox = sum(NOx)) %>%
  timeAverage("1 year")
  ggplot(aes(date,nox)) +
  geom_col()
