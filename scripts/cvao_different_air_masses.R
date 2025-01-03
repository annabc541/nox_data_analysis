library(tidyverse)
library(zoo)
library(openair)
library(janitor)

Sys.setenv(TZ = "UTC")

#need to save o3 corrected no2 minima offset corrected data and read it in here

# Reading in data ---------------------------------------------------------

nox = read.csv("output/data/cvao_nox_offset_ozone_corr.csv") %>% 
  mutate(date = ymd_hms(date))

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = ymd_hms(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2016-12-31 23:59",
         date < "2024-11-01") %>% 
  clean_names() %>% 
  select(date,sahara:south_atlantic,ws:rh_10m,o3_ppb_v,co_ppb_v,ch4_all_ppb_v:co2_with_mpi_flasks_ppm_v,
         jno2_calc,jo1d_calc,j_o1d,j_no2)

air_masses_classification = met_data %>% 
  select(date,sahara:wd) %>% 
  timeAverage("1 day") %>% 
  mutate(african = central_africa + sahel + west_africa + sahara,
         ocean = upwelling + north_atlantic,
         air_masses = case_when(african >= 10~ "African",
                                europe >= 10 ~ "European aged pollution",
                                north_america >= 10 ~ "North American aged pollution",
                                south_atlantic > 5 | south_america > 5 ~ "Southern hemisphere",
                                ocean >= 95 & african == 0 ~ "Clean North Atlantic",
                                african < 10 | europe < 5 ~ "Coastal African",
                                TRUE ~ "Other")) %>% 
  select(date,air_masses)

dat = left_join(nox,met_data,by = "date") %>% 
  left_join(air_masses_classification,by = "date") %>% 
  fill(air_masses) %>% 
  mutate(local_pollution = case_when(wd > 100 & wd < 340 | ws < 2 ~ 1,
                                     TRUE ~ 0))


# NOx plots ---------------------------------------------------------------

dat %>% 
  filter(no_corr < 50 & no_corr > 0,
         air_masses != "Other") %>% 
  ggplot(aes(no_corr)) +
  geom_histogram(bins = 50) +
  facet_wrap(~air_masses)

# Exploratory plots? ------------------------------------------------------

#checking what the air masses classification look like

air_masses2 %>% 
  filter(local_pollution == 0,
         is.na(north_atlantic) == F,
         year >= 2021) %>%
  # pivot_longer(c(sahara:south_atlantic)) %>% 
  ggplot(aes(air_masses)) +
  geom_histogram(stat = "count") +
  # scale_x_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
  # scale_fill_viridis_d() +
  # facet_wrap(~name) + 
  NULL

air_masses2 %>% 
  pivot_longer(cols = c(african,upwelling:south_atlantic)) %>% 
  mutate(value = ifelse(air_masses == "Other",value,NA_real_),
         value = ifelse(local_pollution == 0,value,NA_real_)) %>% 
  mutate(doy = yday(date)) %>% 
  filter(year == 2024) %>% 
  ggplot(aes(doy,value,col = name)) +
  theme_bw() +
  geom_point(size = 10) +
  # facet_grid(rows = vars(year)) +
  # theme(legend.position = "top",
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       axis.text.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       plot.margin = unit(c(0,0.1,0.1, 0.1), "cm"),
  #       text = element_text(size = 16)
  # ) +
  labs(x = NULL,
       y = "Air mass composition (%)",
       col = NULL,
       fill = NULL) +
  scale_x_continuous(breaks = c(1, 32, 60,91,121,152,182,213,244,274,305,335),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_colour_manual(values = c("north_atlantic" = "navy",
                               "south_atlantic" = "steelblue1",
                               "african" = "goldenrod1",
                               "south_america" = "darkseagreen1",
                               "north_america" = "springgreen4",
                               "europe" = "darkolivegreen3",
                               "upwelling" = "deepskyblue3"))
