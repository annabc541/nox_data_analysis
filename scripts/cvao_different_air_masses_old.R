library(tidyverse)
library(zoo)
library(openair)
library(janitor)
library(ggh4x)
library(plotly)
library(ggpmisc)

Sys.setenv(TZ = "UTC")
setwd("~/Cape Verde/nox/nox_data_analysis")

#need to save o3 corrected no2 minima offset corrected data and read it in here
#only used no2 minima for 2023 and 2024 data, data used currently has offset correction in r (nox23-24_ozone_corr_flagging_uncertainties)

# Functions ---------------------------------------------------------------

ppt_to_molecules_cm3 <- function(x){y = x * 2.46 * 10^19 * 10^-12}
molecules_cm3_to_ppt <- function(x){y = x / ( 2.46 * 10^19 * 10^-12)}
tidy_rle = function(rleObj){
  
  require(dplyr)
  
  rleDf = tibble(lengths = rleObj$lengths,
                 values = rleObj$values) %>% 
    mutate(idxStart = cumsum(c(1,lengths))[1:(nrow(.))],
           idxEnd = cumsum(lengths))
  
  #
  rleDf
}

# Reading in data ---------------------------------------------------------

setwd("~/Cape Verde/nox/data_submission/downloaded_data/simone")

files = list.files(full.names = TRUE,pattern = "simone_nox")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = 73,header = TRUE)%>%
    mutate(date = as.POSIXct(round(start_time / 0.041667) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

nox17_21 = bind_rows(datList) %>% 
  arrange(date) %>% 
  mutate(across(c(nitrogen_monoxide,nitrogen_monoxide_LOD,nitrogen_dioxide,nitrogen_dioxide_LOD,
                  nitrogen_monoxide_uncertainty,nitrogen_dioxide_uncertainty), ~ .x * 1000)) %>% 
  select(date,no_ppt = nitrogen_monoxide,no_lod_ppt = nitrogen_monoxide_LOD,
         no_u_ppt = nitrogen_monoxide_uncertainty,no_flag = nitrogen_monoxide_numflag,
         no2_ppt = nitrogen_dioxide,no2_lod_ppt = nitrogen_dioxide_LOD,
         no2_u_ppt = nitrogen_dioxide_uncertainty,no2_flag = nitrogen_dioxide_numflag)

nox22 = read.csv("~/Cape Verde/nox/processing/ozone_correction/processed_data/nox2022.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  select(-o3)

setwd("~/Cape Verde/nox/nox_data_analysis")

nox23 = read.csv("output/data/nox2023_with_uncertainties.csv") %>% 
  mutate(date = ymd_hms(date))

nox24 = read.csv("output/data/nox2024_with_uncertainties.csv") %>% 
  mutate(date = ymd_hms(date))

nox = bind_rows(nox17_21,nox22,nox23,nox24) %>% 
  arrange(date) %>% 
  mutate(across(c(no_ppt:no_u_ppt), ~ ifelse(no_flag == 0.999,NA_real_,.x)),
         across(c(no2_ppt:no2_u_ppt), ~ ifelse(no2_flag == 0.999,NA_real_,.x)))

remove(nox17_21,nox22,nox23,nox24,datList)

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date >= "2017-01-01",
         date < "2025-01-01") %>% 
  clean_names() %>% 
  select(date,ws:rh_10m,o3_ppb = o3_ppb_v,co_ppb = co_ppb_v,ch4_all_ppb_v:co2_with_mpi_flasks_ppm_v,
         jno2_calc,jo1d_calc,j_o1d,j_no2)

air_masses = read.csv("~/Cape Verde/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date >= "2017-01-01",
         date < "2025-01-01") %>% 
    clean_names()
  # rename_with(~ gsub("\\.", " ", .))

df_list = list(nox,met_data,air_masses)
dat = df_list %>% reduce(full_join,by = "date") %>% 
  mutate(jno2_calc = case_when(date >= "2023-05-02 12:00" & date <= "2023-06-01 17:00" ~ NA_real_,
                               date >= "2023-02-01 09:00" & date <= "2023-08-31 17:00" ~ NA_real_,
                               TRUE ~ jno2_calc),
         jno2 = case_when(date < "2020-11-23" ~  jno2_calc,
                          # date >= "2023-10-02 18:00" & date <= "2023-10-11" ~ NA_real_,
                          date > "2020-11-23" & is.na(j_no2) == T ~ jno2_calc,
                          TRUE ~ j_no2),
         jno2 = case_when(date >= "2023-10-02 18:00" & date <= "2023-10-11" ~ NA_real_,
                          date >= "2024-01-17" & date <="2024-01-24 19:00" ~ NA_real_,
                          date >= "2024-01-26 19:00" & date <="2024-01-27 16:00" ~ NA_real_,
                          jno2 <= 0 ~ NA_real_,
                          TRUE ~ jno2),
         across(-c(date), ~ifelse(is.na(ws) == TRUE | is.na(wd) == TRUE,NA_real_,.)))

# Timeseries --------------------------------------------------------------

dat %>% 
  mutate(year = year(date),
         doy = yday(date),
         month = month(date),
         no_ppt = ifelse(no_flag <= 0.147, no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.14, no2_ppt,NA_real_)) %>% 
  group_by(date = floor_date(date,"1 month")) %>%
  summarise(across(c(no_ppt,no2_ppt),list(mean = ~mean(.,na.rm = T),
                                                 se = ~sd(., na.rm = TRUE) / sqrt(length(.)),
                                                 count = ~sum(!is.na(.))))) %>%
  mutate(no_ppt_mean = ifelse(no_ppt_count < 240,NA_real_,no_ppt_mean),
         no2_ppt_mean = ifelse(no2_ppt_count < 240,NA_real_,no2_ppt_mean)) %>%
  # pivot_longer(c(no_ppt_mean,no2_ppt_mean)) %>%
  # filter(year == 2024) %>%
  ggplot(aes(date,no_ppt_mean)) +
  geom_point()
  facet_grid(rows = vars(name),scales = "free") +
  # scale_x_datetime(breaks = "1 year",date_labels = "%Y") +
  # ylim(-5,50) +
  # geom_vline(aes(xintercept = as.POSIXct("2019-07-25 00:00")),col = "red",size = 0.9) +
  # facet_grid(rows = vars(year),scales = "free") +
  # scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
  #                    labels = month.abb) +
  NULL

# Trends? -----------------------------------------------------------------

trend_analysis = dat %>% 
  mutate(year = year(date),
         doy = yday(date),
         month = month(date),
         local_pollution_flag = ifelse(ws <= 2 | wd >= 100 & wd <= 340 | ws <= 2,"Local pollution","Baseline"),
         O3 = ifelse(local_pollution_flag == "Local pollution" | is.na(ws) == T | is.na(wd) == T,
                     NA_real_,o3_ppb),
         CO = ifelse(local_pollution_flag == "Local pollution" | is.na(ws) == T | is.na(wd) == T,
                     NA_real_,co_ppb),
         NO = ifelse(no_flag <= 0.147 & is.na(ws) == F & is.na(wd) == F, no_ppt,NA_real_),
         NO2 = ifelse(no2_flag <= 0.147 & is.na(ws) == F & is.na(wd) == F, no2_ppt,NA_real_),
         polluted_air_masses = sahara + sahel + west_africa + central_africa + europe + north_america)

trend_plot = smoothTrend(trend_analysis,
                        ylab = "Seasonal mean CO (ppb)",
                        xlab = NULL,
                        # data.thresh = 5,
                        # type = "season",
                        # deseason = T,
                        # col = "darkolivegreen3",
                        # avg.time = "season",
                        pollutant = c("CO"))

png(filename = "output/plots/trends/sahara_trend_deseasonalised.png",
    width = 12.5, height = 12, units = "cm", res = 300)
print(trend_plot)
dev.off()

# NOx seasonal diurnals -----------------------------------------------------

dat %>% 
  # filter(no2_flag <= 0.147) %>% 
  mutate(hour = hour(date),
         year = year(date),
         month = month(date),
         no_ppt = ifelse(no_flag <= 0.147, no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147, no2_ppt,NA_real_),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  group_by(hour,year,season) %>% 
  summarise(across(c(no_ppt,no2_ppt,o3_ppb),list(mean = ~mean(.,na.rm = T),
                                                 se = ~sd(., na.rm = TRUE) / sqrt(length(.)),
                                                 count = ~sum(!is.na(.))))) %>%
  mutate(no_ppt_mean = ifelse(no_ppt_count < 45,NA_real_,no_ppt_mean),
         no2_ppt_mean = ifelse(no2_ppt_count < 45,NA_real_,no2_ppt_mean),
         no_err_plot_max = no_ppt_mean + no_ppt_se,
         no_err_plot_min = no_ppt_mean - no_ppt_se,
         no2_err_plot_max = no2_ppt_mean + no2_ppt_se,
         no2_err_plot_min = no2_ppt_mean - no2_ppt_se,
         o3_err_plot_max = o3_ppb_mean + o3_ppb_se,
         o3_err_plot_min = o3_ppb_mean - o3_ppb_se,
         year = as.character(year)) %>% 
  # rename(NO = no_ppt_mean,'NO[2]' = no2_ppt_mean) %>%
  # pivot_longer(c(NO,`NO[2]`)) %>%
  # pivot_longer(cols = c(no_err_plot_max,no2_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>%
  # pivot_longer(cols = c(no_err_plot_min,no2_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>%
  # mutate(flag = case_when(name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
  #                         name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>%
  # filter(is.na(flag) == F) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,no2_ppt_mean,col = year),size = 0.75) +
  geom_ribbon(aes(hour,ymin = no2_err_plot_min,ymax = no2_err_plot_max,fill = year),alpha = 0.25) +
  # facet_nested_wrap(~factor(season,levels = c("Spring (MAM)","Summer (JJA)","Autumn (SON)","Winter (DJF)")) + name,labeller = label_parsed,nrow = 2,scales = "free") +
  facet_wrap(~factor(season,levels = c("Spring (MAM)","Summer (JJA)","Autumn (SON)","Winter (DJF)")),
             ) +
  scale_colour_manual(values = c("steelblue1","navy","springgreen4","darkolivegreen3","khaki4","goldenrod1","darkorange","darkred")) +
  scale_fill_manual(values = c("steelblue1","navy","springgreen4","darkolivegreen3","khaki4","goldenrod1","darkorange","darkred")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~mixing~ratio~(ppt)),
       # y = "NO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)

ggsave("no2_seasonal_diurnal.png",
       path = "output/plots",
       height = 15,
       width = 27,
       units = "cm")

# Monthly averages -------------------------------------------------------

dat %>% 
  mutate(hour = hour(date),
         year = year(date),
         month = month(date),
         doy = yday(date),
         no_ppt = ifelse(no_flag <= 0.147, no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147, no2_ppt,NA_real_)) %>% 
  group_by(month,year) %>% 
  summarise(across(c(no_ppt,no2_ppt,o3_ppb),list(mean = ~mean(.,na.rm = T),
                                                 se = ~sd(., na.rm = TRUE) / sqrt(length(.)),
                                                 count = ~sum(!is.na(.))))) %>% 
  mutate(no_ppt_mean = ifelse(no_ppt_count < 240,NA_real_,no_ppt_mean),
         no2_ppt_mean = ifelse(no2_ppt_count < 240,NA_real_,no2_ppt_mean),
         no_err_plot_max = no_ppt_mean + no_ppt_se,
         no_err_plot_min = no_ppt_mean - no_ppt_se,
         no2_err_plot_max = no2_ppt_mean + no2_ppt_se,
         no2_err_plot_min = no2_ppt_mean - no2_ppt_se,
         o3_err_plot_max = o3_ppb_mean + o3_ppb_se,
         o3_err_plot_min = o3_ppb_mean - o3_ppb_se,
         year = as.character(year)) %>% 
  # pivot_longer(c(NO,`NO[2]`)) %>%
  # pivot_longer(cols = c(no_err_plot_max,no2_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>%
  # pivot_longer(cols = c(no_err_plot_min,no2_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>%
  # mutate(flag = case_when(name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
  #                         name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>%
  # filter(is.na(flag) == F) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(month,no_ppt_mean,col = year),size = 0.75) +
  geom_ribbon(aes(month,ymin = no_err_plot_max,ymax = no_err_plot_min,fill = year),alpha = 0.25) +
  # scale_colour_manual(values = c("darkorange","steelblue1","navy","darkred")) +
  # scale_fill_manual(values = c("darkorange","steelblue1","navy","darkred")) +
  scale_colour_manual(values = c("steelblue1","navy","springgreen4","darkolivegreen3","khaki4","goldenrod1","darkorange","darkred")) +
  scale_fill_manual(values = c("steelblue1","navy","springgreen4","darkolivegreen3","khaki4","goldenrod1","darkorange","darkred")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = seq_along(month.abb), 
                     labels = month.abb) +
  labs(x = NULL,
       # y = expression(NO[2]~mixing~ratio~(ppt)),
       y = "NO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)

ggsave("no_monthly_averages.png",
       path = "output/plots",
       height = 15,
       width = 30,
       units = "cm")

# Daily ozone change ------------------------------------------------------

delta_ozone = dat %>% 
  select(date,o3_ppb,no_ppt,no_flag) %>% 
  mutate(date_only = as.Date(date),
         month = month(date),
         year = year(date),
         hour = hour(date),
         daytime_no = ifelse(hour >= 11 & hour <= 15 & no_flag <= 0.147,no_ppt,NA_real_),
         ozone_nine = ifelse(hour == 9 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_five = ifelse(hour == 17 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_ten = ifelse(hour == 22 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_three= ifelse(hour == 3 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_)) %>% 
  group_by(date_only) %>% 
  summarise(across(c(daytime_no),list(mean = ~mean(.,na.rm = T),count = ~sum(!is.na(.)))),
            across(c(ozone_nine:ozone_three),~mean(.,na.rm = T))) %>% 
  mutate(
    date = as.POSIXct.Date(date_only),
         entrainment_deposition = (ozone_three - ozone_ten),
         delta_ozone = (ozone_five - ozone_nine),
         delta_ozone_chem = delta_ozone - entrainment_deposition,
         daytime_no = ifelse(daytime_no_count != 5,NA_real_,daytime_no_mean))

delta_ozone %>% 
  mutate(year = as.character(year(date)),
         month = month(date),
         doy = yday(date)) %>% 
  timeAverage("1 month") %>%
  pivot_longer(c(delta_ozone,entrainment_deposition,delta_ozone_chem)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free") +
  geom_path(linewidth = 0.75) +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  theme(legend.position = "none",
        text = element_text(size =20)) +
  scale_x_datetime(breaks = "1 year",date_labels = "%Y") +
  # scale_x_continuous(breaks = c(1:12),
  #                    labels = month.abb) +
  # scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
  #                    labels = month.abb) +
  labs(x = NULL,
       y = NULL,
       col = NULL)

delta_ozone %>% 
  mutate(hour = hour(date),
         year = year(date),
         month = month(date),
         doy = yday(date)) %>% 
  group_by(month,year) %>% 
  summarise(across(c(delta_ozone),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>%  
  mutate(do_err_plot_max = delta_ozone_mean + delta_ozone_se,
         do_err_plot_min = delta_ozone_mean - delta_ozone_se,
         year = as.character(year)) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(month,delta_ozone_mean,col = year),linewidth = 0.75) +
  geom_ribbon(aes(month,ymin = do_err_plot_max,ymax = do_err_plot_min,fill = year),alpha = 0.25) +
  # scale_colour_manual(values = c("darkorange","steelblue1","navy","darkred")) +
  # scale_fill_manual(values = c("darkorange","steelblue1","navy","darkred")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = seq_along(month.abb), 
                     labels = month.abb) +
  labs(x = NULL,
       y = expression(Delta~O[3]~(ppb)),
       col = NULL,
       fill = NULL)

# ggsave("delta_o3_timeseries.png",
#        path = "output/plots",
#        height = 15,
#        width = 30,
#        units = "cm")
  

# Plotting air masses -----------------------------------------------------

air_masses2 = read.csv("~/Cape Verde/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename_with(~ gsub("\\.", " ", .))

air_masses2 %>%
  timeAverage("1 day") %>%
  mutate(year = year(date),
         doy = yday(date),
         month = month(date)) %>% 
  filter(year >= 2021,
         month == 7) %>% 
  pivot_longer(cols = -c(date,year,doy,month)) %>% 
  ggplot(aes(doy,value,fill = name)) +
  theme_bw() +
  geom_area() +
  facet_grid(rows = vars(year)) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0.1,0.1, 0.1), "cm"),
        text = element_text(size = 16)
  ) +
  labs(x = NULL,
       y = "Air mass composition (%)",
       col = NULL,
       fill = NULL) +
  # scale_x_continuous(breaks = c(1, 32, 60,91,121,152,182,213,244,274,305,335),
                     # labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                     # expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_fill_manual(values = c("North Atlantic" = "navy",
                               "South Atlantic" = "steelblue1",
                               "Sahara" = "goldenrod1",
                               "Sahel" = "darkorange3",
                               "West Africa" = "firebrick4",
                               "Central Africa" = "khaki4",
                               "South America" = "darkseagreen1",
                               "North America" = "springgreen4",
                               "Europe" = "darkolivegreen3",
                               "Upwelling" = "deepskyblue3"))

# Air mass classification ---------------------------------------------------

#classified based on 6-hour back trajectories (filled data)
dat_air_masses = dat %>% 
  fill(upwelling:south_atlantic) %>% 
  mutate(missing_dates = case_when(date >= "2022-08-23" & date <= "2022-08-31 17:00" ~ 1,
                                   date >= "2022-11-30 19:00" & date <= "2022-12-01 05:00" ~ 1,
                                   date >= "2022-12-06 13:00" & date <= "2022-12-06 23:00" ~ 1,
                                   date >= "2023-08-01" & date <= "2023-08-31" ~ 1,
                                   date >= "2023-10-20 19:00" & date <= "2023-10-21 11:00" ~ 1,
                                   date >= "2024-02-08 13:00" & date <= "2024-02-08 23:00" ~ 1,
                                   date >= "2024-04-02 01:00" & date <= "2024-04-03 05:00" ~ 1,
                                   date >= "2024-11-12 07:00" & date <= "2024-11-12 17:00" ~ 1,
                                   TRUE ~ 0),
         across(c(upwelling:south_atlantic), ~ ifelse(missing_dates == 1,NA_real_,.)),
         across(-date, ~ifelse(is.na(ws) == TRUE | is.na(wd) == TRUE,NA_real_,.)),
         ocean = upwelling + north_atlantic + south_atlantic,
         african = central_africa + sahel + west_africa + sahara,
         polluted = african + north_america + europe,
         air_masses = case_when(wd >= 100 & wd <= 340 | ws <= 2 ~ "Local pollution",
                                ocean >= 99.99 & south_atlantic < 1 | south_america < 1 & south_america > 0 & ocean >= 99.28 ~ "North Atlantic",
                                polluted < 5 & south_atlantic > 1 | south_america > 0 & polluted < 5  ~ "Southern Hemisphere",
                                african > 0 & african <= 1 & north_america <= african & europe <= african ~ "African (coastal)",
                                europe <= african & north_america <= african & african > 1 ~ "African (continental)",
                                europe > african & europe >= north_america & europe > 0  ~ "European/North Atlantic",
                                north_america > african & north_america > europe & north_america > 0 ~ "North American/Atlantic"))

dat_air_masses %>%
  filter(is.na(sahara) == F) %>%
  ggplot(aes(air_masses,fill = air_masses)) +
  theme_bw() +
  geom_histogram(stat = "count") +
  labs(x = NULL,
       y = "Number of occurances") +
  theme(legend.position = "None") +
  scale_fill_manual(values = c("darkorange","darkred","darkolivegreen3","khaki4","springgreen4","navy","steelblue1")) +
  NULL

# ggsave("air_mass_classification.png",
#        path = "output/plots",
#        height = 15,
#        width = 30,
#        units = "cm")

# test = dat_air_masses %>%
#   mutate(hour = hour(date)) %>%
#   filter(is.na(sahara) == F,
#          # date >= "2023-06-01" & date <= "2023-09-30" | date >= "2024-09-01",
#          # hour == 0 | hour == 6 | hour == 12 | hour == 18,
#          ) %>%
#   select(date,o3_ppb,upwelling:air_masses)

#classified based on hourly air masses (interpolate between them)
# dat_air_masses = dat %>% 
#   mutate(across(c(upwelling:south_atlantic),na.approx,na.rm = F,maxgap = 5),
#          ocean = upwelling + north_atlantic + south_atlantic,
#          african = central_africa + sahel + west_africa + sahara,
#          polluted = african + north_america + europe + south_america,
#          air_masses = case_when(wd >= 100 & wd <= 340 | ws <= 2 ~ "Local pollution",
#                                 ocean >= 99.99 ~ "Atlantic",
#                                 african < 10 & african >= europe & african >= north_america ~ "African (coastal)",
#                                 europe <= african & north_america <= african & african >= 10 ~ "African (continental)",
#                                 europe > african & europe >= north_america  ~ "European aged pollution",
#                                 north_america > african & north_america > europe ~ "North American aged pollution"))
# 
# test = dat_air_masses %>% 
#   filter(is.na(upwelling) == T) %>% 
#   select(date)

# Diurnals in different air masses ---------------------------------------

dat_air_masses %>% 
  mutate(hour = hour(date),
         year = year(date),
         no_ppt = ifelse(no_flag <= 0.147,no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147,no2_ppt,NA_real_),
         o3_ppb = ifelse(no_flag == 0.599 | no_flag == 0.559 | no2_flag == 0.599 | no2_flag == 0.559,
                         NA_real_,o3_ppb),
         # air_masses = ifelse(air_masses == "Southern Hemisphere" & o3_ppb > 20,NA_real_,air_masses)
         ) %>% 
  filter(is.na(air_masses)== F,
         air_masses != "Local pollution",
         air_masses != "Southern Hemisphere",
         # year >= 2020
         ) %>%
  group_by(hour,air_masses) %>% 
  summarise(across(c(no_ppt,no2_ppt,o3_ppb),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>% 
  ungroup() %>% 
  mutate(no_err_plot_max = no_ppt_mean + no_ppt_se,
         no_err_plot_min = no_ppt_mean - no_ppt_se,
         no2_err_plot_max = no2_ppt_mean + no2_ppt_se,
         no2_err_plot_min = no2_ppt_mean - no2_ppt_se,
         o3_err_plot_max = o3_ppb_mean + o3_ppb_se,
         o3_err_plot_min = o3_ppb_mean - o3_ppb_se) %>% 
  rename(`NO~(ppt)` = no_ppt_mean,'NO[2]~(ppt)' = no2_ppt_mean,`O[3]~(ppb)` = o3_ppb_mean) %>%
  pivot_longer(c(`NO~(ppt)`,`NO[2]~(ppt)`,`O[3]~(ppb)`)) %>%
  pivot_longer(cols = c(no_err_plot_max,no2_err_plot_max,o3_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>%
  pivot_longer(cols = c(no_err_plot_min,no2_err_plot_min,o3_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>%
  mutate(flag = case_when(name == "NO~(ppt)" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          # name == "O[3]~(ppb)" & min_err_n == "o3_err_plot_min" & max_err_n == "o3_err_plot_max" ~ "o3",
                          name == "NO[2]~(ppt)" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>%
  filter(is.na(flag) == F) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,col = air_masses),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = air_masses),alpha = 0.25) +
  # facet_nested_wrap(~name + year,labeller = label_parsed,scales = "free_y") +
  facet_wrap(~name,scales = "free",labeller = label_parsed) +
  scale_colour_manual(values = c("darkorange","darkred","darkolivegreen3","springgreen4","navy","steelblue1")) +
  scale_fill_manual(values = c("darkorange","darkred","darkolivegreen3","springgreen4","navy","steelblue1")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  labs(x = "Hour of day (UTC)",
       y = "Mixing ratio",
       # y = "NO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)

# ggsave("diurnals_different_air_masses_no_SH.png",
#        path = "output/plots",
#        height = 15,
#        width = 33,
#        units = "cm")

# Mixing ratios in different air masses ---------------------------------------------------------------

#removed any datapoints where not all 5 midday hours are available and resolved outlier SH issues

dat_boxplot = dat_air_masses%>% 
  select(date,no_ppt,no_flag,no2_ppt,no2_flag,o3_ppb,air_masses,upwelling:south_atlantic) %>% 
  mutate(date_only = as.Date(date),
         hour = hour(date),
         daytime_no = ifelse(hour >= 11 & hour <= 15 & air_masses != "Local pollution" & no_flag <= 0.147,no_ppt,NA_real_),
         daytime_no2 = ifelse(hour >= 11 & hour <= 15 & air_masses != "Local pollution" & no2_flag <= 0.147,no2_ppt,NA_real_),
         ozone_nine = ifelse(hour == 9 & air_masses != "Local pollution",o3_ppb,NA_real_),
         ozone_five = ifelse(hour == 17 & air_masses != "Local pollution",o3_ppb,NA_real_)) %>% 
  group_by(date_only) %>% 
  summarise(across(c(daytime_no,daytime_no2),list(mean = ~mean(.,na.rm = T),count = ~sum(!is.na(.)))),
            across(c(ozone_five,ozone_nine,upwelling:south_atlantic),~mean(.,na.rm = T))) %>% 
  mutate(date = as.POSIXct.Date(date_only),
         delta_ozone = ozone_five - ozone_nine,
         daytime_no = ifelse(daytime_no_count != 5,NA_real_,daytime_no_mean),
         daytime_no2 = ifelse(daytime_no2_count != 5,NA_real_,daytime_no2_mean),
         missing_dates = case_when(date >= "2022-08-23" & date <= "2022-08-31 17:00" ~ 1,
                                   date >= "2022-11-30 19:00" & date <= "2022-12-01 05:00" ~ 1,
                                   date >= "2022-12-06 13:00" & date <= "2022-12-06 23:00" ~ 1,
                                   date >= "2023-10-20 19:00" & date <= "2023-10-21 11:00" ~ 1,
                                   date >= "2024-02-08 13:00" & date <= "2024-02-08 23:00" ~ 1,
                                   date >= "2024-04-02 01:00" & date <= "2024-04-03 05:00" ~ 1,
                                   date >= "2024-11-12 07:00" & date <= "2024-11-12 17:00" ~ 1,
                                   TRUE ~ 0),
         across(c(upwelling:south_atlantic), ~ ifelse(missing_dates == 1,NA_real_,.)),
         ocean = upwelling + north_atlantic,
         southern_hemisphere = south_atlantic + south_america,
         african = central_africa + sahel + west_africa + sahara,
         polluted = african + north_america + europe,
         air_masses = case_when(ocean >= 99.99 & south_atlantic < 1 | south_america < 1 & south_america > 0 & ocean >= 99.28 ~ "North Atlantic",
                                polluted < 5 & south_atlantic > 1 | south_america > 0 & polluted < 5  ~ "Southern Hemisphere",
                                african > 0 & african <= 1 & north_america <= african & europe <= african ~ "African (coastal)",
                                europe <= african & north_america <= african & african > 1 ~ "African (continental)",
                                europe > african & europe >= north_america & europe > 0  ~ "European/North Atlantic",
                                north_america > african & north_america > europe & north_america > 0 ~ "North American/Atlantic")) %>% 
         # air_masses = case_when(ocean >= 99.9 ~ "NA",
         #                        # southern_hemisphere > 10 & polluted < 1 ~ "SH",
         #                        europe <= african & north_america <= african & african > 1 ~ "Afr (continent)",
         #                        europe > african & europe >= north_america & europe > 0  ~ "Eu/NA",
         #                        north_america > african & north_america > europe & north_america > 0 ~ "Am/NA",
         #                        african > 0 & african <= 1 & north_america <= african & europe <= african ~ "Afr (coast)")) %>% 
  select(date,daytime_no,daytime_no2,delta_ozone,upwelling:air_masses)

dat_boxplot %>% 
  mutate(month = month(date),
         year = year(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  filter(is.na(air_masses) == F,
         # date >= "2020-01-01",
         air_masses != "Southern Hemisphere",
         year == 2024) %>% 
  # mutate(daytime_no = ifelse(daytime_no < -2.5 | daytime_no > 20,NA_real_,daytime_no),
  #        daytime_no2 = ifelse(daytime_no2 > 100,NA_real_,daytime_no2),
  #        delta_ozone = ifelse(delta_ozone < -20 | delta_ozone > 10,NA_real_,delta_ozone)) %>%
  rename(`Daytime~NO~(ppt)` = daytime_no,
         `Daytime~NO[2]~(ppt)` = daytime_no2,
         `Delta~O[3]~(ppb)` = delta_ozone) %>% 
  pivot_longer(c(`Delta~O[3]~(ppb)`,`Daytime~NO[2]~(ppt)`,`Daytime~NO~(ppt)`)) %>%
  ggplot(aes(air_masses,value,fill = air_masses)) +
  theme_bw() +
  geom_boxplot() +
  facet_wrap(~name, scales = "free",labeller = label_parsed) +
  theme(legend.position = "top",
        text = element_text(size =  20),
        axis.text.x = element_text(size=10)) +
  scale_x_discrete(labels = c("African (coastal)" = "Afr (coast)",
                              "African (continental)" = "Afr (cont)",
                              "European/North Atlantic" = "Eu/NA",
                              "North American/Atlantic" = "NAA",
                              "North Atlantic" = "NA",
                              "Southern Hemisphere" = "SH")) +
  labs(x = NULL,
       y = NULL,
       fill = NULL) +
  scale_fill_manual(values = c("darkorange","darkred","darkolivegreen3","springgreen4","navy","steelblue1"))

# ggsave("boxplot.png",
#        path = "output/plots",
#        height = 15,
#        width = 30,
#        units = "cm")


# NO2 PSS analysis --------------------------------------------------------

no2_pss = dat_air_masses %>% 
  filter(no2_ppt > 0,
         no_ppt > 0) %>%
  mutate(temp_k = temp_10m_deg_c + 273.15,
         k = 1.4 * 10^-12 * exp(-1310/temp_k),
         o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         hour = hour(date),
         no_ppt = ifelse(no_flag <= 0.147,no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147,no2_ppt,NA_real_),
         no_molecule_cm3 = ppt_to_molecules_cm3(no_ppt),
         no2_molecule_cm3 = ppt_to_molecules_cm3(no2_ppt),
         no2_lifetime = (1/j_no2)/60) %>% 
  filter(hour >= 11 & hour <= 15 ) %>% 
  mutate(no2_pss = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k)/jno2),
         # no2_pps_298 = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k_298)/j_no2),
         leighton_ratio = (jno2*no2_molecule_cm3)/(k*o3_molecule_cm3*no_molecule_cm3),
         year = year(date))

#no2 obs vs no2 pss
no2_pss %>% 
  filter(no2_ppt > 0,
         no_ppt > 0,
         is.na(air_masses) == F,
         air_masses != "Local pollution") %>% 
  mutate(year = year(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  ggplot(aes(no2_ppt,no2_pss)) +
  theme_bw() +
  geom_point() +
  facet_wrap(~season,scales = "free") +
  # labs(x = expression(NO[2~Obs]~(ppt)),
  #      y = expression(NO[2~PSS]~(ppt))) +
  stat_poly_line(col = "steelblue1") +
  geom_abline(slope = 1,intercept = 0,col = "darkorange",size = 1,linetype = "dashed") +
  stat_poly_eq(use_label(c("eq"))) +
  NULL

# ggsave("no2_pss_different_seasons.png",
#        path = "output/plots",
#        height = 15,
#        width = 30,
#        units = "cm")
