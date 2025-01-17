library(tidyverse)
library(lubridate)
library(zoo)
library(SciViews)

# Reading in data ---------------------------------------------------------

nox2017 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2017_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2018 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2018_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2019 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2019_calc_df_new_altered_night(21-03)_v17.csv") %>%
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2020 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2020_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2021 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2021_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2022 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2022_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2023 = read.csv("processed_data/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(X),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2024 = read.csv("processed_data/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>%
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>% 
  timeAverage("5 min")

# NO2 offsets -------------------------------------------------------------

nox_only = bind_rows(nox2017,nox2018,nox2019,nox2020,nox2021,nox2022,nox2023,nox2024,nox2025) %>% 
  distinct(date, .keep_all = T) %>% 
  arrange(date) %>% 
  filter(date >= "2017-01-01" & date < "2025-01-01")

nox_min_day = nox_only %>%
  timeAverage("1 hour") %>% 
  mutate(no2_ppt = ifelse(no2_ppt <= 0, NA_real_,no2_ppt)) %>% 
  timeAverage("1 day",statistic = "min") %>% 
  mutate(no2_ppt_min = case_when(no2_ppt == Inf ~ NA_real_,
                                 TRUE ~ no2_ppt)) %>% 
  select(date,no2_ppt_min)

nox_min_month = nox_min_day %>%
  timeAverage("1 month",statistic = "min") %>% 
  mutate(no2_ppt_min_month = case_when(no2_ppt_min == Inf ~ NA_real_,
                                       TRUE ~ no2_ppt_min)) %>% 
  select(date,no2_ppt_min_month)

nox_min_month %>% 
  ggplot(aes(date,no2_ppt_min_month)) +
  theme_bw() +
  geom_point(col = "navy",size = 2.5) +
  labs(x = NULL,
       y = expression(NO[2]~monthly~minima~(ppt))) +
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") +
  theme(text = element_text(size = 20))

# ggsave('no2_monthly_minima.png',
#        path = "~/Writing/Thesis/Chapter 2 (NOx processing)/Images",
#        width = 29,
#        height = 12,
#        units = 'cm')

nox_with_min = nox_only %>% 
  left_join(nox_min_month) %>% 
  mutate(no2_ppt_min_inter = na.approx(no2_ppt_min_month,na.rm = F)) %>% 
  fill(no2_ppt_min_inter) %>%
  mutate(no2_ppt_corr = no2_ppt - no2_ppt_min_inter)

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date >= "2017-01-01" & date < "2025-01-01") %>% 
  select(date,ws,wd,o3_ppb = O3_ppbV)

# Hourly averaging --------------------------------------------------------

hourly_calc_dat = nox_with_min %>%
  group_by(date = floor_date(date,"1 hour")) %>% 
  summarise(no_count = sum(!is.na(no_ppt)),
            no2_count = sum(!is.na(no2_ppt_corr)),
            ce_mean = mean(ce,na.rm = TRUE),
            no_mean = mean(no_ppt,na.rm = TRUE),
            no2_mean = mean(no2_ppt_corr,na.rm = TRUE),
            no_median = median(no_ppt,na.rm = TRUE),
            no2_median = median(no2_ppt_corr,na.rm = TRUE),
            no_art_unc = mean(no_art_unc,na.rm = TRUE)) %>% 
  fill(no_art_unc,ce_mean,.direction = "downup")

# Ozone correction --------------------------------------------------------

tc = 1 #time in converter 1s
t = 4.3 #time in sample line (between inlet and reaction cell) 4.3s
k = 1.8*10^-14 #rate constant for NO + O3 -> NO2 + O2

corr_calc_dat = met_data %>%
  left_join(hourly_calc_dat) %>% 
  #calculations for NO and NO2 ozone correction (derivations in SI of Andersen et al. 2021)
  mutate(ko3 = (1.8*10^-14)*o3_ppb*(10^-9)*(2.48*10^19), #ko3 = kno+o3 * o3 (ppb), kno+o3 = 1.8*10^-14, 10^-9 and 2.48*10^-19 for units
         no_corr = no_mean * exp(ko3 * 4.3), #4.3 is the time the sample gas spends in the sample line
         j = -log(1 - ce_mean)/tc, #j = -ln(1-ce)/time_in_converter
         no2_corr = ((j + ko3)/j) *
           (((no_mean + no2_mean * ce_mean) - no_mean * exp(-j)) /
              (1-exp(-ko3 - j))) - no_corr) %>%
  select(-c(ce_mean,ko3,j))

flagged_dat = corr_calc_dat %>% 
  mutate(no_flag = case_when(is.na(no_corr) ~ 0.999, #0.999 missing data
                             wd > 100 & wd < 340 | ws < 2 ~ 0.559, #0.559 for local contamination (ws and wd indicating air coming from over the island)
                             no_count < 6 ~ 0.391, #0.391 for data coverage < 50% (12 measurements an hour)
                             abs(no_mean - no_median) > 4.8 ~ 0.456, #0.456 invalidated by data originator, likely a spike if these two differ too much
                             # no_corr < no_lod ~ 0.147, #0.147 indicates that NO is below LOD, data point still considered valid)
                             no_corr > 50 ~ 0.459, #0.459 denotes extreme value -> NO above 50 ppt
                             TRUE ~ 0), 
         no2_flag = case_when(is.na(no2_corr) ~ 0.999,
                              wd > 100 & wd < 340 | ws < 2 ~ 0.559,
                              no2_count < 6 ~ 0.391,
                              abs(no2_mean - no2_median) > 33.3 ~ 0.456,
                              no2_corr > 200 ~ 0.459,
                              # abs(no2_blc_mean - no2_diode_mean) > 33.7 ~ 0.456, #remove when blc and diode measurements are too different
                              # no2_corr < no2_lod ~ 0.147, #always put this last, because data is still valid if this flag applies and I don't want it to override any invalidating flags
                              TRUE ~ 0)) %>% 
  select(date,no_corr,no2_corr,o3_ppb,no_flag,no2_flag)

#2023 and 2023 data saved and shared with katie (in merge file)

# dat_to_save = flagged_dat %>% 
#   filter(date >= "2023-01-01" & date < "2024-01-01") %>% 
#   select(date,no = no_corr,no2 = no2_corr)
# 
# write.csv(dat_to_save,"nox2023_offset_ozone_corr.csv",row.names = F)

# Plotting timeseries ----------------------------------------------------------------

flagged_dat %>% 
  filter(date >= "2024-11-23 06:00" & date <= "2024-11-23 12:00") %>%
  mutate(pollution_flag = ifelse(no_flag == 0.559,"Local pollution","Baseline"),
         `NO[2]` = ifelse(no2_flag <= 0.147 & no2_corr > 0,no2_corr,NA_real_),
         NO = ifelse(no_flag <= 0.147 & no_corr > 0,no_corr,NA_real_)
  ) %>% 
  pivot_longer(c(no_corr,no2_corr)) %>% 
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  facet_grid(rows = vars(name),scales = "free",labeller = label_parsed) +
  geom_path(group = 1,size = 0.8) +
  labs(x = NULL,
       y = expression(NO[x]~(ppt)),
       col = NULL) +
  theme(legend.position = "None") +
  # scale_x_datetime(date_breaks = "4 days",date_labels = "%d %b %y") +
  NULL

# ggsave("nox_nov_dec.png",
#        path = "~/Cape Verde/nox/output/cvao_nox2024",
#        height = 12,
#        width = 30,
#        units = "cm")


# Plotting monthly averages -----------------------------------------------

myOutput = flagged_dat %>% 
  mutate(pollution_flag = ifelse(no_flag == 0.559,"Local pollution","Baseline"),
         `NO[2]` = ifelse(no2_flag <= 0.147 & no2_corr > 0,no2_corr,NA_real_),
         NO = ifelse(no_flag <= 0.147 & no_corr > 0,no_corr,NA_real_)
  ) %>%
  timeVariation(pollutant = c("NO[2]"),group = "year")

dat = myOutput$data$month

dat %>% 
  ungroup() %>% 
  ggplot(aes(mnth,Mean,col = variable)) +
  theme_bw() +
  geom_path(linewidth = 1) +
  theme(legend.position = "top") +
  labs(x = NULL,
       y = expression(NO[2]~(ppt)),
       col = NULL) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggsave("no2_monthly_averages.png",
       path = "~/Cape Verde/nox/output/cvao_nox2024",
       height = 12,
       width = 30,
       units = "cm")


# Plotting seasonal diurnals ----------------------------------------------

seasonal_diurnals = flagged_dat %>% 
  mutate(pollution_flag = ifelse(no_flag == 0.559,"Local pollution","Baseline"),
         NO2 = ifelse(no2_flag <= 0.147 & no2_corr > 0,no2_corr,NA_real_),
         NO = ifelse(no_flag <= 0.147 & no_corr > 0,no_corr,NA_real_),
         hour = hour(date),
         year = year(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>%
  group_by(hour,season,year) %>% 
  summarise(across(c(NO,NO2),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>%
  ungroup()

seasonal_diurnals %>% 
  mutate(no_max = NO_mean + NO_se,
         no_min = NO_mean - NO_se,
         no2_max = NO2_mean + NO2_se,
         no2_min = NO2_mean - NO2_se) %>% 
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,NO2_mean,col = as.character(year)),size = 1) +
  geom_ribbon(aes(hour,ymin = no2_min,ymax = no2_max,fill = as.character(year)),alpha = 0.25) +
  facet_wrap(~season) +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[2]~(ppt)),
       col = NULL,
       fill = NULL) +
  theme(legend.position = "top")

# ggsave("no2_seasonal_diurnals.png",
#        path = "~/Cape Verde/nox/output/cvao_nox2024",
#        height = 12,
#        width = 30,
#        units = "cm")
