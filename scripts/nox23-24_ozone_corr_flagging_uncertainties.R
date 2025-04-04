library(tidyverse)
library(lubridate)
library(zoo)
library(SciViews)
library(openair)

setwd("~/Cape Verde/nox/processing")

#used this to get 2023 and 2024 data (minima offsets in python) ozone corrected and get uncertainties

# Read in data ------------------------------------------------------------

nox2023 = read.csv("processed_data/New folder/NOx_2023_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>% 
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode_sub_30_days,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
  timeAverage("5 min")

nox2024 = read.csv("processed_data/New folder/NOx_2024_calc_df.csv") %>% 
  tibble() %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date,"5 min")) %>%
  select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
         no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>% 
  timeAverage("5 min")

# nox2025 = read.csv("~/Cape Verde/nox/processing/processed_data/NOx_2025_calc_df.csv") %>% 
#   tibble() %>% 
#   mutate(date = ymd_hms(DateTime),
#          date = round_date(date,"5 min")) %>% 
#   select(date,no_ppt = NO_Conc_art_corrected,no2_ppt = NO2_Conc_diode,ce = CE_diode,
#          no_art_unc = NO_art_total_uncertainty,night_diff = NO_night_diff_between_nights) %>%
#   timeAverage("5 min")

# nox_only = bind_rows(nox2023,nox2024) %>% 
#   distinct(date, .keep_all = T) %>% 
#   arrange(date) %>% 
#   filter(date >= "2023-01-01" & date < "2025-01-01")

err_dat23 = read.csv("processed_data/New folder/NOx_2023_error_df.csv",header=TRUE,na.strings= c('NA','missing')) %>% 
  mutate(date = ymd_hms(DateTime)) %>%
  filter(date >= "2023-01-01") %>% 
  select(date,no_lod = LOD_NO_pptv_1h,no2_lod = LOD_NO2_diode_pptv_1h)

err_dat24 = read.csv("processed_data/New folder/NOx_2024_error_df.csv",header=TRUE,na.strings= c('NA','missing')) %>%
  mutate(date = ymd_hms(DateTime)) %>%
  filter(date >= "2024-01-01") %>% 
  select(date,no_lod = LOD_NO_pptv_1h,no2_lod = LOD_NO2_diode_pptv_1h)

# err_dat25 = read.csv("processed_data/NOx_2025_error_df.csv",header=TRUE,na.strings= c('NA','missing')) %>%
#   mutate(date = ymd_hms(DateTime)) %>%
#   select(date,no_lod = LOD_NO_pptv_1h,no2_lod = LOD_NO2_diode_pptv_1h) %>% 
#   filter(date < "2025-01-01")

# err_dat = err_dat24 %>% 
#   bind_rows(err_dat23) %>% 
#   distinct(date, .keep_all = T) %>% 
#   timeAverage("1 hour")

cal23_dat = read.csv("processed_data/New folder/NOx_2023_cal_df.csv") %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2023-01-01" & date < "2024-01-01") %>% 
  select(date,tot_cal_unc_no = Total_calibration_uncertainty_NO,
         tot_cal_unc_no2 = Total_calibration_uncertainty_NO2_diode)

cal24_dat = read.csv("processed_data/New folder/NOx_2024_cal_df.csv") %>% 
  mutate(date = ymd_hms(DateTime),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2024-01-01" & date < "2025-01-01") %>% 
  select(date,tot_cal_unc_no = Total_calibration_uncertainty_NO,
         tot_cal_unc_no2 = Total_calibration_uncertainty_NO2_diode)

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date >= "2023-01-01" & date < "2025-01-01") %>% 
  select(date,ws,wd,o3_ppb = O3_ppbV)

# Hourly stats ------------------------------------------------------------

#determining how many measurements have been done each hour and getting hourly mean and median

calc_dat = nox_with_min

hourly_calc_dat = nox2024 %>%
  group_by(date = floor_date(date,"1 hour")) %>% 
  summarise(no_count = sum(!is.na(no_ppt)),
            no2_count = sum(!is.na(no2_ppt)),
            ce_mean = mean(ce,na.rm = TRUE),
            no_mean = mean(no_ppt,na.rm = TRUE),
            no2_mean = mean(no2_ppt,na.rm = TRUE),
            no_median = median(no_ppt,na.rm = TRUE),
            no2_median = median(no2_ppt,na.rm = TRUE),
            no_art_unc = mean(no_art_unc,na.rm = TRUE)) %>% 
  fill(no_art_unc,ce_mean,.direction = "downup")


# Ozone correction --------------------------------------------------------

#NO can react with ozone in the line to give NO2, but is then not photolysed back to NO
#-> measured NO not representative of what's at the inlet
#NO2 produced in the line (see above), as well as by ozone and NO reaction inside converter
#-> measured NO2 not representative of what's at the inlet

tc = 1 #time in converter 1s
t = 4.3 #time in sample line (between inlet and reaction cell) 4.3s
k = 1.8*10^-14 #rate constant for NO + O3 -> NO2 + O2

corr_calc_dat = met_data %>%
  left_join(hourly_calc_dat) %>% 
  left_join(err_dat24) %>% 
  #calculations for NO and NO2 ozone correction (derivations in SI of Andersen et al. 2021)
  mutate(ko3 = (1.8*10^-14)*o3_ppb*(10^-9)*(2.48*10^19), #ko3 = kno+o3 * o3 (ppb), kno+o3 = 1.8*10^-14, 10^-9 and 2.48*10^-19 for units
         no_corr = no_mean * exp(ko3 * 4.3), #4.3 is the time the sample gas spends in the sample line
         j = -log(1 - ce_mean)/tc, #j = -ln(1-ce)/time_in_converter
         no2_corr = ((j + ko3)/j) *
           (((no_mean + no2_mean * ce_mean) - no_mean * exp(-j)) /
              (1-exp(-ko3 - j))) - no_corr) %>%
  select(-c(ce_mean,ko3,j))

# Flagging data -----------------------------------------------------------

#data flag values for ACTRIS

flagged_dat = corr_calc_dat %>% 
  mutate(no_flag = case_when(is.na(no_corr) ~ 0.999, #0.999 missing data
                             wd > 100 & wd < 340 | ws < 2 ~ 0.559, #0.559 for local contamination (ws and wd indicating air coming from over the island)
                             no_count < 6 ~ 0.391, #0.391 for data coverage < 50% (12 measurements an hour)
                             abs(no_mean - no_median) > 4.8 ~ 0.456, #0.456 invalidated by data originator, likely a spike if these two differ too much
                             no_corr < no_lod ~ 0.147, #0.147 indicates that NO is below LOD, data point still considered valid)
                             no_corr > 50 ~ 0.459, #0.459 denotes extreme value -> NO above 50 ppt
                             TRUE ~ 0), 
         no2_flag = case_when(no2_corr > 200 ~ 0.459,
                              is.na(no2_corr) ~ 0.999,
                              wd > 100 & wd < 340 | ws < 2 ~ 0.559,
                              no2_count < 6 ~ 0.391,
                              abs(no2_mean - no2_median) > 33.3 ~ 0.456,
                              # abs(no2_blc_mean - no2_diode_mean) > 33.7 ~ 0.456, #remove when blc and diode measurements are too different
                              no2_corr < no2_lod ~ 0.147, #always put this last, because data is still valid if this flag applies and I don't want it to override any invalidating flags
                              TRUE ~ 0)) %>% 
  select(date,no_corr,no2_corr,no_lod,no2_lod,o3_ppb,no_flag,no2_flag,no_art_unc)

# Uncertainty calculations ------------------------------------------------

#uncertainty of hourly measurements estimated by combining all uncertainties associated with measurements
#ie uncertainties in cal, art, ozone correction and instrument precision
#error propagation fully explained in SI of Andersen et al. 2021

uncertainties = flagged_dat %>% 
  left_join(cal24_dat) %>% 
  fill(tot_cal_unc_no,tot_cal_unc_no2,.direction = "updown") %>%  
  mutate(o3_corr_u = (0.2^2 + (0.07/o3_ppb)^2)^0.5, #0.2 error for ko3, 0.07 error for ozone
         no_u = ((no_lod)^2 + (tot_cal_unc_no * no_corr)^2 + (no_art_unc)^2 + (o3_corr_u * no_corr)^2)^0.5,
         no2_u = ((no_lod)^2 + (no2_lod)^2 + (tot_cal_unc_no2 * no2_corr)^2 + (o3_corr_u * no2_corr)^2)^0.5) 

data_to_save = uncertainties %>% 
  rename(no = no_corr,
         no2 = no2_corr) %>% 
  rename_with(.fn = function(.x){paste0(.x,"_ppt")},
              .cols = c(no,no2,no_lod,no2_lod,no_u,no2_u)) %>% 
  select(date,no_ppt,no_lod_ppt,no_u_ppt,no_flag,no2_ppt,no2_lod_ppt,no2_u_ppt,no2_flag) %>% 
  filter(date < "2025-01-01") %>% 
  mutate(date = format(date, "%Y-%m-%d %H:%M:%S"))

write.csv(data_to_save,"~/Cape Verde/nox/nox_data_analysis/output/data/nox2024_updated.csv",row.names = F)

data_to_save %>% 
  mutate(no2_ppt = ifelse(no2_flag <= 0.147,no2_ppt,NA_real_)) %>% 
  filter(date > "2024-01-01") %>%
  ggplot(aes(date,no2_ppt)) +
  geom_path()
