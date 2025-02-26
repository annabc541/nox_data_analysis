library(tidyverse)
library(openair)
library(zoo)

Sys.setenv(TZ = "UTC")

#read in nox with no2 monthly minima offsets (calculated in cvao_no2_offset) and met data and perform ozone correction calculations

# Reading in data ---------------------------------------------------------

nox = read.csv("output/data/cvao_nox_offset_corr.csv") %>% 
  mutate(date = ymd_hms(date)) %>% 
  rename(no_ppt = no, no2_ppt = no2_corr)

nox_hourly = nox %>% 
  group_by(date = floor_date(date,"1 hour")) %>% 
  summarise(no_count = sum(!is.na(no_ppt)),
            no2_count = sum(!is.na(no2_ppt)),
            ce_mean = mean(ce,na.rm = TRUE),
            no_mean = mean(no_ppt,na.rm = TRUE),
            no2_mean = mean(no2_ppt,na.rm = TRUE),
            no_median = median(no_ppt,na.rm = TRUE),
            no2_median = median(no2_ppt,na.rm = TRUE)) %>% 
  fill(ce_mean,.direction = "downup")

met_data = read.csv("D:/Documents - Copy/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date > "2016-12-31 23:59" & date < "2025-01-01") %>%
  select(date,ws,wd,o3_ppb = O3_ppbV)

# Ozone correction --------------------------------------------------------

#NO can react with ozone in the line to give NO2, but is then not photolysed back to NO
#-> measured NO not representative of what's at the inlet
#NO2 produced in the line (see above), as well as by ozone and NO reaction inside converter
#-> measured NO2 not representative of what's at the inlet

tc = 1 #time in converter 1s
t = 4.3 #time in sample line (between inlet and reaction cell) 4.3s
k = 1.8*10^-14 #rate constant for NO + O3 -> NO2 + O2

corr_calc_dat = met_data %>%
  left_join(nox_hourly) %>%
  #calculations for NO and NO2 ozone correction (derivations in SI of Andersen et al. 2021)
  mutate(ko3 = (1.8*10^-14)*o3_ppb*(10^-9)*(2.48*10^19), #ko3 = kno+o3 * o3 (ppb), kno+o3 = 1.8*10^-14, 10^-9 and 2.48*10^-19 for units
         no_corr = no_mean * exp(ko3 * 4.3), #4.3 is the time the sample gas spends in the sample line
         j = -log(1 - ce_mean)/tc, #j = -ln(1-ce)/time_in_converter
         no2_corr = ((j + ko3)/j) *
           (((no_mean + no2_mean * ce_mean) - no_mean * exp(-j)) /
              (1-exp(-ko3 - j))) - no_corr) %>% 
  select(-c(ce_mean,ko3,j))

flagged_dat = corr_calc_dat %>% 
  mutate(no_flag = case_when(no_corr > 50 ~ 0.459, #0.459 denotes extreme value -> NO above 50 ppt
                             wd > 100 & wd < 340 | ws < 2 ~ 0.559, #0.559 for local contamination (ws and wd indicating air coming from over the island)
                             no_count < 6 ~ 0.391, #0.391 for data coverage < 50% (12 measurements an hour)
                             abs(no_mean - no_median) > 4.8 ~ 0.456, #0.456 invalidated by data originator, likely a spike if these two differ too much
                             TRUE ~ 0), 
         no2_flag = case_when(no2_corr > 200 ~ 0.459,
                              wd > 100 & wd < 340 | ws < 2 ~ 0.559,
                              no2_count < 6 ~ 0.391,
                              abs(no2_mean - no2_median) > 33.3 ~ 0.456,
                              TRUE ~ 0)) %>% 
  select(date,no_corr,no_flag,no2_corr,no2_flag)


dat_to_save = flagged_dat %>% 
  # select(date,no_corr,no2_corr = no2_corr_o3_corr) %>% 
  mutate(date = format(date, "%Y-%m-%d %H:%M:%S"))

write.csv(dat_to_save,"output/data/cvao_nox_offset_ozone_corr.csv",row.names = F)

# Plotting ----------------------------------------------------------------

dat_corr_flagged %>%
  filter(date >= "2024-01-01") %>% 
  # timeAverage("1 day") %>% 
  # pivot_longer(c(no2_corr,no2_corr_o3_corr)) %>%
  mutate(year = year(date),
         no_corr = ifelse(local_pollution == 0,no_corr,NA_real_)) %>%
  ggplot(aes(date,no_corr)) +
  geom_path() +
  # facet_grid(rows = vars(name),scales = "free") +
  # ylim(-30,200) +
  NULL


myOutput = timeVariation(dat_corr_flagged,pollutant = c("diff"),group = "year")

dat = myOutput$data$month

dat %>% 
  ungroup() %>% 
  ggplot(aes(mnth,Mean,col = variable)) +
  theme_bw() +
  geom_path(linewidth = 1) +
  theme(legend.position = "top") +
  labs(x = NULL,
       y = expression(NO[2]~diodes~(ppt)),
       col = NULL) +
  scale_colour_viridis_d() +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

# ggsave("no2_corr_diff.png",
#        path = "~/csiro/output/cvao_no2_offsets",
#        height = 12,
#        width = 30,
#        units = "cm")
