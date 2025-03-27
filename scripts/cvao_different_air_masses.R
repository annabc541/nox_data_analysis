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

# Reading in nox data ---------------------------------------------------------

# setwd("~/Cape Verde/nox/data_submission/downloaded_data/simone")

# files = list.files(full.names = TRUE,pattern = "simone_no1")
# datList = list()
# for(index in 1:length(files)) {
#   
#   datList[[index]] = read.table(files[index],skip = 69,header = TRUE)%>%
#     mutate(date = as.POSIXct(round(start_time / 0.041667) *3600,
#                              origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
#     tibble()
#   
# }
# 
# no14_16 = bind_rows(datList) %>% 
#   arrange(date) %>% 
#   mutate(across(c(nitrogen_monoxide,nitrogen_monoxide_LOD,nitrogen_monoxide_uncertainty),
#                 ~ .x * 1000)) %>% 
#   select(date,no_ppt = nitrogen_monoxide,no_lod_ppt = nitrogen_monoxide_LOD,
#          no_u_ppt = nitrogen_monoxide_uncertainty,no_flag = nitrogen_monoxide_numflag)

setwd("~/Cape Verde/nox/data_submission/downloaded_data")

#reading in NO data from first group on ebas(2012 to 2016)
files = list.files(full.names = TRUE,pattern = "_1")
skip_values = c(71,71,73,75,73)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = skip_values[index],header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.041667) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_1 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no = NO,no_ppb = NO.1,no_flag = flag) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

#reading in NO data from second group on ebas
files = list.files(full.names = TRUE,pattern = "_2.nas")
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = 74,header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.041667) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_2 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no = NO,no_flag = flag_NO) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

#reading in NO data from third group on ebas - appears to be just lod and uncertainty
files = list.files(full.names = TRUE,pattern = "_3")
skip_values = c(74,74,74,74,74,74,74,76,74)
datList = list()
for(index in 1:length(files)) {
  
  datList[[index]] = read.table(files[index],skip = skip_values[index],header = TRUE) %>%
    mutate(date = as.POSIXct(round(starttime / 0.041667) *3600,
                             origin = sprintf("20%s-01-01",str_extract(files[index],"\\d+")))) %>% 
    tibble()
  
}

ebas_no_3 = bind_rows(datList) %>% 
  arrange(date) %>% 
  select(date,no_lod = NO,no_lod_ppb = NO.1,
         no_uncertainty = NO.2,no_uncertainty_ppb = NO.3) %>% 
  rename_with( .fn = function(.x){paste0(.x,"_1")},
               .cols=-date)

ebas_no = left_join(ebas_no_2,ebas_no_3,by = "date") %>%
  mutate(no_ppb_1 = no_1 / 0.6249228119) %>% 
  bind_rows(ebas_no_1) %>% 
  # filter(no_flag_1 != 0.999) %>%
  arrange(date) %>% 
  mutate(no_ppt = ifelse(no_flag_1 == 0.999 | no_ppb_1 >= 9.99999990,NA_real_,no_ppb_1 * 10^3),
         no_u_ppt = ifelse(no_flag_1 == 0.999,NA_real_,no_uncertainty_ppb_1 * 10^3)) %>% 
  select(date,no_ppt,no_flag = no_flag_1) %>% 
  filter(date < "2017-01-01") %>% 
  # bind_rows(early_dat) %>% 
  arrange(date)

# ebas_no %>% 
#   filter(date < "2017-01-01") %>% 
#   mutate(no_ppt_ebas = ifelse(no_flag_ebas <= 0.147,no_ppt_ebas,NA_real_)) %>% 
#   ggplot(aes(date,no_ppt_ebas)) +
#   geom_path()

remove(datList,files,index,skip_values,ebas_no_1,ebas_no_2,ebas_no_3)

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

nox23_24 = read.csv("output/data/nox23-24_offset_ozone_corr.csv") %>% 
  mutate(date = ymd_hms(date))

nox = bind_rows(ebas_no,nox17_21,nox22,nox23_24) %>% 
  arrange(date) %>% 
  mutate(across(c(no_ppt,no_lod_ppt,no_u_ppt), ~ ifelse(no_flag == 0.999,NA_real_,.x)),
         across(c(no2_ppt,no2_lod_ppt,no2_u_ppt), ~ ifelse(no2_flag == 0.999,NA_real_,.x)))

remove(ebas_no,nox17_21,nox22,nox23_24,datList)

# Reading in met and creating df for analysis -----------------------------

met_data = read.csv("~/Cape Verde/20240507_CV_merge.csv") %>% 
  mutate(date = dmy_hm(date),
         date = round_date(date, "1 hour")) %>% 
  filter(date >= "2012-01-01",
         date < "2025-01-01") %>% 
  clean_names() %>% 
  select(date,ws:rh_10m,o3_ppb = o3_ppb_v,co_ppb = co_ppb_v,ch4_all_ppb_v:co2_with_mpi_flasks_ppm_v,
         jno2_calc,jo1d_calc,j_o1d,j_no2)

voc_data = read.csv("data/timeseries_all_GCFID_VOC_data_2006_to_2024_March25.csv") %>% 
  mutate(date = dmy_hm(date_time)) %>% 
  filter(date > "2012-01-01") %>% 
  mutate(propene = ifelse(date >= "2023-01-01",NA_real_,propene)) %>% 
  select(date,ethane:isoprene)

air_masses = read.csv("~/Cape Verde/new_CVAO_sector_%_boxes_1.csv") %>% 
  rename(date = X) %>% 
  mutate(date = ymd_hms(date)) %>% 
  filter(date >= "2012-01-01",
         date < "2025-01-01") %>% 
    clean_names()
  # rename_with(~ gsub("\\.", " ", .))

df_list = list(nox,voc_data,met_data,air_masses)

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
         wind_flag = ifelse(ws <2 | between(wd,100,340),1,0),
         across(-c(date), ~ifelse(is.na(ws) == TRUE | is.na(wd) == TRUE | wind_flag == 1,NA_real_,.)),
         no_flag = case_when(date >= "2012-04-12 13:00" & date <= "2012-05-10" ~ 0.456,
                             date >= "2012-11-19 12:00" & date <= "2012-11-20 02:00" ~ 0.456,
                             date >= "2013-10-05 20:00" & date <= "2013-10-11 08:00" ~ 0.456,
                             date >= "2013-11-06 14:00" & date <= "2013-11-11 14:00" ~ 0.456,
                             date >= "2013-11-27 13:00" & date <= "2013-12-02 15:00" ~ 0.456,
                             date >= "2013-12-09 18:00" & date <= "2013-12-13" ~ 0.456,
                             date >= "2014-04-17" & date <= "2014-04-20 07:00" ~ 0.456,
                             date >= "2016-02-13 04:00" & date <= "2016-02-13 12:00" ~ 0.456,
                             date >= "2016-10-02" & date <= "2016-10-03" ~ 0.456,
                             date >= "2018-02-22 19:00" & date <= "2018-02-25 19:00" ~ 0.456,
                             date >= "2019-07-02" & date <= "2019-07-10" ~ 0.391,
                             date >= "2020-01-21 09:00" & date <= "2020-01-21 23:00" ~ 0.456,
                             date >= "2020-02-01" & date <"2020-02-02" ~ 0.559,
                             date >= "2020-08-01" & date <= "2020-08-03 11:00" ~ 0.456,
                             date >= "2020-08-27" & date <= "2020-08-29 20:00" ~ 0.456,
                             date >= "2021-05-08" & date <= "2021-05-13 15:00" ~ 0.456,
                             date >= "2021-05-19 16:00" & date <= "2021-05-20" ~ 0.456,
                             date >= "2021-05-24 18:00" & date <= "2021-05-27 05:00" ~ 0.456,
                             date >= "2021-05-31" & date <= "2021-06-03 17:00" ~ 0.456,
                             date >= "2021-06-07" & date <= "2021-06-08 06:00" ~ 0.456,
                             date >= "2022-04-14 21:00" & date <= "2022-04-16 10:00" ~ 0.456,
                             date >= "2022-05-01 02:00" & date <= "2022-05-02 22:00" ~ 0.456,
                             date >= "2022-05-16 18:00" & date <= "2022-05-18 17:00" ~ 0.456,
                             date >= "2022-06-28 09:00" & date <= "2022-06-29 13:00" ~ 0.456,
                             date >= "2023-08-31 03:00" & date <= "2023-09-08 15:00" ~ 0.456,
                             date >= "2024-12-20" ~ 0.456,
                             date > "2024-11-03" & date < "2024-11-07" ~ 0.456,#bad cal
                             date >= "2024-11-23 08:00" & date <= "2024-11-23 12:00" ~ 0.456,#massive spike
                             date >= "2024-11-29 16:00" & date <= "2024-11-29 17:00" ~ 0.456,#massive spike
                             TRUE ~ no_flag),
         no2_flag = case_when(date >= "2021-05-08" & date <= "2021-05-13 15:00" ~ 0.456,
                              date >= "2021-05-26" & date < "2021-05-27" ~ 0.456,
                              date >= "2022-05-15 13:00" & date <= "2022-05-18 17:00" ~ 0.456,
                              date >= "2024-12-20" ~ 0.456,
                              date > "2024-11-03" & date < "2024-11-07" ~ 0.456,#bad cal
                              date >= "2024-01-14 13:00" & date <= "2024-01-14 14:00" ~ 0.456,#negative NOx
                              date >= "2024-11-05 00:00" & date <= "2024-11-05 02:00" ~ 0.456, #massive spike, large uncertainty
                              date >= "2024-11-23 08:00" & date <= "2024-11-23 12:00" ~ 0.456,#massive spike
                              date >= "2024-11-29 16:00" & date <= "2024-11-29 17:00" ~ 0.456,#massive spike
                              date >= "2021-05-19 16:00" & date <= "2021-05-31 22:00" ~ 0.456,
                              TRUE ~ no2_flag))

# Timeseries --------------------------------------------------------------

dat %>% 
  # filter(date > "2017-01-01") %>%
  mutate(year = year(date),
         doy = yday(date),
         month = month(date),
         NO = ifelse(no_flag <= 0, no_ppt,NA_real_),
         NO2 = ifelse(no2_flag <= 0 & date >= "2017-01-01", no2_ppt,NA_real_)) %>% 
  timeAverage(pollutant = c("NO","NO2"),avg.time = "1 day") %>%
  rename(`NO[2]` = NO2) %>% 
  pivot_longer(c(NO,`NO[2]`)) %>%
  # filter(year < 2024) %>%
  ggplot(aes(date,value,col = name)) +
  geom_path(linewidth = 0.75) +
  theme_bw() +
  facet_wrap(~name,scales = "free",labeller = label_parsed,ncol = 1) +
  scale_x_datetime(breaks = "1 year",date_labels = "%Y") +
  theme(legend.position = "None",
        text = element_text(size = 20)) +
  scale_colour_manual(values = c("darkorange","steelblue1")) +
  labs(x = NULL,
       y = expression(Daily~mean~NO[x]~mixing~ratio~(ppt))) +
  NULL

# ggsave("nox_daily_timeseries.png",
#        path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
#        height = 15,
#        width = 30,
#        units = "cm")

# Trends? -----------------------------------------------------------------

trend_analysis = dat %>% 
  # filter(date >= "2017-01-01") %>%
  mutate(NO = ifelse(no_flag <= 0.147,no_ppt,NA_real_),
         NO2 = ifelse(no2_flag <= 0.147,no2_ppt,NA_real_),
         african = sahara + west_africa + sahel + central_africa)

trend_plot = smoothTrend(trend_analysis,
                        ylab = "North American air mass (%)",
                        xlab = NULL,
                        # data.thresh = 30,
                        # type = "season",
                        deseason = T,
                        col = c("springgreen4"),
                        # avg.time = "season",
                        pollutant = c("north_america"))

png(filename = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images/cheating/no_trend_deseason.png",
    width = 15, height = 10, units = "cm", res = 300)
print(trend_plot)
dev.off()

# NOx seasonal diurnals -----------------------------------------------------


# breaks_fun <- function(y) {
#   if(max(y) > 10) {
#     seq(0,40,10)
#     }
#   # } else if(max(x) < 0.5) {
#   #   seq(0,0.15,0.05)
#   # }
#   else {seq(0,8,2)}
# }

dat %>% 
  filter(date > "2017-01-01") %>%
  mutate(hour = hour(date),
         year = year(date),
         month = month(date),
         no_ppt = ifelse(no_flag <= 0.147, no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147, no2_ppt,NA_real_),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  group_by(hour,season) %>% 
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
         o3_err_plot_min = o3_ppb_mean - o3_ppb_se) %>% 
  rename(NO = no_ppt_mean,'NO[2]' = no2_ppt_mean) %>%
  pivot_longer(c(NO,`NO[2]`)) %>%
  pivot_longer(cols = c(no_err_plot_max,no2_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>%
  pivot_longer(cols = c(no_err_plot_min,no2_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>%
  mutate(flag = case_when(name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>%
  filter(is.na(flag) == F) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,
                col = factor(season,levels = c("Spring (MAM)","Summer (JJA)","Autumn (SON)","Winter (DJF)"))),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,
                  fill = factor(season,levels = c("Spring (MAM)","Summer (JJA)","Autumn (SON)","Winter (DJF)"))),alpha = 0.25) +
  facet_wrap(~name,scales = "free_y",labeller = label_parsed) +
  # facet_nested_wrap(~factor(season,levels = c("Spring~(MAM)","Summer~(JJA)","Autumn~(SON)","Winter~(DJF)")) + name,
  #                   labeller = label_parsed,nrow = 2,scales = "free_y") +
  # facet_wrap(~factor(season,levels = c("Spring (MAM)","Summer (JJA)","Autumn (SON)","Winter (DJF)"))) +
  scale_colour_manual(values = c("springgreen4","goldenrod1","darkorange","navy")) +
  scale_fill_manual(values = c("springgreen4","goldenrod1","darkorange","navy")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = seq(0,23,4)) +
  # scale_y_continuous(breaks = breaks_fun) +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[x]~mixing~ratio~(ppt)),
       # y = "NO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)

ggsave("nox_seasonal_diurnal.png",
       path = "~/Writing/Thesis/Chapter 3 (NOx CVAO science)/Images",
       height = 15,
       width = 30,
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
  select(date,o3_ppb,no_ppt,no_flag,upwelling:south_atlantic) %>% 
  mutate(date_only = as.Date(date),
         month = month(date),
         year = year(date),
         hour = hour(date),
         daytime_no = ifelse(hour >= 11 & hour <= 12 & no_flag <= 0.147,no_ppt,NA_real_),
         ozone_nine = ifelse(hour == 9 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_five = ifelse(hour == 17 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_ten = ifelse(hour == 22 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_three= ifelse(hour == 3 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_)) %>% 
  group_by(date_only) %>% 
  summarise(across(c(daytime_no),list(mean = ~mean(.,na.rm = T),count = ~sum(!is.na(.)))),
            across(c(ozone_nine:ozone_three,upwelling:south_atlantic),~mean(.,na.rm = T))) %>% 
  mutate(
    date = as.POSIXct.Date(date_only),
         entrainment_deposition = (ozone_three - ozone_ten)/5,
         delta_ozone = (ozone_five - ozone_nine)/8,
         delta_ozone_chem = delta_ozone - entrainment_deposition,
         daytime_no = daytime_no_mean,
    ocean = upwelling + north_atlantic + south_atlantic,
    african = central_africa + sahel + west_africa + sahara,
    polluted = african + north_america + europe)

delta_ozone %>% 
  mutate(year = year(date),
         month = month(date),
         doy = yday(date)) %>% 
  # filter(year >= 2017) %>%
  timeAverage("1 month") %>%
  # mutate(air_masses = case_when(ocean >= 98 & south_atlantic <= 1 ~ "North Atlantic",
  #                               south_atlantic > 1 & south_atlantic + south_america > polluted ~ "Southern Hemisphere",
  #                               african > polluted/2 & african > 1 ~ "African",
  #                               europe > polluted/2 & europe > 1 ~ "European/North Atlantic",
  #                               north_america > polluted/2 & north_america > 1 ~ "North American/Atlantic")) %>% 
  # group_by(month) %>%
  # summarise(entrainment_deposition = mean(entrainment_deposition,na.rm = T),
            # delta_ozone = mean(delta_ozone,na.rm = T),
            # delta_ozone_chem = mean(delta_ozone_chem,na.rm = T),
            # daytime_no = mean(daytime_no,na.rm = T)) %>%
  pivot_longer(c(daytime_no,delta_ozone_chem)) %>%
  ggplot(aes(date,value,col = name)) +
  theme_bw() +
  # stat_poly_line(col = "steelblue1",fullrange = T) +
  # stat_poly_eq(mapping = use_label("eq"),
  #              label.x = 0.03,label.y = 0.95,size = 5) +
  # stat_poly_eq(mapping = use_label(c("rr.label", "n")),
  #              label.x = 0.03,label.y = 0.85,size = 5) +
  facet_grid(rows = vars(name),scales = "free") +
  geom_point(size = 2) +
  # scale_x_continuous(
  #   breaks = 1:12,
  #   labels = month.abb
  # ) +
  geom_path() +
  theme(legend.position = "top",
        text = element_text(size =20)) +
  labs(col = NULL,
       x = NULL,
       y = NULL)

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

ggsave("delta_o3_daytime_no_monthly.png",
       path = "output/plots",
       height = 15,
       width = 30,
       units = "cm")
  

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

#classified based on daily averages
air_masses_classified = dat %>% 
  select(date,upwelling:south_atlantic,ws,wd) %>% 
  mutate(across(-date, ~ifelse(is.na(ws) == TRUE | is.na(wd) == TRUE | ws <= 2 | between(wd,100,340),NA_real_,.))) %>% 
  timeAverage("1 day",data.thresh = 16) %>% 
  mutate(ocean = upwelling + north_atlantic + south_atlantic,
         african = central_africa + sahel + west_africa + sahara,
         polluted = african + north_america + europe,
         air_masses = case_when(ocean >= 98 & south_atlantic <= 1 ~ "North Atlantic",
                                south_atlantic > 1 & south_atlantic + south_america > polluted ~ "Southern Hemisphere",
                                african > polluted/2 & african > 1 ~ "African",
                                europe > polluted/2 & europe > 1 ~ "European/North Atlantic",
                                north_america > polluted/2 & north_america > 1 ~ "North American/Atlantic",
                                # is.na(sahara) == F ~ "Unclear"
                                ),
         na_flag = ifelse(is.na(air_masses),1,0)) %>% 
  select(date,air_masses,na_flag)

air_mass_stats = air_masses_classified %>% 
  count(air_masses) %>% 
  mutate(percent = n/sum(n) *100)

air_masses_classified %>%
  mutate(month = month(date),
         year = year(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring~(MAM)",
                            month >= 6 & month <= 8 ~ "Summer~(JJA)",
                            month >= 9 & month <= 11 ~ "Autumn~(SON)",
                            TRUE ~ "Winter~(DJF)")) %>%
  filter(is.na(air_masses) == F,
         year == 2016) %>%
  ggplot(aes(air_masses,fill = air_masses)) +
  theme_bw() +
  geom_histogram(stat = "count") +
  labs(x = NULL,
       y = "Number of occurrences",
       fill = NULL) +
  theme(legend.position = "None") +
  facet_wrap(~factor(season,levels = c("Spring~(MAM)","Summer~(JJA)","Autumn~(SON)","Winter~(DJF)")),
             labeller = label_parsed) +
  scale_x_discrete(labels = c("African" = "Afr",
                              "European/North Atlantic" = "Eu/NAt",
                              "North American/Atlantic" = "Am/NAt",
                              "North Atlantic" = "NAt",
                              "Southern Hemisphere" = "SH")) +
  scale_fill_manual(values = c("goldenrod1","darkolivegreen3","springgreen4","navy","steelblue1")) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  NULL

dat_air_masses = dat %>% 
  timeAverage("1 hour") %>% 
  left_join(air_masses_classified,by = "date") %>% 
  fill(na_flag,air_masses,.direction = "down") %>% 
  mutate(air_masses = ifelse(na_flag == 1,NA_real_,air_masses)) %>% 
  select(-na_flag)

nox_mean_airmass = dat_air_masses %>% 
  mutate(no_ppt = ifelse(no_flag <= 0.147,no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0.147,no2_ppt,NA_real_),
         hour = hour(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  filter(date > "2017-01-01",
         hour >= 11 & hour <= 15) %>%
  group_by(air_masses) %>% 
  summarise(across(c(no_ppt,no2_ppt),list(mean = ~mean(.,na.rm = T),
                                          sd =~2*sd(.,na.rm = T),
                                          se = ~sd(., na.rm = TRUE) / sqrt(length(.)),
                                          count = ~sum(!is.na(.)))))

# ggsave("air_mass_classification.png",
#        path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
#        height = 15,
#        width = 30,
#        units = "cm")

# Diurnals in different air masses ---------------------------------------

dat_air_masses %>% 
  mutate(hour = hour(date),
         year = year(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)"),
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
  group_by(hour,air_masses,season) %>% 
  summarise(across(c(no_ppt,no2_ppt,o3_ppb),list(mean = ~mean(.,na.rm = T),sd = ~sd(.,na.rm = T),se = ~sd(., na.rm = TRUE) / sqrt(length(.))))) %>% 
  ungroup() %>% 
  mutate(no_err_plot_max = no_ppt_mean + no_ppt_se,
         no_err_plot_min = no_ppt_mean - no_ppt_se,
         no2_err_plot_max = no2_ppt_mean + no2_ppt_se,
         no2_err_plot_min = no2_ppt_mean - no2_ppt_se,
         o3_err_plot_max = o3_ppb_mean + o3_ppb_se,
         o3_err_plot_min = o3_ppb_mean - o3_ppb_se) %>% 
  rename(NO = no_ppt_mean,'NO[2]' = no2_ppt_mean,`O[3]~(ppb)` = o3_ppb_mean) %>%
  pivot_longer(c(NO,`NO[2]`,`O[3]~(ppb)`)) %>%
  pivot_longer(cols = c(no_err_plot_max,no2_err_plot_max,o3_err_plot_max),values_to = "max_err_v",names_to = "max_err_n") %>%
  pivot_longer(cols = c(no_err_plot_min,no2_err_plot_min,o3_err_plot_min),values_to = "min_err_v",names_to = "min_err_n") %>%
  mutate(flag = case_when(name == "NO" & min_err_n == "no_err_plot_min" & max_err_n == "no_err_plot_max" ~ "no",
                          # name == "O[3]~(ppb)" & min_err_n == "o3_err_plot_min" & max_err_n == "o3_err_plot_max" ~ "o3",
                          name == "NO[2]" & min_err_n == "no2_err_plot_min" & max_err_n == "no2_err_plot_max" ~ "no2")) %>%
  filter(is.na(flag) == F) %>%
  ggplot() +
  theme_bw() +
  geom_path(aes(hour,value,col = air_masses),size = 0.75) +
  geom_ribbon(aes(hour,ymin = min_err_v,ymax = max_err_v,fill = air_masses),alpha = 0.25) +
  # facet_nested_wrap(~name + year,labeller = label_parsed,scales = "free_y") +
  # facet_wrap(~name,scales = "free",labeller = label_parsed) +
  facet_grid(rows = vars(name),cols = vars(season),scales = "free", labeller = label_parsed) +
  scale_colour_manual(values = c("darkred","darkolivegreen3","springgreen4","navy","steelblue1")) +
  scale_fill_manual(values = c("darkred","darkolivegreen3","springgreen4","navy","steelblue1")) +
  theme(legend.position = "top",
        text = element_text(size =  20)) +
  scale_x_continuous(breaks = c(0,4,8,12,16,20)) +
  labs(x = "Hour of day (UTC)",
       y = expression(NO[x]~mixing~ratio~(ppt)),
       # y = "NO mixing ratio (ppt)",
       col = NULL,
       fill = NULL)

ggsave("diurnals_different_air_masses_no_SH.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
       height = 15,
       width = 30,
       units = "cm")

# Mixing ratios in different air masses ---------------------------------------------------------------

#boxplot facetted by air mass and by nox
dat_air_masses %>% 
  mutate(month = month(date),
         year = year(date),
         hour = hour(date),
         no_ppt = ifelse(no_flag <= 0 & between(hour,11,12), no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag <= 0 & between(hour,11,12), no2_ppt,NA_real_),
         ozone_nine = ifelse(hour == 9,o3_ppb,NA_real_),
         ozone_five = ifelse(hour == 17,o3_ppb,NA_real_),
         ozone_ten = ifelse(hour == 22 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         ozone_three= ifelse(hour == 3 & no_flag != 0.559 & no_flag != 0.599,o3_ppb,NA_real_),
         air_masses = case_when(air_masses == "North Atlantic" ~ "North~Atlantic",
                                air_masses == "European/North Atlantic" ~ "European/North~Atlantic",
                                TRUE ~ air_masses)) %>% 
  # filter(hour >= 11 & hour <= 15) %>% 
  timeAverage("1 day") %>% 
  mutate(delta_ozone = (ozone_five - ozone_nine)/8, #divide by 8 to make it daily as there are 8 hours between 9:00 and 17:00
         entrainment_deposition = (ozone_three - ozone_ten)/5,
         delta_ozone = (ozone_five - ozone_nine)/8,
         delta_ozone_chem = delta_ozone - entrainment_deposition,
         ocean = upwelling + north_atlantic + south_atlantic,
         african = central_africa + sahel + west_africa + sahara,
         polluted = african + north_america + europe,
         air_masses = case_when(ocean >= 98 & south_atlantic <= 1 ~ "North~Atlantic",
                                south_atlantic > 1 & south_atlantic + south_america > polluted ~ "Southern Hemisphere",
                                african > polluted/2 & african > 1 ~ "African",
                                europe > polluted/2 & europe > 1 ~ "European/North~Atlantic",
                                north_america > polluted/2 & north_america > 1 ~ "North American/Atlantic")) %>% 
  filter(air_masses != "Southern Hemisphere",
         air_masses != "North American/Atlantic") %>%
  rename(`Daytime~NO~(ppt)` = no_ppt,`Daytime~NO[2]~(ppt)` = no2_ppt,
         `Delta~O[3]~(ppb~d^{-1})` = delta_ozone_chem) %>% 
  pivot_longer(c(`Daytime~NO~(ppt)`,`Daytime~NO[2]~(ppt)`,`Delta~O[3]~(ppb~d^{-1})`)) %>%
  ggplot(aes(as.character(year),value,fill = air_masses)) +
  theme_bw() +
  geom_boxplot(outliers = F,varwidth =  T) +
  scale_fill_manual(values = c("goldenrod1","darkolivegreen3","#1E3799")) +
  # ylim(-5,20) +
  # ylim(0,100) +
  facet_grid(rows = vars(name),cols = vars(air_masses),scales = "free", labeller = label_parsed) +
  theme(legend.position = "None",
      text = element_text(size =  20),
      axis.text.x = element_text(size= 19,
                                 angle = 45,
                                 vjust = 1,
                                 hjust = 1)) +
  labs(x = NULL,
       y = NULL,
       # y = expression(Daytime~NO[x]~mixing~ratio~(ppt)),
       fill = NULL)

ggsave("daytime_nox_delta_o3_air_mass_boxplot2012.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
       height = 20,
       width = 35,
       units = "cm")

# Global shipping emissions comparison ------------------------------------

global_shipping_emissions = read.csv("data/timeSeries_16_21_global.csv") %>% 
  mutate(date = ymd(Date)) %>% 
  select(date,typeMerge,buildPeriod,NOx)

cvao_nox_ocean = dat_air_masses %>% 
  mutate(NO = ifelse(no_flag <= 0.147, no_ppt,NA_real_),
         NO2 = ifelse(no2_flag <= 0.147, no2_ppt,NA_real_),
         NOx = ifelse(is.na(NO) == F & is.na(NO2) == F,NO + NO2,NA_real_),
         year = year(date)) %>%
  filter(air_masses == "North Atlantic",
         year >= 2017 & year <= 2021) %>% 
  timeAverage("1 day",statistic = "mean") %>% 
  select(date,cvao_nox = NOx)

global_shipping_emissions_comp = global_shipping_emissions %>% 
  mutate(year = year(date)) %>% 
  filter(year >= 2017 & year <= 2021) %>% 
  group_by(date) %>%
  summarise(nox_shipping_tonnes = sum(NOx)) %>% 
  left_join(cvao_nox_ocean,by = "date")

global_shipping_emissions_comp %>% 
  mutate(year = year(date),
         nox_shipping_tonnes = nox_shipping_tonnes/10^6) %>% 
  group_by(year) %>% 
  summarise(nox_shipping_tonnes = sum(nox_shipping_tonnes),
            cvao_nox = mean(cvao_nox,na.rm = T)) %>% 
  mutate(CVAO = "CVAO",
         Shipping = "Shipping") %>% 
  ggplot() +
  geom_path(aes(year,cvao_nox,col = CVAO),size = 1) +
  geom_path(aes(year,nox_shipping_tonnes,col = Shipping),size = 1) +
  theme_bw() +
  scale_y_continuous(name = expression(Mean~North~Atlantic~NO[x]~(ppt)),
                     sec.axis = sec_axis(~.,name = expression(NO[x]~shipping~emissions~(10^{6}~tonnes)))) +
  scale_colour_manual(values = c("steelblue1","darkorange"),
                      breaks = c("CVAO","Shipping")) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  labs (x = NULL,
        col = NULL)

ggsave("nox_shipping_emissions.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
       height = 15,
       width = 30,
       units = "cm")

# Reading in ro2 and ho2 ---------------------------------------------------

box_modelled_ro2 = read.csv("output/data/box_modelled_radicals/CVAO_modelled_concs_RO2_10112021.csv") %>% 
  clean_names()

monthly_yearly_means_ro2 = box_modelled_ro2 %>% 
  pivot_longer(c(april_2018:september_2019)) %>% 
  mutate(b = paste0(str_replace_all(name, "_", "-")),
         c = as.POSIXct(parse_date(b,"%B-%Y")),
         minute = 00,
         d = glue::glue("{c} {hour}:{minute}"),
         date = as.POSIXct(d)) %>% 
  mutate(hour = hour(date),
         month = month(date),
         year = year(date)) %>% 
  arrange(hour,month,year) %>% 
  select(year,month,hour,ro2 = value)

monthly_means_ro2 = box_modelled_ro2 %>% 
  mutate(
    `4` = rowMeans(across(matches("(?i)(april)"))),
    `8` = rowMeans(across(matches("(?i)(august)"))),
    `12` = rowMeans(across(matches("(?i)(december)"))),
    `2` = rowMeans(across(matches("(?i)(february)"))),
    `1` = rowMeans(across(matches("(?i)(january)"))),
    `7` = rowMeans(across(matches("(?i)(july)"))),
    `6` = rowMeans(across(matches("(?i)(june)"))),
    `3` = rowMeans(across(matches("(?i)(march)"))),
    `5` = rowMeans(across(matches("(?i)(may)"))),
    `11` = rowMeans(across(matches("(?i)(november)"))),
    `9` = rowMeans(across(matches("(?i)(september)"))),
    `10` = october_2017) %>% 
  pivot_longer(c(`4`:`10`),names_to = "month",values_to = "ro2") %>% 
  arrange(month) %>% 
  mutate(month = as.numeric(month)) %>%
  select(hour,month,ro2)

box_modelled_ho2 = read.csv("output/data/box_modelled_radicals/CVAO_modelled_concs_HO2_10112021.csv") %>% 
  clean_names()

monthly_yearly_means_ho2 = box_modelled_ho2 %>% 
  pivot_longer(c(april_2018:september_2019)) %>% 
  mutate(b = paste0(str_replace_all(name, "_", "-")),
         c = as.POSIXct(parse_date(b,"%B-%Y")),
         minute = 00,
         d = glue::glue("{c} {hour}:{minute}"),
         date = as.POSIXct(d)) %>% 
  mutate(hour = hour(date),
         month = month(date),
         year = year(date)) %>% 
  arrange(hour,month,year) %>% 
  select(year,month,hour,ho2 = value)

monthly_means_ho2 = box_modelled_ho2 %>% 
  mutate(
    `4` = rowMeans(across(matches("(?i)(april)"))),
    `8` = rowMeans(across(matches("(?i)(august)"))),
    `12` = rowMeans(across(matches("(?i)(december)"))),
    `2` = rowMeans(across(matches("(?i)(february)"))),
    `1` = rowMeans(across(matches("(?i)(january)"))),
    `7` = rowMeans(across(matches("(?i)(july)"))),
    `6` = rowMeans(across(matches("(?i)(june)"))),
    `3` = rowMeans(across(matches("(?i)(march)"))),
    `5` = rowMeans(across(matches("(?i)(may)"))),
    `11` = rowMeans(across(matches("(?i)(november)"))),
    `9` = rowMeans(across(matches("(?i)(september)"))),
    `10` = october_2017) %>% 
  pivot_longer(c(`4`:`10`),names_to = "month",values_to = "ho2") %>% 
  arrange(month) %>% 
  mutate(month = as.numeric(month)) %>%
  select(hour,month,ho2)

# creating df with ro2 and ho2 for pss ------------------------------------

dat1 = dat_air_masses %>% 
  filter(date >= "2017-01-01") %>% 
  mutate(month = month(date),
         year = year(date),
         hour = hour(date))

dat2 = dat1 %>% 
  left_join(monthly_yearly_means_ro2,by = c("month","hour","year")) %>% 
  left_join(monthly_yearly_means_ho2,by = c("month","hour","year"))

dat3 = dat2 %>% 
  filter(is.na(ro2)) %>% 
  select(-c(ro2,ho2)) %>% 
  left_join(monthly_means_ho2,by = c("month","hour")) %>% 
  left_join(monthly_means_ro2,by = c("month","hour"))

dat_for_pss = dat2 %>% 
  filter(is.na(ro2) == F) %>% 
  bind_rows(dat3) %>% 
  arrange(date)

remove(box_modelled_ho2,box_modelled_ro2,dat1,dat2,monthly_means_ho2,monthly_means_ro2,monthly_yearly_means_ho2,monthly_yearly_means_ro2)

# NO2 PSS analysis  --------------------------------------------------------

io_molecules = ppt_to_molecules_cm3(1.4)
bro_moelcules = ppt_to_molecules_cm3(2.5)

no2_pss = dat_for_pss %>% 
  mutate(month = month(date),
         hour = hour(date)) %>% 
  mutate(temp_k = ifelse(is.na(temp_10m_deg_c),298.15,temp_10m_deg_c + 273.15),
         k = 2.07 * 10^-12 * exp(-1400/temp_k),
         k_ro2 = 2.3 * 10^-12 * exp(360/temp_k),
         k_ho2 = 3.45 * 10^-12 * exp(270/temp_k),
         k_io = 7.15 * 10^-12 * exp(300/temp_k),
         k_bro = 8.7 * 10^-12 * exp(260/temp_k),
         o3_molecule_cm3 = ppt_to_molecules_cm3(o3_ppb * 1000),
         no_ppt = ifelse(no_flag == 0 & between(hour,11,12),no_ppt,NA_real_),
         no2_ppt = ifelse(no2_flag == 0 & between(hour,11,12),no2_ppt,NA_real_),
         no_molecule_cm3 = ppt_to_molecules_cm3(no_ppt),
         no2_molecule_cm3 = ppt_to_molecules_cm3(no2_ppt),
         no2_lifetime = (1/jno2)/60) %>% 
  mutate(no2_pss_ext = molecules_cm3_to_ppt(((o3_molecule_cm3*k+ro2*k_ro2+ho2*k_ho2+k_io*io_molecules+k_bro*bro_moelcules)*no_molecule_cm3)/jno2),
         no2_pss_simp = molecules_cm3_to_ppt(((o3_molecule_cm3*k)*no_molecule_cm3)/jno2),
         ratio = no2_ppt/no2_pss_ext,
         # no2_pps_298 = molecules_cm3_to_ppt((o3_molecule_cm3*no_molecule_cm3*k_298)/j_no2),
         leighton_ratio = (jno2*no2_molecule_cm3)/(k*o3_molecule_cm3*no_molecule_cm3),
         year = year(date))

no2_lifetime = no2_pss %>% 
  filter(is.na(no2_lifetime) == F) %>% 
  mutate(lifetime_ten = no2_lifetime >= 10) %>% 
  count(lifetime_ten) %>% 
  mutate(percent = n/sum(n) *100)

no2_lifetime = mean(no2_pss$no2_lifetime,na.rm = T)

no2_pss_daily = no2_pss %>% 
  filter(no_ppt <= no2_ppt,
         no2_lifetime <= 10) %>%
  select(-c(no_flag,no_lod_ppt,no2_flag,no2_lod_ppt,jno2_calc:j_no2,k:no2_lifetime,leighton_ratio)) %>% 
  timeAverage("1 day") %>% 
  mutate(ocean = upwelling + north_atlantic + south_atlantic,
         african = central_africa + sahel + west_africa + sahara,
         polluted = african + north_america + europe,
         air_masses = case_when(ocean >= 98 & south_atlantic <= 1 ~ "North Atlantic",
                                south_atlantic > 1 & south_atlantic + south_america > polluted ~ "Southern Hemisphere",
                                african > polluted/2 & african > 1 ~ "African",
                                europe > polluted/2 & europe > 1 ~ "European/North Atlantic",
                                north_america > polluted/2 & north_america > 1 ~ "North American/Atlantic"))

# no2 obs vs no2 pss ------------------------------------------------------

no2_pss_daily %>% 
  filter(air_masses != "Southern Hemisphere",
         is.na(air_masses) == F) %>% 
  mutate(year = year(date),
         month = month(date),
         season = case_when(month >= 3 & month <= 5 ~ "Spring (MAM)",
                            month >= 6 & month <= 8 ~ "Summer (JJA)",
                            month >= 9 & month <= 11 ~ "Autumn (SON)",
                            TRUE ~ "Winter (DJF)")) %>% 
  ggplot(aes(no2_ppt,no2_pss_ext)) +
  theme_bw() +
  geom_point(aes()) +
  facet_wrap(~air_masses) +
  labs(x = expression(NO[2~Obs]~(ppt)),
       y = expression(NO[2~PSS]~(ppt))) +
  stat_poly_line(col = "steelblue1",fullrange = T) +
  geom_abline(slope = 1,intercept = 0,col = "darkorange",size = 1,linetype = "dashed") +
stat_poly_eq(mapping = use_label("eq"),
    label.x = 0.03,label.y = 0.95,size = 5) +
  stat_poly_eq(mapping = use_label(c("rr.label", "n")),
               label.x = 0.03,label.y = 0.85,size = 5) +
  theme(text = element_text(size = 20)) +
  ylim(0,140) +
  xlim(0,140) +
  NULL

# ggsave("no2_pss_ext_different_air_masses.png",
#        path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
#        height = 15,
#        width = 30,
#        units = "cm")

# no2 ratio pdf ------------------------------------------------------

#pdf for different air masses
no2_pss_daily %>% 
  mutate(propene = ifelse(year <= 2022,propene,NA_real_),
         co_values = case_when(co_ppb <= 70 ~ "<70",
                               co_ppb > 70 & co_ppb <= 80 ~ "70-80",
                               co_ppb > 80 & co_ppb <= 90 ~ "80-90",
                               co_ppb > 90 & co_ppb <= 100 ~ "90-100",
                               co_ppb > 100 & co_ppb <= 110 ~ "100-110",
                               co_ppb > 110 ~ ">110")) %>% 
  filter(is.na(air_masses) == F,
         air_masses != "Southern Hemisphere") %>% 
  ggplot(aes(ratio)) +
  theme_bw() +
  geom_density(aes(fill = air_masses),alpha = 0.5) +
  geom_vline(aes(xintercept = 1),size = 0.8) +
  geom_segment(aes(x =  1.58, xend = 1.58,
                   y = 0, yend = 0.68),size = 0.8, col = "goldenrod1") +
  geom_segment(aes(x =  1.45, xend = 1.45,
                   y = 0, yend = 0.7),size = 0.8, col = "darkolivegreen3") +
  geom_segment(aes(x =  1.39, xend = 1.39,
                   y = 0, yend = 0.51),size = 0.8, col = "springgreen4") +
  geom_segment(aes(x =  1.32, xend = 1.32,
                   y = 0, yend = 0.465),size = 0.8, col = "navy") +
  scale_fill_manual(values = c("goldenrod1","darkolivegreen3","springgreen4","navy")) +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8)) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = expression(NO[2~Obs]/NO[2~PSS]),
       y = "Density",
       fill = NULL) +
  NULL

# ggsave("no2_pss_ratio_pdf.png",
#        path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
#        height = 15,
#        width = 30,
#        units = "cm")

# no2 ratio boxplot -------------------------------------------------------

#coloured by CO mixing ratios
no2_pss_daily %>% 
  # filter(date >= "2017-07-01" & date <= "2020-06-30") %>%
  mutate(year = as.character(year),
         propene = ifelse(year <= 2022,propene,NA_real_),
         ethane = ethane/1000, #in ppb
         co_values = case_when(co_ppb <= 80 ~ "<80",
                               co_ppb > 80 & co_ppb <= 90 ~ "80-90",
                               co_ppb > 90 & co_ppb <= 100 ~ "90-100",
                               co_ppb > 100 & co_ppb <= 110 ~ "100-110",
                               co_ppb > 110 ~ ">110"),
         ethane_values = case_when(ethane <= 0.5 ~ "< 0.5",
                                   between(ethane,0.5,1) ~ "0.5-1.0",
                                   between(ethane,1,1.5) ~ "1.0-1.5",
                                   ethane >= 1.5 ~ "> 1.5"),
         acetylene_values = case_when(acetylene <= 30 ~ "< 30",
                                      between(acetylene,30,60) ~ "30-60",
                                      between(acetylene,60,90) ~ "60 -90",
                                      between(acetylene,90,120) ~ "90-120",
                                      acetylene >= 120 ~ "> 120"),
         jno2_values = case_when(jno2 <= 0.007 ~ "< 0.7",
                                 between(jno2,0.007,0.008) ~ "0.7-0.8",
                                 between(jno2,0.008,0.009) ~ "0.8 -0.9",
                                 between(jno2,0.009,0.01) ~ "0.9-1.0",
                                 jno2 >= 0.01 ~ "> 1.0"),
         co_values = fct_relevel(co_values,"<80","80-90","90-100","100-110",">110"),
         ethane_values = fct_relevel(ethane_values,"< 0.5","0.5-1.0","1.0-1.5","> 1.5"),
         acetylene_values = fct_relevel(acetylene_values,"< 30","30-60","60 -90","90-120","> 120"),
         jno2_values = fct_relevel(jno2_values,"< 0.7","0.7-0.8","0.8 -0.9","0.9-1.0","> 1.0")) %>% 
  filter(is.na(co_values) == F) %>% 
  ggplot(aes(year,ratio,fill = co_values)) +
  theme_bw() +
  geom_boxplot(outliers = F) +
  geom_hline(yintercept = 1,linetype = "dashed") +
  scale_fill_manual(values = c("darkred","orangered3","darkorange","goldenrod1","lightgoldenrod1")) + #co
  # scale_fill_manual(values = c("navy","royalblue","steelblue1","lightblue")) + #ethane
  # scale_fill_manual(values = c("darkgreen","springgreen4","darkolivegreen3","darkolivegreen1","lightgreen")) + #acetylene
  theme(legend.position = "top",
        text = element_text(size = 18)) +
  labs(x = NULL,
       y = NULL,
       y = expression(NO[2~Obs]/NO[2~PSS]),
       fill = "CO (ppb)")

ggsave("no2_ratio_co.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
       height = 15,
       width = 30,
       units = "cm")

no2_pss_daily %>% 
  filter(is.na(year) == F) %>% 
  mutate(jno2 = jno2 * 10^2) %>% 
  rename(`j[NO[2]]~(10^{-2}~s^{-1})` = jno2,
         `NO~(ppt)` = no_ppt,
         `NO[2~Obs]~(ppt)` = no2_ppt,
         `NO[2~PSS]~(ppt)` = no2_pss_ext,
         `O[3]~(ppb)` = o3_ppb,
         `NO[2~Obs]/NO[2~PSS]` = ratio) %>%  
  pivot_longer(c(`j[NO[2]]~(10^{-2}~s^{-1})`,`O[3]~(ppb)`,`NO~(ppt)`,`NO[2~Obs]~(ppt)`,`NO[2~PSS]~(ppt)`,
                 `NO[2~Obs]/NO[2~PSS]`)) %>% 
  mutate(year = as.character(year),
         name = fct_relevel(name,"j[NO[2]]~(10^{-2}~s^{-1})","O[3]~(ppb)","NO~(ppt)",
                            "NO[2~Obs]~(ppt)","NO[2~PSS]~(ppt)","NO[2~Obs]/NO[2~PSS]")
         ) %>% 
  ggplot(aes(year,value,fill = name)) +
  scale_fill_manual(values = c("darkred","darkorange","goldenrod1","darkolivegreen3","springgreen4","steelblue1")) +
  geom_boxplot(outliers = F) +
  theme_bw() +
  facet_wrap(~name,scales = "free_y",labeller = label_parsed) +
  labs(x = NULL,
       y = NULL) +
  theme(text = element_text(size = 20),
        legend.position = "None",
        axis.text.x = element_text(size= 19,
                                   angle = 45,
                                   vjust = 1,
                                   hjust = 1))

ggsave("boxplot_ratio_species.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images",
       height = 15,
       width = 30,
       units = "cm")
  
  


# Getting sensitivities ---------------------------------------------------

#getting sensitivities
setwd("D:/Cape Verde/processed_data_simone")

files = list.files(pattern = "_cal_",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE)%>%
    tibble() %>% 
    rename(date = DateTime) %>% 
    mutate(date = ymd_hms(date))
  
}

sens14to22 = bind_rows(datList) %>% 
  select(date,SENS) %>% 
  timeAverage("1 day") %>% 
  filter(date < "2021-01-01")


setwd("~/Cape Verde/nox/processing/processed_data")

files = list.files(pattern = "_cal_df.csv",full.names=TRUE)
datList = list()

for(index in 1:length(files)) {
  datList[[index]] = read.csv(files[index],header=TRUE) %>%
    tibble()
}

sens20to23 = bind_rows(datList) %>% 
  filter(!str_detect(DateTime,"2024"),!str_detect(DateTime,"2025")) %>% 
  mutate(date = ymd_hm(DateTime)) %>% 
  select(date,SENS) %>% 
  arrange(date) %>% 
  timeAverage("1 day") %>% 
  filter(date < "2024-01-01")

sens24 = read.csv("NOx_2024_cal_df.csv") %>% 
  mutate(date = ymd_hms(DateTime)) %>% 
  select(date,SENS) %>% 
  arrange(date) %>% 
  timeAverage("1 day") %>% 
  filter(date >= "2024-01-01" & date < "2025-01-01")

sens = bind_rows(sens14to22,sens20to23,sens24) %>% 
  mutate(date = round_date(date,"1 day")) %>% 
  filter(is.na(SENS) == F)

# Timeseries with issues--------------------------------------------------------------

dat_air_masses %>% 
  left_join(sens,by = "date") %>% 
  mutate(year = year(date),
         doy = yday(date),
         month = month(date),
         NO = ifelse(no_flag <= 0, no_ppt,NA_real_),
         NO2 = ifelse(no2_flag <= 0 & date >= "2017-01-01", no2_ppt,NA_real_)) %>% 
  timeAverage(avg.time = "1 month") %>%
  rename(`NO[2]~(ppt)` = NO2,
         `NO~(ppt)` = NO,
         `Sensitivity~(cps~ppt^{-1})` = SENS) %>% 
  pivot_longer(c(`NO[2]~(ppt)`,`NO~(ppt)`,`Sensitivity~(cps~ppt^{-1})`)) %>%
  mutate(name = fct_relevel(name,"NO~(ppt)","NO[2]~(ppt)","Sensitivity~(cps~ppt^{-1})")) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_point() +
  geom_vline(xintercept = c(as.POSIXct("2012-05-01"),as.POSIXct("2017-03-01"),as.POSIXct("2019-11-19"),
                            as.POSIXct("2020-06-11")),
             linewidth = 1,linetype = "dashed",show.legend = TRUE) +
  geom_vline(xintercept = c(as.POSIXct("2014-11-25"),as.POSIXct("2019-10-05")),
             linewidth = 1,linetype = "solid",show.legend = TRUE) +
  theme_bw() +
  facet_wrap(~name,scales = "free",labeller = label_parsed,ncol = 1) +
  scale_x_datetime(breaks = "1 year",date_labels = "%Y") +
  scale_colour_manual(values = c("darkorange","steelblue1","navy")) +
  guides(color = FALSE) +
  # guides(linetype = guide_legend(values = c(solid = "Major Changes", dashed = "Minor Updates"))) +
  theme(legend.position = "top",
        text = element_text(size = 20)) +
  labs(x = NULL,
       y = NULL) +
  NULL

ggsave("nox_sens_monthly_timeseries.png",
       path = "~/Writing/Thesis/Chapter 4 (NOx CVAO science)/Images/cheating",
       height = 15,
       width = 30,
       units = "cm")

# Missing kOH -------------------------------------------------------------

kOH = read.csv("data/missing kOH for Anna.csv") %>% 
  mutate(date = dmy_hm(Time.and.Date)) %>% 
  select(date,kOH_measured = kOHmeasured,kOH_model = kOHmodel,missing_kOH = missing.kOH)

missing_kOH = no2_pss_daily %>% 
  filter(between(date,as.POSIXct("2023-02-15"),as.POSIXct("2023-02-25"))) %>% 
  select(date,no_ppt,no2_ppt,no2_pss_ext,ratio,o3_ppb,ro2,ho2,temp_10m_deg_c) %>% 
  full_join(kOH,by = "date")

missing_kOH %>% 
  timeAverage("1 day") %>% 
  ggplot() +
  geom_point(aes(missing_kOH,ratio))

kOH %>% 
  pivot_longer(c(-date)) %>% 
  ggplot(aes(date,value,col = name)) +
  geom_path()
