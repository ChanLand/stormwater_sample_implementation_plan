# SIP_soil_screen
# Modified from PMark's original script, AMC 2020-06
# V1: This uses data from the updated EIM query, changes the screening standards
# V2: Condenses the output of fd_summary to have fewer columns that are instead coded by SSL or BTV
# V3: Adds in location_id of max_result; uses new site association table
# VSma - same as V3, but groups by sma instead of by site/sma - SW output is only by sma; create tables for SIP documents
# VFinal - cleaned up version of VSma

if(!require(readxl)){
  install.packages("readxl"); require(readxl)}

if(!require(magrittr)){
  install.packages("magrittr"); require(magrittr)}

if(!require(tidyverse)){
  install.packages("tidyverse"); require(tidyverse)}

if(!require(fuzzyjoin)){
  install.packages("fuzzyjoin"); require(fuzzyjoin)}

if(!require(data.table)){
  install.packages("data.table"); require(data.table)}

if(!require(lubridate)){
  install.packages("lubridate"); require(lubridate)}

if(!require(gridExtra)){
  install.packages("gridExtra"); require(gridExtra)}

library(gtable)

library(grid)

# This option prevents values from being in scientific notation
#options(scipen = 999) # keeps numbers from being in scientific notation
# Put back to default settings
#options(scipen = 0, digits = 7)

#options(scipen=2, digits=3)

# Import data - two files because combined file size is greater than the 500K records allowed by EIM
soil_raw_detect <- read_csv("../../SIP/SIP_soil_screening/Data/Soil_Detect_2000_2020.csv",
                            col_types = list('sample_date' = col_date('%m/%d/%Y')))
names(soil_raw_detect) %<>% tolower
names(soil_raw_detect) <- make.names(names(soil_raw_detect), unique=TRUE)

soil_raw_nondetect <- read_csv("../../SIP/SIP_soil_screening/Data/Soil_Nondetect_2000_2020.csv",
                               col_types = list('sample_date' = col_date('%m/%d/%Y')))
names(soil_raw_nondetect) %<>% tolower
names(soil_raw_nondetect) <- make.names(names(soil_raw_nondetect), unique=TRUE)

# Combine detect and non-detect into one dataframe
soil_raw <- bind_rows(soil_raw_detect, soil_raw_nondetect) 
names(soil_raw) %<>%
  str_replace_all("_", ".")
soil_raw$field.preparation.code <- as.character(soil_raw$field.preparation.code)

rm(soil_raw_detect, soil_raw_nondetect)

# Load list of parameters with WQS or TAL
###################### This will be used to filter the FD tables below
standards <- read_csv('../../SIP/SIP_soil_screening/Data/all_water_qual_parameters.csv')

#####
# Import Ryti Soil Background and filter to only soils BV and soils max value.
#####

ryti_bk_raw <- read_excel("../../SIP/SIP_soil_screening/Data/Ryti Soil Backgrounds.xlsx")
names(ryti_bk_raw) %<>% tolower
names(ryti_bk_raw) <- make.names(names(ryti_bk_raw), unique=TRUE)
ryti_bk_raw <- ryti_bk_raw %>% rename(parameter.code = parameter_code) %>%
  rename(report.units = report_units) 

#View(ryti_bk_raw)

ryti_bk <- ryti_bk_raw %>%
  gather(key, value, -parameter.code, -report.units, -'mg.kg.units') %>%
  filter(key %in% c('allh'))

#write.csv(ryti_bk, 'Output/Ryti_BTVs.csv')

# Use this to filter out SSLs later
btv_params <- ryti_bk$mg.kg.units

rm(ryti_bk_raw)
#View(ryti_bk)

#####
# Combine ryti background with analytical
#####

soil_inorganic_bv <- inner_join(soil_raw, ryti_bk, by=c("parameter.code"="parameter.code", "parameter.name" = "mg.kg.units", "report.units"="report.units")) %>%
  spread(key = key, value = value) %>%
  mutate(allh.ratio = report.result/allh)
soil_inorganic_bv$allh = signif(soil_inorganic_bv$allh,3)
soil_inorganic_bv$allh.ratio = signif(soil_inorganic_bv$allh.ratio,3)
#View(soil_inorganic_bv)

#####
# Produce BV Inorganic Frequency-Detect tables
#####


fd_summary_allh <- soil_inorganic_bv %>%
  filter(!is.na(allh.ratio)) %>%
  filter(parameter.name %in% standards$Parameter | str_detect(parameter.name, 'PCB-') | str_detect(parameter.name, 'Aroclor') | parameter.name == 'Chromium hexavalent ion') %>% # called 'Chromium VI' in standards
  group_by(sma.number, parameter.code, parameter.name, detect.flag, allh, report.units) %>%
  summarise(
    max_result = max(report.result),
    max_location = paste(unique(location.id[which(report.result == max(report.result))]), collapse = "; "),
    max_date = paste(unique(sample.date[which(report.result == max(report.result))]), collapse = "; "),
    count = n(),
    count_gt_bv = length(report.result[allh.ratio > 1]),
    count_lt_bv = length(report.result[allh.ratio <= 1])) %>%
  mutate(screen.type = 'BTV')
  

View(fd_summary_allh)

#####
# Import Residential SSLs
#####
residential_ssl <- read_csv("../../SIP/SIP_soil_screening/Data/RESIDENTIAL SSL_03_25_2020.csv")
names(residential_ssl) %<>% tolower
names(residential_ssl) <- make.names(names(residential_ssl), unique=TRUE)
residential_ssl <- residential_ssl %>% 
  select(parameter.name, parameter.code, upper.limit, units) %>%
  filter(!parameter.name %in% btv_params) %>%
  mutate(SSL_ten_p = upper.limit*0.1) %>%
  unique()

#View(residential_ssl)

#####
# Combine Residential SSLs with analytical
#####

soil_SSL <- inner_join(soil_raw, residential_ssl, by=c("parameter.code"="parameter.code","parameter.name"="parameter.name","report.units"="units")) %>%
  mutate(ssl_10.ratio = report.result/SSL_ten_p) 
soil_SSL$upper.limit = signif(soil_SSL$upper.limit,3)
soil_SSL$SSL_ten_p = signif(soil_SSL$SSL_ten_p,3)
soil_SSL$ssl_10.ratio = signif(soil_SSL$ssl_10.ratio, 3)
#View(soil_SSL)


#####
# Produce SSL Frequency-Detect tables
#####

fd_summary_ssl <- soil_SSL %>%
  filter(!is.na(ssl_10.ratio)) %>%
  filter(parameter.name %in% standards$Parameter | str_detect(parameter.name, 'PCB-') | str_detect(parameter.name, 'Aroclor') | parameter.name == 'Chromium hexavalent ion') %>% # called 'Chromium VI' in standards
  group_by(sma.number, parameter.code, parameter.name, detect.flag, upper.limit, SSL_ten_p, report.units) %>%
  summarise(
    max_result = max(report.result),
    max_location = paste(unique(location.id[which(report.result == max(report.result))]), collapse = "; "),
    max_date = paste(unique(sample.date[which(report.result == max(report.result))]), collapse = "; "),
    count = n(),
    count_gt_0.1ssl = length(report.result[ssl_10.ratio > 1]),
    count_lt_0.1ssl = length(report.result[ssl_10.ratio <= 1])) %>%
  mutate(screen.type = 'SSL_0.1')

View(fd_summary_ssl)
#write_excel_csv(fd_summary_ssl, "../table/fd_ssl_summary_03_25_2020.csv")

#####
# Combine SSL and BV summaries
#####

fd_summary <- full_join(fd_summary_allh, fd_summary_ssl, by=c("sma.number", "parameter.code","parameter.name", 
                                                              "detect.flag","max_result", "max_location", "max_date", "report.units", "screen.type",
                                                              "count", "allh" = "SSL_ten_p", "count_lt_bv" = "count_lt_0.1ssl",
                                                              "count_gt_bv" = "count_gt_0.1ssl"))
fd_summary <- fd_summary %>%
  arrange(sma.number, parameter.code, detect.flag)
names(fd_summary)
# rename columns
names(fd_summary) <- c("sma.number","parameter.code","parameter.name", "detect.flag","screening.level","units","max_result", 
                       "max_location", "max_date", "total.count","count.results.gt.screening.level",
                       "count.results.lt.screening.level", "screen.type", "upper.limit")

# rearrange columns
fd_summary <- fd_summary[c("sma.number","parameter.code","parameter.name","detect.flag", "screen.type", 
                           "max_result", "max_date", "units", "max_location", "screening.level", "total.count",
                           "count.results.lt.screening.level", "count.results.gt.screening.level")]

# Add tier classifications
fd_summary <- fd_summary %>%
  mutate(soil_tier = 
           case_when(
             (detect.flag == 'Y' & count.results.gt.screening.level == 0) ~ 'Tier 1',
             (detect.flag == 'Y' & count.results.gt.screening.level > 0) ~ 'Tier 2', # Note: only 2 tiers for soil
             (detect.flag == 'N') ~ 'ND'
           ))

View(fd_summary)

# This doesn't seem to do anything... 
# fd_summary$screening.level <- signif(fd_summary$screening.level, 3)

write_excel_csv(fd_summary, "Output/FD_Summary_Soils_w_Tiers_WQSonly_20201007.csv")

########################################################
# Create tables for SIP documents
########################################################

# Max locations exceeding standard - this is the input file for the make_soil_table function
# Only exceedances are included in tables in SIP document
max_locations <- fd_summary %>%
  filter(count.results.gt.screening.level > 0, detect.flag == 'Y') %>% # only include exceedances (eg, > screening level and detected)
  select(sma.number, parameter.name, screen.type, screening.level, max_result, max_date, max_location)


# write_excel_csv(max_locations, 'Tables/max_locations.csv')

# Create a table for each SMA
smas <- unique(fd_summary$sma.number)

# Create and save soil table
######################################3
# This function deals with sigfigs - ensures 3 sigfigs are reported, but in the case that the last sigfig
# is a zero before a decimal, it drops the decimal so there is not something like "250."
sigfig <- function(vec, digits){
  return(gsub("\\.$", "", formatC(signif(vec,digits=digits), digits=digits, format="fg", flag="#")))
}

make_soil_table <- function(df, location){
  location <- enquo(location)
  # Soil data for specified SMA
  table <- df %>%
    filter(sma.number == !! location) %>%
    select(parameter.name, screen.type, screening.level, max_result, max_date, max_location) %>%
    mutate(screening.level = sigfig(screening.level, 3),
           max_result = sigfig(max_result, 3)) %>%
    # mutate(screening.level = sprintf('%#0.3g', screening.level),
    # max_result = sprintf('%#0.3g', max_result)) %>%
    rename(`Screening Type` = screen.type, `Screening Level (mg/kg)` = screening.level,
           `Max Result (mg/kg)` = max_result, `Date of Max Result` = max_date, `Location of Max Result` = max_location) %>%
    arrange(parameter.name) %>%
    remove_rownames() %>%
    column_to_rownames(var = 'parameter.name') 
  return(table)
}

# Hmmm... numbers show up in scientific notation with 3 sigfigs in max_locations, but output with variable numbers of sigfigs in make_soil_table
# Leave it for now and check in on where we stand with scientific notation.
test <- make_soil_table(max_locations, 'CDV-SMA-1.4')

# Function to export tables in nice format to correct folder
save_soil_table <- function(sma){
  tmp <- make_soil_table(max_locations, location = sma)
  # Don't make a table if there are no data...
  # Note: if there are no data for an SMA in 'smas' it does not mean there are no soil data, just that there are no exceedances
  if (nrow(tmp) > 0){
    # These next if/else statements adjust table size based on number of rows
    if (nrow(tmp) >= 5) {
      bmp(paste0('../../SIP/SIP_soil_screening/Output/', sma, '/', sma, '_soil_exceedance_table', '.bmp'), height = 30*nrow(tmp), width = 200*ncol(tmp))
      table <- tableGrob(tmp)
      title <- textGrob(sma, gp = gpar(fontsize = 24))
      padding <- unit(0.5, 'line')
      table <- gtable_add_rows(table, heights = grobHeight(title) + padding, pos = 0)
      table <- gtable_add_grob(
        table, list(title),
        t = 1, l = 1, r = ncol(table)
      )
      grid.draw(table)
      dev.off()
    } else {
      bmp(paste0('../../SIP/SIP_soil_screening/Output/', sma, '/', sma, '_soil_exceedance_table', '.bmp'), height = 80*nrow(tmp), width = 200*ncol(tmp))
      table <- tableGrob(tmp)
      title <- textGrob(sma, gp = gpar(fontsize = 24))
      padding <- unit(0.5, 'line')
      table <- gtable_add_rows(table, heights = grobHeight(title) + padding, pos = 0)
      table <- gtable_add_grob(
        table, list(title),
        t = 1, l = 1, r = ncol(table)
      )
      grid.draw(table)
      dev.off()
    }
  }
}


# Loop through all SMAs to save to corresponding folders

save_soil_table('CDV-SMA-1.4')

for (location in smas) {
  save_soil_table(location)
}


######################################################
# Prepare the comparison of results to UTL and SSL   #  
######################################################

sma_site <- read_excel("Data/new_SMA_assoc_table.xlsx", col_names = TRUE)
sma_site$activity_end_date <- as_date(sma_site$activity_end_date)
sma_site$activity_start_date <- as_date(sma_site$activity_start_date)
names(sma_site) %<>% tolower
names(sma_site) <- make.names(names(sma_site), unique=TRUE)

sma_site <- sma_site %>%
  filter(monitoring_allowed_flag == 'Y') %>%
  mutate(activity_end_date = if_else(is.na(activity_end_date), Sys.Date(), activity_end_date)) %>%
  group_by(sma_number) %>%
  summarize(sw_stage = sw_stage[which.max(activity_end_date)])
names(sma_site) %<>%
  str_replace_all("_", ".")

View(sma_site)

write_excel_csv(sma_site, 'Output/current_SMA_stages.csv')

#####
# Join sma_site with the fd inorganic summary for BVs and the
#####
sma_fd_summary <- left_join(sma_site, fd_summary, by = c("sma.number")) 
sma_fd_summary <- sma_fd_summary %>%
  arrange(sma.number,parameter.code) 
View(sma_fd_summary)

write_excel_csv(sma_fd_summary, 'Output/SMA_FD_summary_w_locs_SMA_grouped_06292020.csv')


