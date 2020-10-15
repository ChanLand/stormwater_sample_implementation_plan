# SIP soil plot program
# AMC 07-2020
# V2: Makes non-detects open circles, distinguishes exceedences with blue x-axis labels
# Exceedances only count as samples that are detected and exceed BTV or 10% SSL (whichever standard applies)
# VFinal - cleaned up version

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

if(!require(RColorBrewer)){
  install.packages("RColorBrewer"); require(RColorBrewer)}

if(!require(scales)){
  install.packages("scales"); require(scales)}

# Import data - two files because combined file size is greater than the 500K records allowed by EIM
soil_raw_detect <- read_csv("../../SIP/SIP_soil_screening/Data/Soil_Detect_2000-2020_10_15_2020.csv",
                            col_types = list('sample_date' = col_date('%m/%d/%Y')))
names(soil_raw_detect) %<>% tolower
names(soil_raw_detect) <- make.names(names(soil_raw_detect), unique=TRUE)

soil_raw_nondetect <- read_csv("../../SIP/SIP_soil_screening/Data/Soil_Nondetect_2000-2020_10_15_2020.csv",
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

################################################
# Make SSL plots
#################################################

# Import SSLs
residential_ssl <- read_csv("../../SIP/SIP_soil_screening/Data/RESIDENTIAL SSL_03_25_2020.csv")
names(residential_ssl) %<>% tolower
names(residential_ssl) <- make.names(names(residential_ssl), unique=TRUE)
residential_ssl <- residential_ssl %>% 
  select(parameter.name, parameter.code, upper.limit, units) %>%
  filter(!parameter.name %in% btv_params) %>%
  mutate(SSL_ten_p = upper.limit*0.1) %>%
  unique()

# Inner join SSLs with soil data
soil_SSL <- inner_join(soil_raw, residential_ssl, by=c("parameter.code"="parameter.code","parameter.name"="parameter.name","report.units"="units")) %>%
  mutate(ssl_10.ratio = report.result/SSL_ten_p) %>%
  filter(parameter.name %in% standards$Parameter | str_detect(parameter.name, 'PCB-') | str_detect(parameter.name, 'Aroclor') | parameter.name == 'Chromium hexavalent ion') # called 'Chromium VI' in standards)
  
soil_SSL$upper.limit = signif(soil_SSL$upper.limit,3)
soil_SSL$SSL_ten_p = signif(soil_SSL$SSL_ten_p,3)
soil_SSL$ssl_10.ratio = signif(soil_SSL$ssl_10.ratio, 3)

# soil_SSL has all the exceedance ratio data for all the SMAs
# It's probability not practical, memory-wise, to loop through all the SMAs; it's unlikely that all plots will need to be made at once anyway

# Function from https://stackoverflow.com/questions/14255533/pretty-ticks-for-log-normal-scale-using-ggplot2-dynamic-not-manual/22227846
# This makes breaks at every factor of 10
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

# Function to generate plot
# Needs some fine tuning with axis ranges, etc., but works pretty well!
plot_ssl <- function(df, exceed_table){
  ggplot(df, aes(parameter.name, ssl_10.ratio)) +
    geom_jitter(aes(color = as.factor(year(sample.date)), shape = detect.flag), alpha = 0.6, width = 0.3) + 
    scale_color_brewer(palette = 'Set1') +
    scale_shape_manual(name = 'Detected', 
                       labels = c('No', 'Yes'),
                       values = c(1, 16)) +
    geom_boxplot(fatten = 1.5, outlier.shape = NA, alpha =0.1) + 
    scale_y_log10(breaks = base_breaks(), 
                  labels = prettyNum) +
    annotation_logticks(sides = "lr") +
    geom_hline(yintercept = 1, linetype = 'dotted') +
    theme_bw() +
    labs(x = NULL, y = 'Exceedance Ratio (Result/10% SSL)', color = "Sample Year", title = df$sma.number) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.y = element_text(vjust = 1),
          axis.text.x = element_text(color = ifelse((exceed_table$ssl_10.ratio > 1 & exceed_table$detect.flag == 'Y'), "blue", "black"), 
                                     face = 'bold', size = 10, angle = 40, hjust = 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 1.75), 'cm'))
}


# Create a function that takes the SMA name as an input
sma_ssl_plot <- function(df, location){
  location <- enquo(location)
  
  # all soil data for specified SMA
  sample <- df %>%
    filter(sma.number == !! location)
  
  # List of soil parameters measured at specified SMA
  soil_SSL_params <- sort(unique(sample$parameter.name))
  
  plot_list = list()
  for (i in 1:ceiling(length(soil_SSL_params)/20)){ # 20 parameters per plot
    j <- seq(1, length(soil_SSL_params), 20) 
    if (!is.na(j[i+1])){ # not the last element of j
      
      plotdat <- sample %>%
        filter(parameter.name %in% soil_SSL_params[j[i]:(j[i+1]-1)])
      
      # Exceedance max value for use in plot_ssl function
      exceed_table <- plotdat %>%
        group_by(parameter.name, detect.flag) %>%
        summarize(ssl_10.ratio = max(ssl_10.ratio)) %>%
        mutate(number_group = n()) %>%
        filter(!(detect.flag == 'N' & number_group == 2))
      
      p <- plot_ssl(plotdat, exceed_table)
    }
    else {
      plotdat <- sample %>%
        filter(parameter.name %in% soil_SSL_params[j[i]:length(soil_SSL_params)])
      
      # Exceedance max value for use in plot_ssl function
      exceed_table <- plotdat %>%
        group_by(parameter.name, detect.flag) %>%
        summarize(ssl_10.ratio = max(ssl_10.ratio)) %>%
        mutate(number_group = n()) %>%
        filter(!(detect.flag == 'N' & number_group == 2))
      
      p <- plot_ssl(plotdat, exceed_table)
      
    }
    
    plot_list[[i]] <- p
  }
  return(plot_list)
}

# generate SSL plots for specified location

# Function to output plots into correct folders
save_ssl_plots <- function(sma){
  
  plots <- sma_ssl_plot(soil_SSL, location = sma)
  
  for (i in 1:length(plots)) {
    ggsave(plots[[i]], filename = paste0('../../SIP/SIP_soil_screening/Output/', sma, '/', sma, '_', 'SSL_plot', '_', i, '.bmp'), height = 8, width = 10)
  }
}


# loop through locations to make all plots
ssl_locations_all <- unique(soil_SSL$sma.number)
# Break into chunks because poor computer doesn't have enough space for all the plots... 
ssl_locations_1 <- ssl_locations_all[1:40]
ssl_locations_2 <- ssl_locations_all[41:80]
ssl_locations_3 <- ssl_locations_all[81:120]
ssl_locations_4 <- ssl_locations_all[121:159]


for (location in ssl_locations_all) {
  save_ssl_plots(location)
}

##############################################
# Make BTV plots
##############################################

soil_inorganic_bv <- inner_join(soil_raw, ryti_bk, by=c("parameter.code"="parameter.code", "parameter.name" = "mg.kg.units", "report.units"="report.units")) %>%
  spread(key = key, value = value) %>%
  mutate(allh.ratio = report.result/allh) %>%
  filter(parameter.name %in% standards$Parameter | str_detect(parameter.name, 'PCB-') | str_detect(parameter.name, 'Aroclor') | parameter.name == 'Chromium hexavalent ion') # called 'Chromium VI' in standards)

  
soil_inorganic_bv$allh = signif(soil_inorganic_bv$allh,3)
soil_inorganic_bv$allh.ratio = signif(soil_inorganic_bv$allh.ratio,3)

plot_btv <- function(df, exceed_table){
  ggplot(df, aes(parameter.name, allh.ratio)) +
    geom_jitter(aes(color = as.factor(year(sample.date)), shape = detect.flag), alpha = 0.6, width = 0.3) + 
    scale_color_brewer(palette = 'Set1') +
    scale_shape_manual(name = 'Detected', 
                       labels = c('No', 'Yes'),
                       values = c(1, 16)) +
    geom_boxplot(fatten = 1.5, outlier.shape = NA, alpha =0.1) + 
    scale_y_log10(breaks = base_breaks(), 
                  labels = prettyNum) +
    annotation_logticks(sides = "lr") +
    geom_hline(yintercept = 1, linetype = 'dotted') +
    theme_bw() +
    labs(x = NULL, y = 'Exceedance Ratio (Result/BTV)', color = "Sample Year", title = df$sma.number) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(color = ifelse((exceed_table$allh.ratio > 1 & exceed_table$detect.flag == 'Y'), "blue", "black"), 
                                     face = 'bold', size = 10, angle = 40, hjust = 1),
          plot.margin = unit(c(0.5, 0.5, 0.5, 1.75), 'cm'))
}

# Create a function that takes the SMA name as an input
sma_btv_plot <- function(df, location){
  location <- enquo(location)
  # Soil data for specified SMA
  sample <- df %>%
    filter(sma.number == !! location)
  # Exceedance max value for use in plot_ssl function
  exceed_table <- sample %>%
    group_by(parameter.name, detect.flag) %>%
    summarize(allh.ratio = max(allh.ratio)) %>%
    mutate(number_group = n()) %>%
    filter(!(detect.flag == 'N' & number_group == 2))
  # Make plot
  plot <- plot_btv(sample, exceed_table)
  
  return(plot)
}

# Function to output plots into correct folders
save_btv_plots <- function(sma){
  
  plot <- sma_btv_plot(soil_inorganic_bv, location = sma)
  
  ggsave(plot, filename = paste0('../../SIP/SIP_soil_screening/Output/', sma, '/', sma, '_', 'BTV_plot', '.bmp'), height = 8, width = 10)
  
}

# Loop through locations and save plots
btv_locations <- unique(soil_inorganic_bv$sma.number)

for (location in btv_locations) {
  save_btv_plots(location)
}
