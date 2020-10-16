# Screen all the IP data
# This expands on copper_BLM_hardness_dep_comparison_V2.R
# Rather than just focusing on SMAs where copper is T1, classify all SMAs that have sampled
# AMC, 2020-08
# 2020-10: Code has been modified to produce two different spreadsheets - one includes all soil exceedances, 
# while the other only contains exceedances for parameters that have WQSs; this moves some Tier 0 SMAs to other Tiers

library(tidyverse)
library(lubridate)
library(writexl)

# Import data
#############################################

# Stormwater screened data - these are screened using hardness dependent standards
sw <- read_csv('../../SIP/SIP_soil_screening/Data/SW_data_sigfigUpdate101520.csv',
                col_types = list('sample_date' = col_date('%m/%d/%Y')))

# Watershed associations for IP sites
driosmas <- read_csv("../../Site_specific/Site_specific_evaluation/Data/dRioSMAs.csv")
# A-SMA-6 has two entries - one of them appears to be bad (says drio is 0.00087 km) - delete this to remove duplicates
driosmas <- driosmas %>%
  filter(drio_km > 0.5) %>%
  select(-2) # don't need distance to Rio here

# Soil screened data
soil <- read_csv('../../SIP/SIP_soil_screening/Output/FD_Summary_Soils_w_Tiers_202010115.csv',
                 col_types = list('max_date' = col_date("%Y-%m-%d"))) %>%
  select(sma.number, parameter.name, detect.flag, screen.type, soil_tier) %>%
  # Just want one result per parameter per SMA (eg, not a detect and non-detect)
  # If there is a detect, keep that one, if not, keep ND
  group_by(sma.number, parameter.name) %>%
  mutate(sample_n = n()) %>%
  filter(!(sample_n == 2 & detect.flag == 'N')) %>%
  ungroup() %>%
  select(1:5)

# List of all active SMAs and their compliance stage
#all_sma_stage <- read_csv('Data/current_SMA_stages.csv')
# I think we don't need to include these data for this part. 
# Makes more sense to just focus on the SMAs with data (185 of them)

# List of sites that have collected samples
sample_locs <- unique(sw$location_id_alias) # 185 samplers out of 250

#############################################
# Join SMA watershed associations to SW exceedance data
# This is so that watershed specific BLM standards can be joined to the data
sw_combo <- sw %>%
  left_join(driosmas, by = 'location_id_alias') %>%
  mutate(watershed = case_when(
    watershed == 'Los Alamos' | watershed == 'Pueblo' ~ 'Los Alamos/Pueblo',
    watershed == 'Water' ~ 'Water/Canon de Valle',
    TRUE ~ as.character(watershed))
  ) %>%
  # These lines pick out just the max sample result in the current stage 
  group_by(location_id_alias, parameter_name, field_preparation_code) %>%
  slice(which.max(new_report_result)) # This is based on new_report_result - this is important to use here; sometimes ND have high report_result than Detects; new_report_result corrects for this
# Note: new_report_result is the same as report_result for detected samples - nondetects become 1/2(method_detection_limit)
# Gross alpha and radium 226/228 are SSC normalized, but earlier samples don't have SSC data

# TAL exceedances and BTV exceedances are calculated using new_report_result

# new_report_result_ssc are used for gross alpha and radium 226/228 for BTV/Exceedance calculation, 
# but are not really important to distiguish here because we'll just need the exceedance flag info

##################################################################
# Classify into tiers for all_sw 
all_sw <- sw_combo %>%
  arrange(location_id_alias, parameter_name, field_preparation_code, detect_flag) %>%
  # Note: anything that does not have a TAL will have an NA for tier
  mutate(HD.tier = case_when(
    (detect_flag == 'Y' & TAL_exceed_flag == 'N') ~ 'Tier 1',
    (detect_flag == 'Y' & TAL_exceed_flag == 'Y' & compBTV_exceed_flag == 'N') ~ 'Tier 2',
    (detect_flag == 'Y' & TAL_exceed_flag == 'Y' & compBTV_exceed_flag == 'Y') ~ 'Tier 3',
    (detect_flag == 'Y' & TAL_exceed_flag == 'N' & compBTV_exceed_flag == 'Y') ~ 'Tier 1',
    (detect_flag == 'Y' & TAL_exceed_flag == 'Y' & is.na(compBTV_exceed_flag) ~ 'Tier 3'), 
    (detect_flag == 'N' & !is.na(TAL_exceed_flag)) ~ 'Tier 1', # If there is a TAL, but ND, will be considered Tier 1 if it exceeds TAL flag or not
    (is.na(TAL_exceed_flag)) ~ 'No TAL'
    #is.na(parameter_name) ~ 'Tier 0' # implies there has not been a sample - leave this out for now; any locations that have soil data but not SW data will be added when soil are joined
  )) 


# Confirm there are no NAs in the HD tier column
if (sum(is.na(all_sw$HD.tier)) == 0) {
  print('Lookin good!')
} else {
  print('NAs present; check tier classifications!')
}


#####################################################
# add soil classifications and finalize HD and BLM tiers
# If something exceeded in soil (eg soil_tier == 'Tier 2'), but was not monitored in SW, eg(HD or BLM.tier == NA)
# Note, there are 71 out of 185 SMAs with SW data but no accompanying soil data (and 48 SMAs with soil data but no SW sample)
all_sw_soil <- all_sw %>%
  full_join(soil, by = c('location_id_alias' = 'sma.number', 'parameter_name' = 'parameter.name')) %>%
  mutate(HD.tier.final = ifelse(is.na(HD.tier) & soil_tier == 'Tier 2', 'Tier 0', HD.tier)) %>% # there are no NAs in HD tiers columns so anything with an NA in this df is something that was measured in soil but not SW
  filter(!((soil_tier == 'Tier 1' | soil_tier == 'ND') & (is.na(HD.tier.final)))) %>% # can exclude all soil data for parameters not measured in SW that are T1 or ND
  filter(location_id_alias %in% sample_locs) # only include SMAs that have collected a sample for the next part

# Which SMAs do not have soil data?
SW_smas <- as_tibble(unique(all_sw$location_id_alias))
soil_smas <- as_tibble(unique(soil$sma.number))

just_SW_smas <- anti_join(SW_smas, soil_smas)
rm(SW_smas, soil_smas)
####################################################
# List of analytes in each tier for hardness dependent
HD_tiers <- all_sw_soil %>%
  group_by(location_id_alias, HD.tier.final) %>%
  summarize(exceedance_n = n(), parameter_exceedances = paste(parameter_name, collapse = ", ")) %>%
  mutate(soil_data = ifelse(location_id_alias %in% just_SW_smas$value, 'N', 'Y'))

# Just out of curiousity, see what results would be if we didn't include soil
# HD_tiers_no_soil <- all_sw %>%
#   group_by(location_id_alias, HD.tier) %>%
#   summarize(exceedance_n = n(), parameter_exceedances = paste(parameter_name, collapse = ", "))
# 
# HD_tiers_wide_no_soil <- HD_tiers_no_soil %>%
#   select(1:2, 4) %>%
#   pivot_wider(names_from = HD.tier, values_from = parameter_exceedances) %>%
#   mutate(action =            
#            case_when(
#               (is.na(`Tier 2`) & is.na(`Tier 3`)) ~ 'Remove site',
#               (is.na(`Tier 3`) & !is.na(`Tier 2`)) ~ 'LTS',
#               (`Tier 3` == 'Gross alpha') ~ 'LTS',
#               TRUE ~ 'Corrective Action'
#             ))
            
# table(HD_tiers_wide_no_soil$action)
# Corrective Action               LTS       Remove site 
#               99                70                16 

HD_tiers_wide <- HD_tiers %>% 
  select(1:2, 4:5) %>%
  pivot_wider(names_from = HD.tier.final, values_from = parameter_exceedances) %>%
  mutate(action = 
           case_when(
             (is.na(`Tier 0`) & is.na(`Tier 2`) & is.na(`Tier 3`) & soil_data == 'Y') ~ 'Remove site',
             (is.na(`Tier 0`) & is.na(`Tier 2`) & is.na(`Tier 3`) & soil_data == 'N') ~ 'Remove site, pending soil data',
             (is.na(`Tier 3`) & !is.na(`Tier 2`) & is.na(`Tier 0`) & soil_data == 'Y') ~ 'LTS',
             (is.na(`Tier 3`) & !is.na(`Tier 2`) & is.na(`Tier 0`) & soil_data == 'N') ~ 'LTS, pending soil data',
             (`Tier 3` == 'Gross alpha' & is.na(`Tier 0`) & soil_data == 'Y') ~ 'LTS',
             (`Tier 3` == 'Gross alpha' & is.na(`Tier 0`) & soil_data == 'N') ~ 'LTS, pending soil data',
             (!is.na(`Tier 0`)) ~ 'Tier 0',
             (!is.na('Tier 3') & `Tier 3` != 'Gross alpha' & soil_data == 'Y') ~ 'Corrective Action',
             TRUE ~ 'Corrective Action, pending soil data'
           )) %>%
  mutate(action_wo_soil = 
           case_when(
             is.na(`Tier 2`) & is.na(`Tier 3`) ~ 'SW Tier 1',
             !is.na(`Tier 2`) & (is.na(`Tier 3`)|`Tier 3` == 'Gross alpha') ~ 'SW Tier 2',
             is.na(`Tier 2`) & `Tier 3` == 'Gross alpha' ~ 'SW Tier 2', 
             TRUE ~ 'SW Tier 3'
           ))

#write_xlsx(HD_tiers_wide, 'Output/Tables/HD_tiers_and_recs_new_copper.xlsx')

# Before getting the breakdown of tiers, 
# look at excel file and identify any SMAs with just aroclor T0 where we were monitoring PCBs - these should not be T0


##########################################
# Prep data for maps
# Only include SMAs with stormwater samples here
# Go through xlsx files exported above, and pick out SMAs where the only Tier 0 analytes
# are Aroclors; if PCBs were monitored in SW, the Aroclors don't count as something we 
# didn't monitor in SW; correct Tier accordingly
# For the BLM and HD xlsx files, the following corrections need to be made:
# A-SMA-3 becomes Tier 3 (corrective action)
# S-SMA-0.25 becomes Tier 3 (corrective action)
# S-SMA-3.72 becomes Tier 1 (remove site)

HD_tiers_wide <- HD_tiers_wide %>%
  mutate(action = 
           case_when(
             location_id_alias == 'A-SMA-3' ~ 'Corrective Action',
             location_id_alias == 'S-SMA-0.25' ~ 'Corrective Action',
             location_id_alias == 'S-SMA-3.72' ~ 'Remove site',
             TRUE ~ as.character(action)
           ))

table(HD_tiers_wide$action)


# These will be the data files for the maps
#write_excel_csv(HD_tiers_wide, 'Output/soil_screen_for_maps.csv')

###########################################################
# Add in soil data from SMAs where SW samples have not been collected yet
# THere are 48 SMAs with soil data but no SW data (162 SMAs total with soil data)
soil_no_SW <- all_sw %>%
  full_join(soil, by = c('location_id_alias' = 'sma.number', 'parameter_name' = 'parameter.name')) %>%
  ungroup() %>%
  filter(!(location_id_alias %in% sample_locs)) %>%
  select(1,2,28) %>%
  group_by(location_id_alias, soil_tier) %>%
  summarize(exceedance_n = n(), parameter_exceedances = paste(parameter_name, collapse = ", "))
  
soil_no_SW_wide <- soil_no_SW %>%
  select(1:2, 4) %>%
  pivot_wider(names_from = soil_tier, values_from = parameter_exceedances)
  # Note, parameters in Tier 2 would be considered something in Tier 0 when
  # combining with stormwater data; eg, these are soil exceedances that we need to 
  # make sure we are monitoring for in SW

# Build a df with Tier 2 and location name with fillers for other columns

no_SW_soil_POCs <- tibble(
                      location_id_alias = soil_no_SW_wide$location_id_alias,
                      soil_data = 'Y',
                      `No TAL` = NA,
                      `Tier 0` = soil_no_SW_wide$`Tier 2`,
                      `Tier 1` = NA,
                      `Tier 2` = NA, 
                      `Tier 3` = NA,
                      `action` = 'Soil data only',
                      `action_wo_soil` = NA)

# Join soil data to HD_tiers_wide 
HD_tiers_wide_w_soil <- HD_tiers_wide %>% bind_rows(no_SW_soil_POCs) %>%
  arrange(location_id_alias)

# Add in SMAs with no SW or soil data
no_data_sma <- anti_join(driosmas['location_id_alias'], HD_tiers_wide_w_soil['location_id_alias']) 
  
no_data_sma_df <- tibble(
                    location_id_alias = no_data_sma$location_id_alias,
                    soil_data = 'N',
                    `No TAL` = NA,
                    `Tier 0` = NA,
                    `Tier 1` = NA,
                    `Tier 2` = NA, 
                    `Tier 3` = NA,
                    `action` = 'Tier 0',
                    `action_wo_soil` = NA)

# Join to HD_tiers_wide_w_soil to have complete list of 250 SMAs
HD_tiers_wide_all_SMAs <- HD_tiers_wide_w_soil %>% bind_rows(no_data_sma_df) %>%
  arrange(location_id_alias)

table(HD_tiers_wide_all_SMAs$action)

write_xlsx(HD_tiers_wide_all_SMAs, 'Output/soil_screen.xlsx')

############################################################
############################################################
# Some of the analytes listed in Tier 0 do not have a water quality standard

# In this version, filter those out and see how many SMAs are changed from Tier 0 to something else

# Load list of parameters with WQS or TAL
standards <- read_csv('../../SIP/SIP_soil_screening/Data/all_water_qual_parameters.csv')

soil_wqs <- soil %>% 
  filter(parameter.name %in% standards$Parameter | str_detect(parameter.name, 'PCB-') | str_detect(parameter.name, 'Aroclor') | parameter.name == 'Chromium hexavalent ion') # called 'Chromium VI' in standards

all_sw_soil_wqs <- all_sw %>%
  full_join(soil_wqs, by = c('location_id_alias' = 'sma.number', 'parameter_name' = 'parameter.name')) %>%
  mutate(HD.tier.final = ifelse(is.na(HD.tier) & soil_tier == 'Tier 2', 'Tier 0', HD.tier)) %>% # there are no NAs in HD tiers columns so anything with an NA in this df is something that was measured in soil but not SW
  filter(!((soil_tier == 'Tier 1' | soil_tier == 'ND') & (is.na(HD.tier.final)))) %>% # can exclude all soil data for parameters not measured in SW that are T1 or ND
  filter(location_id_alias %in% sample_locs) # only include SMAs that have collected a sample for the next part

HD_tiers_wqs <- all_sw_soil_wqs %>%
  group_by(location_id_alias, HD.tier.final) %>%
  summarize(exceedance_n = n(), parameter_exceedances = paste(parameter_name, collapse = ", ")) %>%
  mutate(soil_data = ifelse(location_id_alias %in% just_SW_smas$value, 'N', 'Y'))

HD_tiers_wide_wqs <- HD_tiers_wqs %>% 
  select(1:2, 4:5) %>%
  pivot_wider(names_from = HD.tier.final, values_from = parameter_exceedances) %>%
  mutate(action = 
           case_when(
             (is.na(`Tier 0`) & is.na(`Tier 2`) & is.na(`Tier 3`) & soil_data == 'Y') ~ 'Remove site',
             (is.na(`Tier 0`) & is.na(`Tier 2`) & is.na(`Tier 3`) & soil_data == 'N') ~ 'Remove site, pending soil data',
             (is.na(`Tier 3`) & !is.na(`Tier 2`) & is.na(`Tier 0`) & soil_data == 'Y') ~ 'LTS',
             (is.na(`Tier 3`) & !is.na(`Tier 2`) & is.na(`Tier 0`) & soil_data == 'N') ~ 'LTS, pending soil data',
             (`Tier 3` == 'Gross alpha' & is.na(`Tier 0`) & soil_data == 'Y') ~ 'LTS',
             (`Tier 3` == 'Gross alpha' & is.na(`Tier 0`) & soil_data == 'N') ~ 'LTS, pending soil data',
             (!is.na(`Tier 0`)) ~ 'Tier 0',
             (!is.na('Tier 3') & `Tier 3` != 'Gross alpha' & soil_data == 'Y') ~ 'Corrective Action',
             TRUE ~ 'Corrective Action, pending soil data'
           )) %>%
  mutate(action_wo_soil = 
           case_when(
             is.na(`Tier 2`) & is.na(`Tier 3`) ~ 'SW Tier 1',
             !is.na(`Tier 2`) & (is.na(`Tier 3`)|`Tier 3` == 'Gross alpha') ~ 'SW Tier 2',
             is.na(`Tier 2`) & `Tier 3` == 'Gross alpha' ~ 'SW Tier 2', 
             TRUE ~ 'SW Tier 3'
           ))

HD_tiers_wide_wqs <- HD_tiers_wide_wqs %>%
  mutate(action = 
           case_when(
             location_id_alias == 'A-SMA-3' ~ 'Corrective Action',
             location_id_alias == 'S-SMA-0.25' ~ 'Corrective Action',
             location_id_alias == 'S-SMA-3.72' ~ 'Remove site',
             TRUE ~ as.character(action)
           ))

# This version of just the soil exceedances only includes parameters that exceeded that have WQSs
###################################################
# Add in soil data from SMAs where SW samples have not been collected yet
# THere are 48 SMAs with soil data but no SW data (162 SMAs total with soil data)
soil_no_SW_wqs <- all_sw %>%
  full_join(soil, by = c('location_id_alias' = 'sma.number', 'parameter_name' = 'parameter.name')) %>%
  ungroup() %>%
  filter(!(location_id_alias %in% sample_locs)) %>%
  filter(parameter_name %in% standards$Parameter | str_detect(parameter_name, 'PCB-') | str_detect(parameter_name, 'Aroclor') | parameter_name == 'Chromium hexavalent ion') %>% # called 'Chromium VI' in standards
  select(1,2,28) %>%
  group_by(location_id_alias, soil_tier) %>%
  summarize(exceedance_n = n(), parameter_exceedances = paste(parameter_name, collapse = ", "))

soil_no_SW_wide_wqs <- soil_no_SW_wqs %>%
  select(1:2, 4) %>%
  pivot_wider(names_from = soil_tier, values_from = parameter_exceedances)
# Note, parameters in Tier 2 would be considered something in Tier 0 when
# combining with stormwater data; eg, these are soil exceedances that we need to 
# make sure we are monitoring for in SW

# Build a df with Tier 2 and location name with fillers for other columns

no_SW_soil_POCs_wqs <- tibble(
  location_id_alias = soil_no_SW_wide_wqs$location_id_alias,
  soil_data = 'Y',
  `No TAL` = NA,
  `Tier 0` = soil_no_SW_wide_wqs$`Tier 2`,
  `Tier 1` = NA,
  `Tier 2` = NA, 
  `Tier 3` = NA,
  `action` = 'Soil data only',
  `action_wo_soil` = NA)

# Join soil data and SMAs with no data to HD_tiers_wide_wqs 
HD_tiers_wide_wqs_all_SMAs <- HD_tiers_wide_wqs %>% bind_rows(no_SW_soil_POCs_wqs, no_data_sma_df) %>%
  arrange(location_id_alias)

table(HD_tiers_wide_wqs_all_SMAs$action)

write_xlsx(HD_tiers_wide_wqs_all_SMAs, 'Output/soil_screen_WQS.xlsx')
