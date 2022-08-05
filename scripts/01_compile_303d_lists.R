## CA-listed-waters
## 01. Compile 303(d) listed waters in California
## the base files were provided from a database accessed by the 303(d) team at the CA Water Boards in July 2022
## Issue #2 asks this team to provide instructions for how this data is accessed.


library(here)
library(tidyverse)
library(readxl)
library(janitor)



### 1. load Excel files
# list2002 <- read_excel(here("rawdata_xls", "2002-303d-list.xls"), sheet = 1) %>%
#   clean_names %>%
#   remove_empty(c("rows", "cols"))
# list2006 <- read_excel(here("rawdata_xls", "2006-303d-list.xls"), sheet = 1) %>%
#   clean_names %>%
#   remove_empty(c("rows", "cols"))
list2010 <- read_excel(here("rawdata_xls", "IR_Files", "RevisionCodeReport_2010.xlsx"), skip = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2012 <- read_excel(here("rawdata_xls", "IR_Files", "RevisionCodeReport_2012.xlsx"), skip = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2016 <- read_excel(here("rawdata_xls", "IR_Files", "RevisionCodeReport_2014-16.xlsx"), skip = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2018 <- read_excel(here("rawdata_xls", "IR_Files", "RevisionCodeReport_2018.xlsx")) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2022 <- read_excel(here("rawdata_xls", "IR_Files", "RevisionCodeReport_2020-2022.xlsx")) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))



### 2. clean columns among dfs: select columns, new column names, add year.

colnames(list2022)

# newnames <- c("water_body_id", "report_year", "regional_board_no",
#               "water_body_type", "pollutant", "subpollutant",
#               "water_body_name")

newnames <- c("report_year", "regional_board_no", "water_body_name", "water_body_id", "pollutant", "decision_id",
              "previous_cycle_poll_listing_decision", "current_cycle_poll_listing_decision")


# list2002e <- list2002 %>%
#   dplyr::mutate(report_year = 2002,
#                 subpollutant = NA) %>%
#   dplyr::select(wbid, report_year, regional_board_no, water_body_type, pollutant_stressor, subpollutant, water_body_name) %>%
#   setNames(.,newnames) %>%
#   distinct() ## removes duplicated rows due to separate, suspected source data 

# list2006e <- list2006 %>%
#   dplyr::mutate(report_year = 2006) %>%
#   dplyr::select(wbid, report_year, region_number, water_body_type, pollutant_category, pollutant, water_body_name) %>%
#   setNames(.,newnames) %>%
#   distinct() ## removes duplicated rows due to separate, suspected source data
  
list2010e <- list2010 %>%
  dplyr::mutate(report_year = 2010) %>%
  dplyr::select(report_year, region, water_body_name, water_body_id, decision_pollutant_s, decision_id, 
                previous_cycle_poll_listing_decision, current_cycle_poll_listing_decision) %>%
  setNames(.,newnames)

list2012e <- list2012 %>%
  dplyr::mutate(report_year = 2012) %>%
  dplyr::select(report_year, region, water_body_name, water_body_id, decision_pollutant_s, decision_id, 
                previous_cycle_poll_listing_decision, current_cycle_poll_listing_decision) %>%
  setNames(.,newnames)

list2016e <- list2016 %>%
  dplyr::mutate(report_year = 2016) %>%
  dplyr::select(report_year, region, water_body_name, water_body_id, decision_pollutant_s, decision_id, 
                previous_cycle_poll_listing_decision, current_cycle_poll_listing_decision) %>%
  setNames(.,newnames)

list2018e <- list2018 %>%
  dplyr::mutate(report_year = 2018) %>%
  dplyr::select(report_year, region, water_body_name, water_body_id, decision_pollutant_s, decision_id, 
                previous_cycle_poll_listing_decision, current_cycle_poll_listing_decision) %>%
  setNames(.,newnames)

list2022e <- list2022 %>%
  dplyr::mutate(report_year = 2022) %>%
  dplyr::select(report_year, region, water_body_name, water_body_id, decision_pollutant_s, decision_id, 
                previous_cycle_poll_listing_decision, current_cycle_poll_listing_decision) %>%
  setNames(.,newnames)



### 3. rowbind dfs of 303(d) lists
#### data types of cols should be: num, chr, chr, chr, chr, num, chr, chr
# compare_df_cols(list2002e, list2006e, list2010e, list2012e, list2016e, list2018e, list2022e, return = "mismatch")
compare_df_cols(list2010e, list2012e, list2016e, list2018e, list2022e, return = "mismatch")

#### change data types of mismatched cols -- none needed for 8/5/2022 commit
# list2002e$subpollutant <- as.character(list2002e$subpollutant)
# list2002e$regional_board_no <- as.numeric(list2002e$regional_board_no)
# list2006e$regional_board_no <- as.numeric(list2006e$regional_board_no)
# list2018e$regional_board_no <- as.numeric(list2018e$regional_board_no)

#### finally, rowbind
# impaired_list <- bind_rows(list2002e, list2006e, list2010e, list2012e, list2016e, list2018e, list2022e)
impaired_list <- bind_rows(list2010e, list2012e, list2016e, list2018e, list2022e)



### 4. clean up variable possibilities
colnames(impaired_list)

#### view entries by column
levels(as.factor(impaired_list$regional_board_no)) ## ok
# levels(as.factor(impaired_list$water_body_type)) ## cleanup
levels(as.factor(impaired_list$pollutant)) ## no obvious typos
# levels(as.factor(impaired_list$subpollutant)) ## cleanup

#### clean up water_body_type -- used for old excel files from 303d website
# table(as.factor(impaired_list$water_body_type))
# impaired_list <- impaired_list %>%
#   dplyr::mutate(water_body_type = recode(water_body_type, "Bays and Harbors" = "Bay & Harbor",
#          "Coastal Shorelines" = "Coastal & Bay Shoreline",
#          "Estuaries" = "Estuary",
#          # "Lakes/Reserviors" = "Lake & Reservoir",
#          "Lakes/Reservoirs" = "Lake & Reservoir",
#          "Rivers/Streams" = "River & Stream",
#          "Saline Lakes" = "Saline Lake",
#          "Wetlands, Freshwater" = "Wetland, Freshwater",
#          "Wetlands, Tidal" = "Wetland, Tidal"))
# levels(as.factor(impaired_list$water_body_type))

#### clean up subpollutant -- used for old excel files from 303d website
# pollutants <- as.data.frame(table(impairedlist$subpollutant))
# impaired_list <- impaired_list %>%
#   dplyr::mutate(subpollutant = recode(subpollutant,
#                                       "Benzo(a)pyrene  (3,4-Benzopyrene -7-d)" = "Benzo(a)pyrene (3,4-Benzopyrene -7-d",
#                                       "Bis(2ethylhexyl)phthalate/DEHP" = "Bis(2ethylhexyl)phthalate (DEHP)",
#                                       "Dioxin Compounds (including 2,3,7,8-TCDD)" = "Dioxin compounds (including 2,3,7,8-TCDD)",
#                                       "Indicator bacteria" = "Indicator Bacteria"))


### 5. save cleaned df
# write_csv(impaired_list, here("transformeddata", "CWA_303d_waters_2002_2022.csv"))
write_csv(impaired_list, here("transformeddata", "CWA_303d_waters_2010_2022.csv"))
