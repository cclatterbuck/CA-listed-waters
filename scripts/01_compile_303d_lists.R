## CA-listed-waters
## 01. Compile 303(d) listed waters from Water Boards IR website
## Note: does not include delisting data
## Excel files from: https://www.waterboards.ca.gov/water_issues/programs/water_quality_assessment/integrated_report_cycles.html


library(here)
library(tidyverse)
library(readxl)
library(janitor)



### 1. load Excel files
# list2002 <- read_excel(here("rawdata_xls", "2002-303d-list.xls"), sheet = 1) %>%
#   clean_names %>%
#   remove_empty(c("rows", "cols"))
list2006 <- read_excel(here("rawdata_xls", "2006-303d-list.xls"), sheet = 1) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2010 <- read_excel(here("rawdata_xls", "2010-303d-list.xls"), sheet = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2012 <- read_excel(here("rawdata_xls", "2012-303d-list.xlsx"), sheet = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2016 <- read_excel(here("rawdata_xls", "apx-i-1416-303d-list.xlsx"), sheet = 1, skip = 3) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2018 <- read_excel(here("rawdata_xls", "apx-a-2018-303d-list.xlsx"), sheet = 1, skip = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))
list2022 <- read_excel(here("rawdata_xls", "apx-a-2022-303d-list.xlsx"), sheet = 1, skip = 2) %>%
  clean_names %>%
  remove_empty(c("rows", "cols"))



### 2. clean columns among dfs: select columns, new column names, add year.
# colnames(list2002)
colnames(list2022)

newnames <- c("water_body_id", "report_year", "regional_board_no",
              "water_body_type", "pollutant", "subpollutant",
              "water_body_name")

## to add IR categories to 2010 and beyond
# newnames2 <- c("water_body_id", "report_year", "regional_board_no",
#               "water_body_type", "pollutant", "subpollutant",
#               "water_body_name", "integrated_report_category")

# list2002e <- list2002 %>%
#   dplyr::mutate(report_year = 2002,
#                 subpollutant = NA) %>%
#   dplyr::select(wbid, report_year, regional_board_no, water_body_type, pollutant_stressor, subpollutant, water_body_name) %>%
#   setNames(.,newnames) %>%
#   distinct() ## removes duplicated rows due to separate, suspected source data 

list2006e <- list2006 %>%
  dplyr::mutate(report_year = 2006) %>%
  dplyr::select(wbid, report_year, region_number, water_body_type, pollutant_category, pollutant, water_body_name) %>%
  setNames(.,newnames) %>%
  distinct() ## removes duplicated rows due to separate, suspected source data
  
list2010e <- list2010 %>%
  dplyr::mutate(report_year = 2010) %>%
  dplyr::select(wbid, report_year, region, water_body_type, pollutant_category, pollutant, water_body_name) %>%
  setNames(.,newnames)

list2012e <- list2012 %>%
  dplyr::mutate(report_year = 2012) %>%
  dplyr::select(wbid, report_year, region, water_body_type, pollutant_category, pollutant, water_body_name) %>%
  setNames(.,newnames)

list2016e <- list2016 %>%
  dplyr::mutate(report_year = 2016) %>%
  dplyr::select(wbid, report_year, region, water_body_type, pollutant_category, pollutant, water_body_name) %>%
  setNames(.,newnames)

list2018e <- list2018 %>%
  dplyr::mutate(report_year = 2018) %>%
  dplyr::select(water_body_id, report_year, regional_board, water_body_type, pollutant_category, pollutant, water_body) %>%
  setNames(.,newnames)

list2022e <- list2022 %>%
  dplyr::mutate(report_year = 2022) %>%
  dplyr::select(water_body_id, report_year, regional_board, water_body_type, pollutant_category, pollutant, water_body) %>%
  setNames(.,newnames)



### 3. rowbind dfs of 303(d) lists
#### data types of cols should be: chr, num, num, chr, chr, chr, chr
# compare_df_cols(list2002e, list2006e, list2010e, list2012e, list2016e, list2018e, list2022e, return = "mismatch")
compare_df_cols(list2006e, list2010e, list2012e, list2016e, list2018e, list2022e, return = "mismatch")

#### change data types of mismatched cols
# list2002e$subpollutant <- as.character(list2002e$subpollutant)
# list2002e$regional_board_no <- as.numeric(list2002e$regional_board_no)
list2006e$regional_board_no <- as.numeric(list2006e$regional_board_no)
list2018e$regional_board_no <- as.numeric(list2018e$regional_board_no)

#### finally, rowbind
# impaired_list <- bind_rows(list2002e, list2006e, list2010e, list2012e, list2016e, list2018e, list2022e)
impaired_list <- bind_rows(list2006e, list2010e, list2012e, list2016e, list2018e, list2022e)



### 4. clean up variable possibilities
colnames(impaired_list)

#### view entries by column
levels(as.factor(impaired_list$regional_board_no)) ## ok
levels(as.factor(impaired_list$water_body_type)) ## cleanup
levels(as.factor(impaired_list$pollutant)) ## no obvious typos
levels(as.factor(impaired_list$subpollutant)) ## no obvious typos

#### clean up water_body_type
table(as.factor(impaired_list$water_body_type))
impaired_list <- impaired_list %>%
  dplyr::mutate(water_body_type = recode(water_body_type, "Bays and Harbors" = "Bay & Harbor",
         "Coastal Shorelines" = "Coastal & Bay Shoreline",
         "Estuaries" = "Estuary",
         # "Lakes/Reserviors" = "Lake & Reservoir",
         "Lakes/Reservoirs" = "Lake & Reservoir",
         "Rivers/Streams" = "River & Stream",
         "Saline Lakes" = "Saline Lake",
         "Wetlands, Freshwater" = "Wetland, Freshwater",
         "Wetlands, Tidal" = "Wetland, Tidal"))
levels(as.factor(impaired_list$water_body_type))



### 5. save cleaned df
# write_csv(impaired_list, here("transformeddata", "CWA_303d_waters_2002_2022.csv"))
write_csv(impaired_list, here("transformeddata", "CWA_303d_waters_2006_2022.csv"))
