## CA-listed-waters
## 02. Determine 303(d) delisted waters
## output is not cleaned yet, do not use for any official purposes.
## Data starts from output of step 01. The output is a table with all newly listed and delisted wbid/pollutant combinations in the IR from 2010-current. 
## if anyone knows how to loop or purrr::map this flow, please create a branch & pull request to contribute!

library(here)
library(tidyverse)
library(janitor)
library(readxl)

# listed <- read_csv(here("transformeddata", "CWA_303d_waters_2006_2022.csv")) %>%
#   remove_empty(c("rows", "cols")) ## no longer included as new data from 2006 because "new decision" indicates the beginning of the database and not an *actual* new decision. 
decisions <- read_csv(here("transformeddata", "CWA_303d_waters_2010_2022.csv")) %>%
  remove_empty(c("rows", "cols"))


## filter out current cycle decisions that are "do not list", which also removes decisions with multiple pollutants (which are never listed per 303d team)
levels(as.factor(decisions$current_cycle_poll_listing_decision))
listed <- decisions |>
  dplyr::filter(current_cycle_poll_listing_decision != "Do Not List on 303(d) list (TMDL required list)")
levels(as.factor(listed$current_cycle_poll_listing_decision))


## load pollutant category file & add as column to listed
categories <- read_excel(here("rawdata_xls", "IR_Files", "IR_Pollutant_Category.xlsx")) %>%
  remove_empty(c("rows", "cols")) %>%
  clean_names() %>%
  dplyr::select(pollutant_name, pollutant_category_name) %>%
  rename(pollutant = pollutant_name,
         pollutant_category = pollutant_category_name)

categories$pollutant_category <- recode(categories$pollutant_category,
                                        "SALINITY/TOTAL DISSOLVED SOLIDS/CHLORIDES/SULFATES" = "Salinity/Total Dissolved Solids/Chlorides/Sulfates")

listed <- listed |>
  left_join(categories, by = "pollutant") %>%
  relocate(pollutant_category, .after = pollutant)


## reduce characters in previous cycle & current cycle columns
listed$previous_cycle_poll_listing_decision <- str_sub(listed$previous_cycle_poll_listing_decision, start = 1, end = 13)
levels(as.factor(listed$previous_cycle_poll_listing_decision))
listed$previous_cycle_poll_listing_decision <- recode(listed$previous_cycle_poll_listing_decision,
                                                      "Delist from 3" = "Delist",
                                                      "Do Not List o" = "Do Not List",
                                                      "List on 303(d" = "List")
levels(as.factor(listed$current_cycle_poll_listing_decision)) # check 
  
listed$current_cycle_poll_listing_decision <- str_sub(listed$current_cycle_poll_listing_decision, start = 1, end = 13)
levels(as.factor(listed$current_cycle_poll_listing_decision))
listed$current_cycle_poll_listing_decision <- recode(listed$current_cycle_poll_listing_decision,
                                                      "Delist from 3" = "Delist",
                                                      "List on 303(d" = "List")
levels(as.factor(listed$current_cycle_poll_listing_decision))


## determine new listings & delistings for each cycle & add listing_status column
newlistings <- listed |>
  dplyr::filter(current_cycle_poll_listing_decision == "List" & previous_cycle_poll_listing_decision != "List") |>
  dplyr::mutate(listing_status = "new listing")

delistings <- listed |>
  dplyr::filter((current_cycle_poll_listing_decision == "Delist" & previous_cycle_poll_listing_decision == "List") | 
                  (current_cycle_poll_listing_decision == "Delist" & previous_cycle_poll_listing_decision == "Do Not Delist")) |>
  dplyr::mutate(listing_status = "delisted")


## bind newlistings and delistings. Result is a table with all newly listed and delisted wbid/pollutant combinations in the IR from 2010-current.
final <- bind_rows(newlistings, delistings) %>%
  relocate(listing_status, .after = report_year) %>%
  arrange(report_year, regional_board_no, listing_status)

## save
# write_csv(final, here("transformeddata", "lists_delists_2010_2022.csv"))
