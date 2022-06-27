## CA-listed-waters
## 02. Determine 303(d) delisted waters
## output is not cleaned yet, do not use for any official purposes.
## Data starts from output of step 01. Like step 01, the output is a csv of all delisted waters, which may contain duplicates of water_body_id as each delist represents a water_body_id and a subpollutant. 
## if anyone knows how to loop or purrr::map this flow, please contribute!

library(here)
library(tidyverse)
library(janitor)

# listed <- read_csv(here("transformeddata", "CWA_303d_waters_2002_2022.csv")) %>%
#   remove_empty(c("rows", "cols"))
listed <- read_csv(here("transformeddata", "CWA_303d_waters_2006_2022.csv")) %>%
  remove_empty(c("rows", "cols"))

## make water_body_id a factor & separate data by year
listed$report_year_char <- paste0("df",as.character(listed$report_year))
listed$water_body_id <- as.factor(listed$water_body_id)
list_year <- split(listed, listed$report_year_char)
list2env(list_year,envir=.GlobalEnv)

## create dfs of delistings by year
## assumes all rows use the same terminology for pollutants & subpollutants...
# delist1 <- anti_join(df2002, df2006, by = c('water_body_id', 'pollutant')) ## no subpollutants in 2002 dataset
delist2 <- anti_join(df2006, df2010, by = c('water_body_id', 'subpollutant'))
delist3 <- anti_join(df2010, df2012, by = c('water_body_id', 'subpollutant'))
delist4 <- anti_join(df2012, df2016, by = c('water_body_id', 'subpollutant'))
delist5 <- anti_join(df2016, df2018, by = c('water_body_id', 'subpollutant'))
delist6 <- anti_join(df2018, df2022, by = c('water_body_id', 'subpollutant'))

## combine & save
# delisted <- bind_rows(delist1, delist2, delist3, delist4, delist5, delist6) %>%
#   dplyr::select(-report_year_char)
delisted <- bind_rows(delist2, delist3, delist4, delist5, delist6) %>%
  dplyr::select(-report_year_char)
str(delisted)
# write_csv(delisted, here("transformeddata", "CWA_303d_delistedwaters_2002_2022.csv"))
write_csv(delisted, here("transformeddata", "CWA_303d_delistedwaters_2006_2022.csv"))


