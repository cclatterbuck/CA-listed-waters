## CA-listed-waters
## 03. visualise listed & delisted waters over time

library(here)
library(tidyverse)
library(janitor)

lists <- read_csv(here("transformeddata", "lists_delists_2010_2022.csv")) %>%
  remove_empty(c("rows", "cols"))

table1 <- lists |>
  count(report_year, listing_status, sort = FALSE) |>
  group_by(listing_status) |> 
  arrange(report_year) |> 
  mutate(cs = cumsum(n)) %>%
  ungroup()
table1$listing_status <- factor(table1$listing_status)
levels(table1$listing_status) <- c("delisted", "new listing")

ggplot(data=table1) +
  geom_line(aes(x = report_year, y = cs, col = listing_status), size = 2) +
  scale_color_manual(labels = c("delisted", "listed"), values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(2010,2022,2)) +
  # geom_text(aes(x = 2018, y = 3100, label = "Listed"), color = "red") +
  # geom_text(aes(x = 2018, y = 300, label = "Delisted"), color = "blue") +
  annotate("label", x = 2021, y = 4200, label = "Listed", fill = "red", color = "white") +
  annotate("label", x = 2021, y = 300, label = "Delisted", fill = "blue", color = "white") +
  labs(title = "Why do we need regulatory watershed protection in California?",
       subtitle = "Waters are becoming impaired faster than we can delist them",
       x = "Integrated report year",
       y = "No. new waterbody/pollutant combinations",
       color = NULL) +
  theme_classic() +
  theme(legend.position = "none")


table2 <- table1 |>
  dplyr::select(-n) |>
  pivot_wider(names_from = listing_status, values_from = cs) |>
  clean_names()
  
ggplot(data=table2, aes(x = report_year)) +
  geom_ribbon(aes(ymin = delisted, ymax = new_listing), fill = "#435501") +
  geom_ribbon(aes(ymin = 0, ymax = delisted), fill = "#550143") +
  scale_x_continuous(breaks = seq(2010,2022,2)) +
  geom_text(aes(x = 2021, y = 2000, label = "Listed"), color = "#FFFEF2", fontface = "bold", size = 7) +
  geom_text(aes(x = 2021, y = 300, label = "Delisted"), color = "#FFFEF2", fontface = "bold", size = 7) +
  labs(title = "California waters are becoming impaired faster than we can delist them", 
       subtitle = "Cumulative sums of California's 303(d) newly listed and delisted waters, 2010-current",
       x = "Year",
       y = "Waterbody/pollutant combinations") +
  # theme_classic() +
  theme(plot.background = element_rect(fill = "#FFFEF2"), # background colour
        plot.title = element_text(colour = "#643d1c", # text colour
                                  size = 16, # font size
                                  face = "bold"), # bold text
        plot.subtitle = element_text(colour = "#643d1c",
                                     size = 13,
                                     face = "italic"),
        plot.caption = element_text(colour = "#643d1c"),
        axis.text = element_text(colour = "#643d1c"),
        axis.title.x = element_text(colour = "#643d1c", size = 10, face = "bold"),
        axis.title.y = element_text(colour = "#643d1c", size = 10, face = "bold"),
        axis.text.x = element_text(colour = "#643d1c", size = 10),
        axis.text.y = element_text(colour = "#643d1c", size = 10),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(fill = "#FFFEF2"),
        panel.grid = element_blank(),
        legend.position = "none")


####
#### OLD
####

# listed <- read_csv(here("transformeddata", "CWA_303d_waters_2006_2022.csv")) %>%
#   remove_empty(c("rows", "cols"))
# 
# delisted <- read_csv(here("transformeddata", "CWA_303d_delistedwaters_2006_2022.csv")) %>%
#   remove_empty(c("rows", "cols"))

## get totals by report year & delisting year

### currently have all listed bodies per year...need to get *new* listings per year. 
### if delisting data was accurate, could subtract these from the previous's year's total # listed and get the new listed bodies for that year. However, we know it is inaccurate!
### could also double-check data using anti_join(2010, 2006)... for the records in each year. 

# listed_year <- listed %>%
#   group_by(report_year) %>%
#   summarise(listed = n()) %>%
#   setNames(.,c("year", "listed")) ## need to figure out new lists per year...
# 
# delisted_year <- delisted %>%
#   group_by(delist_year) %>%
#   summarise(delisted = n()) %>%
#   dplyr::mutate(csum_delisted = cumsum(delisted)) %>%
#   dplyr::select(!delisted) %>%
#   setNames(.,c("year", "csum_delisted"))


# ## put in same dataframe
# visdata <- left_join(listed_year, delisted_year, by = "year")
# 
# visdata2 <- visdata %>%
#   pivot_longer(!year, names_to = "listing_status", values_to = "count")
# visdata2$listing_status <- factor(visdata2$listing_status, levels = c("listed", "csum_delisted"))
#   
# 
# 
# ## add figs
# ggplot(data=visdata2) +
#   geom_ribbon(data = visdata, aes(x = year, ymin = csum_delisted, ymax = listed), fill  = "grey70") +
#   geom_line(aes(x = year, y = count, col = listing_status), size = 2) +
#   scale_color_manual(labels = c("listed", "delisted"), values = c("red", "blue")) +
#   scale_y_continuous(breaks = seq(0,6000,1000)) +
#   annotate("text", label = "the gap of despair :(", x = 2016, y = 2500, size = 6, colour = "black") +
#   theme_bw()
  
