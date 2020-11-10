library(tidyverse)
library(lubridate)
library(tsibble)
library(readxl)
library(tidyxl)

#devtools::install_github("nacnudus/unpivotr", build_vignettes = TRUE)
library(unpivotr)

path <- "dashboard/data/ATVI 12-Quarter Financial Model Q1 CY20a.xlsx"

# import all sheets and merge to one df
import_df <-
path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(xlsx_cells, path = path)

# filter for sheets
import_clean_df <-
import_df %>%
  filter(sheet %in% c(
    'Operating Metrics',
    'NR and OI by Segment',
    'Rev Mix by Segment',
    'Rev Mix by Geographic Region'
    )) %>%
  filter(is_blank == FALSE, row > 3)  # 3 and above is header, filter out blanks

formats <- xlsx_formats(path) # find format codes

bold <- formats$local$font$bold # find bold format code

merge_df <- data.frame()

# loop for each sheet in df
for (sheet_filter in unique(import_clean_df$sheet)) {

  append_df <-
    import_clean_df %>%
    filter(sheet == sheet_filter) %>%
    behead("up", "quarter") %>% # label quarter
    behead("up", "cy") %>% # label fiscal year
    behead_if(bold[local_format_id], direction = 'left-up', name = 'metric') %>% # label bold columns
    fill(character, .direction = 'down') %>% # label segments (blizzard, king etc)
    select(sheet:character, quarter:metric) # select columns we care about

  merge_df <- bind_rows(merge_df, append_df) # bind it all back together

}

merge_df <-
  merge_df %>%
  select(sheet, numeric, character:metric) %>%
  filter(!is.na(quarter)) %>%
  mutate(year = paste0(20, str_remove_all(cy, '^CY?')),
         year_quarter = paste0(year, ' ', quarter),
         year_quarter = yearquarter(year_quarter)
         )

merge_df %>%
  filter(metric == 'Net Bookings 1') %>%
  ggplot(aes(x = year_quarter, y = numeric, group = character, color = character)) +
  labs(title = 'Quarterly Bookings', y = 'Quarterly Bookings (USD, millions)', x = '', color = "Segment") +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE)
