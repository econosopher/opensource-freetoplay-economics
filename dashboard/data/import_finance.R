library(tidyverse)
library(lubridate)
library(tsibble)
library(readxl)
library(tidyxl)
library(unpivotr)

# list of files
file_path <- list.files('dashboard/data', pattern = 'xlsx', full.names = TRUE)

# initialize data frame
quarterly_results <- data.frame()

# import all sheets and merge to one df
for (path in file_path) {

 import_df <-
  path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(xlsx_cells, path = path) %>%
  mutate(file = path) # add column with path name

  quarterly_results <- bind_rows(quarterly_results, import_df)

}

# filter for sheets of interest
quarterly_results_df <-
quarterly_results %>%
  filter(sheet %in% c(
    'Operating Metrics',
    'NR and OI by Segment',
    'Rev Mix by Segment',
    'Rev Mix by Geographic Region'
    ))

formats <- xlsx_formats(path) # find format codes

bold <- formats$local$font$bold # find bold format code

merge_df <- data.frame()

# for each sheet, create new columns and clean data
for (sheet_filter in unique(quarterly_results_df$sheet)) {

  #append_df <-
    quarterly_results_df %>%
    filter(sheet == sheet_filter) %>%
    filter(!is_blank, row > 3) %>% # 3 and above is header, filter out blanks
    behead("up", "quarter") %>% # label quarter
    behead("up", "cy") %>% # label fiscal year
    behead_if(bold[local_format_id] == TRUE, direction = 'left-up', name = 'metric') %>% # label bold columns
    select(sheet:character, quarter:metric) %>%  # select columns we care about
    filter(address == 'B8')
    fill(character, .direction = 'down') %>% # label segments (blizzard, king etc)


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

#append_df <-
    quarterly_results_df %>%
    filter(sheet == 'Operating Metrics') %>%
    filter(is_blank == FALSE, row > 3) %>% # 3 and above is header, filter out blanks
    behead("up", "quarter") %>% # label quarter
    behead("up", "cy") %>%  # label fiscal year
    view()
    behead_if(bold[local_format_id] == TRUE, direction = 'left-up', name = 'metric')