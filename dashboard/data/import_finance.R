library(tidyverse)
library(stringr)
library(lubridate)
library(tsibble)
library(readxl)
library(tidyxl)
library(unpivotr)

# list of quarterly earnings worksheets
file_paths <- list.files('dashboard/data', pattern = 'xlsx', full.names = TRUE)
#path <- readxl::readxl_example("datasets.xlsx")
#path <- "/Users/pblack/Documents/Git Projects/opensource-freetoplay-economics/dashboard/data/dashboard/data/ATVI 12-Quarter Financial Model Q1 CY20a.xlsx"
path <- file_paths[1]
# initialize data frame
quarterly_results <- data.frame()

# import all sheets from each workbook and merge to one df
#for (path in file_paths) {

 import <-
  path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(xlsx_cells, path = path) %>%
  mutate(workbook = word(path, -1, sep = '/')) # add column with file_name

  quarterly_results <- bind_rows(quarterly_results, import)

#}

# filter for sheets of interest across all workbooks
quarterly_sheets <-
quarterly_results %>%
  filter(sheet %in% c(
    'Operating Metrics',
    'NR and OI by Segment',
    'Rev Mix by Segment',
    'Rev Mix by Geographic Region'
    ))

formats <- xlsx_formats("ATVI 12-Quarter Financial Model Q1 CY20a.xlsx") # find format codes
bold <- formats$local$font$bold # find bold format code

merge_df <- data.frame()

# for each sheet, create new columns and clean data
for (sheet_filter in unique(quarterly_sheets$sheet)) {

  append_df <-
    quarterly_sheets %>%
    filter(sheet == sheet_filter) %>%
    filter(!is_blank, row > 3) %>% # 3 and above is header, filter out blanks
    behead("up", "quarter") %>% # label quarter
    behead("up", "cy") %>% # label fiscal year
    behead_if(bold[local_format_id] == TRUE, direction = 'left-up', name = 'metric') %>% # label bold columns
    select(sheet:character, quarter:metric) %>%  # select columns we care about
    fill(character, .direction = 'down')  # label segments (blizzard, king etc)

  merge_df <- bind_rows(merge_df, append_df) # bind it all back together

}

# clean and mutate data types
merge_df <-
  merge_df %>%
  select(sheet, numeric, character:metric) %>%
  filter(!is.na(quarter)) %>%
  mutate(year = paste0(20, str_remove_all(cy, '^CY?')),
         year_quarter = paste0(year, ' ', quarter),
         year_quarter = yearquarter(year_quarter)
         )
