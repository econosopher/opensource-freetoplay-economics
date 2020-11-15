library(tidyverse)
library(stringr)
library(lubridate)
library(tsibble)
library(readxl)
library(tidyxl)
library(unpivotr)

# list of quarterly earnings worksheets
file_paths_ea <- list.files('data', pattern = 'xlsx', full.names = TRUE)
path_ea <- file_paths_ea[2]

# initialize data frame
quarterly_results_ea <- data.frame()

# import all sheets from each workbook and merge to one df
import_ea <-
  path_ea %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(xlsx_cells, path = path_ea) %>%
  mutate(workbook = word(path_ea, -1, sep = '/')) # add column with file_name

quarterly_results_ea <- bind_rows(quarterly_results_ea, import_ea)

# filter for sheets of interest across all workbooks
quarterly_sheets_ea <-
quarterly_results_ea %>%
  filter(sheet %in% c(
    'Quarterly Net Rev - 12 quarters'
    ))

formats_ea <- xlsx_formats(path_ea) # find format codes
bold_ea <- formats_ea$local$font$bold # find bold format code

merge_df_ea <- data.frame()

# for each sheet, create new columns and clean data
for (sheet_filter in unique(quarterly_sheets_ea$sheet)) {

  append_df_ea <-
    quarterly_sheets_ea %>%
    filter(sheet == sheet_filter) %>%
    filter(!is_blank, row > 3) %>% # 3 and above is header, filter out blanks
    behead("up", "quarter") %>% # label quarter
    behead("up", "cy") %>% # label fiscal year
    behead_if(bold_ea[local_format_id] == TRUE, direction = 'left-up', name = 'metric') %>% # label bold columns
    select(sheet:character, quarter:metric) %>% # select columns we care about
    fill(character, .direction = 'down')  # label segments (blizzard, king etc)

  merge_df_ea <- bind_rows(merge_df_ea, append_df_ea) # bind it all back together

}

# clean and mutate data types
merge_df_ea <-
  merge_df_ea %>%
  select(sheet, numeric, character:metric) %>%
  filter(!is.na(quarter)) %>%
  mutate(year = paste0(20, str_remove_all(cy, '^FY?')),
         year_quarter = paste0(year, ' ', quarter),
         year_quarter = yearquarter(year_quarter)
         )
