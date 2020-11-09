library(tidyverse)
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

import_clean_df <-
import_df %>%
  filter(sheet %in% c(
    'Operating Metrics',
    'NR and OI by Segment',
    'Rev Mix by Segment',
    'Rev Mix by Geographic Region'
    )) %>%
  filter(is_blank == FALSE, row > 3)  # 3 and above is header

formats <- xlsx_formats(path) # find format codes

bold <- formats$local$font$bold # find bold format code

merge_df <- data.frame()

for (sheet_filter in unique(import_clean_df$sheet)) {

  append_df <-
    import_clean_df %>%
    filter(sheet == sheet_filter) %>%
    behead("up", "quarter") %>%
    behead("up", "cy") %>%
    behead_if(bold[local_format_id], direction = 'left-up', name = 'metric') %>%
    fill(character, .direction = 'down') %>%
    select(sheet:character, quarter:metric)

  merge_df <- bind_rows(merge_df, append_df)

}



operating_metrics %>%
  behead("up", "quarter") %>%
  behead("up", "cy") %>%
  behead_if(bold[local_format_id], direction = 'left-up', name = 'metric') %>%
  select(sheet:data_type, numeric:character, quarter, cy, metric) %>%
  fill(character, .direction = 'down') %>%
  view()
