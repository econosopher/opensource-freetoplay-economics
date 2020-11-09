library(tidyverse)
library(readxl)
library(tidyxl)

path <- "dashboard/data/ATVI 12-Quarter Financial Model Q1 CY20a.xlsx"

# import all sheets as seperate df's
import_df <-
path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)

operating_metrics <-
import_df$`Operating Metrics` %>%
  filter_all(any_vars(!is.na(.))) %>% # filter of all NA rows
  slice(3:n()) %>% # filter of partial NA rows
  filter(!grepl("^\\d ", `...2`)) # filter notes

quarter <-
operating_metrics %>%
  filter(grepl("^Q\\d", `...3`)) %>% # filter for Qx rows
  t() %>% # transpose
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(key = rowname, quarter = V1) # create join column

cy <-
operating_metrics %>%
  filter(grepl("^CY\\d", `...3`)) %>% # filter for CYx rows
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(key = rowname, cy = V1) # create join column

metrics <-
operating_metrics %>%
  filter(!is.na(`...2`)) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  rename(key = rowname)

metrics_join <-
metrics %>%
  inner_join(quarter, by = "key") %>%
  inner_join(cy, by = "key")

metrics_names <- # create row to replace Vx column with metric names
  metrics_join %>%
  slice(2) %>%
  select(starts_with('V')) %>%
  unlist(use.names = FALSE) %>%
  as.character()

metrics_join %>%
  #mutate_at(vars(starts_with('V')), as.double) %>%
  mutate(cy = as.character(cy)) %>%
  rename_with(.fn = ~paste0(metrics_names), starts_with("V")) %>%
  slice(3:n()) %>%
  filter_all(any_vars(!is.na(.))) %>%   # filter of all NA rows
  drop_na(cy) %>%
  select_if(~!all(is.na(.)))


