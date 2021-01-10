pacman::p_load(tidyverse, rvest, lubridate, stringr, purrr, httr)

# import list of games
title_url <-
  read_html("https://playstation.fandom.com/wiki/List_of_PlayStation_Plus_games_(North_America)")

# pull down all tables into giant list
tables <-
title_url %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# unnest tables
games_df <-
tables %>%
  bind_rows(tables) %>% # make a huge df
  mutate_if(is.list, simplify_all) %>%   # flatten
  unnest() %>%
  select(Game, starts_with("Platform"), `Date added`, `Date removed`) %>% # stuff we care about
  filter(Game != "Game") %>% # Where the table repeats
  mutate(
    date_added = mdy(`Date added`),
    date_removed = mdy(`Date removed`),
    Game = str_replace(
      Game,
      "Repeat.*", # remove all text to the right of Repeat
      ""
      ),
    Game = str_trim(Game, side = c("both")) # get rid of white space
  ) %>%
  distinct()

  # where are the ps4 and ps5 games?
  games_df %>%
    group_by(`Platform(s)...5`) %>%
    tally()

    games_df %>%
    group_by(`Platform(s)...4`) %>%
    tally()

ps4_and_5 <-
  games_df %>%
  filter(
    `Platform(s)...4` %in% c("PS4", "PS5") |
    `Platform(s)...5` %in% c("PS4", "PS5")
    )

games_list <-
ps4_and_5 %>%
  select(Game) %>%
  mutate( # clean special characters, these are not in URLs
    Game = str_replace_all(Game, ":", ""),
    Game = str_replace_all(Game, "'", ""),
    Game = str_replace_all(Game, " ", "_"),
    Game = str_replace_all(Game, "_\\(....\\)", ""), # gets rid of (1994) notation
    Game = str_replace_all(Game, "__", "_"),
    Game = toupper(Game)
  ) %>%
  distinct() %>%
  pull() # convert to vector

# lots of trial and error, this skips already downloaded games
already_downloaded <-
list.files("posts/ps_plus/csv_scrape/") %>%
  as_tibble() %>%
  mutate(
    value = str_replace(value, ".csv", ""),
    value = toupper(value)
    )

# filter down to games not yet downloaded
games_list <-
  games_list %>%
  as_tibble() %>%
  anti_join(already_downloaded) %>%
  pull()

failed_game_scrapes <- c()

for (game in games_list) {

  download_url <- paste0("https://gamstat.com/csv/PSN-", game, ".csv")

  download_path <- paste0("posts/ps_plus/csv_scrape/", game, ".csv")

  tryCatch(
    download.file(download_url, download_path),
    error = function(e){
      message(paste("Scrape failed for", game))
      failed_game_scrapes <<- c(failed_game_scrapes, game)
      }
    )

  Sys.sleep(5) # wait 30 seconds before running loop again (we don't want to be seen as a DDOS attack)

}

# any pattens in fial

# failed downloads that didn't throw a 404
list.files("posts/ps_plus/csv_scrape/", full.names = TRUE) %>%
  file.info() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  filter(size == 0) %>%
  mutate(
    rowname = word(rowname, sep = '/', -1),
    rowname = str_remove(rowname, '.csv')
    ) %>% view()


quarterly_results <- data.frame()

path <- "posts/ps_plus/csv_scrape/"

# import all sheets from each workbook and merge to one df
for (path in file_paths) {

  import <-
    path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(xlsx_cells, path = path) %>%
    mutate(workbook = word(path, -1, sep = '/')) # add column with file_name
}

  quarterly_results <- bind_rows(quarterly_results, import)
