pacman::p_load(tidyverse, rvest, lubridate, stringr, purrr, httr, fs, scales)

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
  filter(Game != "Game") %>% # where the table repeats
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

  # where are the PS4 and PS5 games?
  games_df %>%
    group_by(`Platform(s)...5`) %>%
    tally()

    games_df %>%
    group_by(`Platform(s)...4`) %>%
    tally()

# filter for those games
ps4_and_5 <-
  games_df %>%
  filter(
    `Platform(s)...4` %in% c("PS4", "PS5") |
    `Platform(s)...5` %in% c("PS4", "PS5")
    )

# clean string names
games_list <-
ps4_and_5 %>%
  select(Game) %>%
  mutate( # clean special characters, these are not in URLs
    Game = toupper(Game),
    Game = str_replace_all(Game, ":", ""),
    Game = str_replace_all(Game, "'", ""),
    Game = str_replace_all(Game, " ", "_"),
    Game = str_replace_all(Game, "_\\(....\\)", ""), # gets rid of (1994) notation
    Game = str_replace_all(Game, "__", "_"),
    Game = str_replace_all(Game, "!", ""),
    Game = str_replace_all(Game, "_EDITION", "")
  ) %>%
  distinct() %>%
  pull() # convert to vector

failed_game_scrapes <- c()

for (game in games_list) {

  download_url <- paste0("https://gamstat.com/csv/PSN-", game, ".csv")

  download_path <- paste0("posts/ps_plus/csv_scrape/", game, ".csv")

  # create list of failed games
  tryCatch(
    download.file(download_url, download_path),
    error = function(e){
      message(paste("Scrape failed for", game))
      failed_game_scrapes <<- c(failed_game_scrapes, game)
      }
    )

  Sys.sleep(5) # wait x seconds before running loop again (we don't want to be seen as a DDOS attack)

}

# failed downloads that didn't throw a 404 but had a 0 bytes error
re_run_list <-
list.files("posts/ps_plus/csv_scrape/", full.names = TRUE) %>%
  file.info() %>%
  rownames_to_column() %>%
  as_tibble() %>%
  filter(size == 0) %>%
  mutate(
    rowname = word(rowname, sep = '/', -1),
    rowname = str_remove(rowname, '.csv')
    ) %>%
  select(value = rowname) %>%
  bind_rows(
    as_tibble_col(failed_game_scrapes)
  ) %>%
  rename(Game = value) %>%
  mutate(
    Game = str_replace_all(Game, "_EDITION", "")
  ) %>%
  distinct() %>%
  mutate( # that's right, we're doing
    Game = case_when(
      Game == '10_SECOND_NINJA' ~ '10_SECOND_NINJA_X',
      Game == 'BADLAND_GAME_OF_THE_YEAR' ~ 'BADLAND',
      Game == 'BIOSHOCK_THE_COLLECTION' ~ 'BIOSHOCK',
      Game == 'BORDERLANDS_THE_HANDSOME_COLLECTION' ~ 'BORDERLANDS',
      Game == 'BULLETSTORM_FULL_CLIP' ~ 'BULLETSTORM',
      Game == 'CALL_OF_DUTY_MODERN_WARFARE_2_CAMPAIGN_REMASTERED' ~ 'CALL_OF_DUTY_MODERN_WARFARE_2',
      Game == 'CALL_OF_DUTY_MODERN_WARFARE_REMASTERED' ~ 'Call_of_Duty_Modern_Warfare_2016',
      Game == 'CLAIRE_EXTENDED_CUT' ~ 'CLAIRE',
      Game == 'DARKSIDERS_II_DEATHINITIVE' ~ 'DARKSIDERS_II',
      Game == 'DAY_OF_THE_TENTACLE_REMASTERED' ~ 'DAY_OF_THE_TENTACLE',
      Game == 'DEAD_NATION_APOCALYPSE' ~ 'DEAD_NATION',
      Game == 'DRIVECLUB_PLAYSTATION_PLUS' ~ 'DRIVECLUB',
      Game == 'GAME_OF_THRONES_' ~ 'GAME_OF_THRONES',
      Game == 'GAUNTLET_SLAYER' ~ 'GAUNTLET',
      Game == 'GOD_OF_WAR_III_REMASTERED' ~ 'GOD_OF_WAR_III',
      Game == 'GONE_HOME_CONSOLE' ~ 'GONE_HOME',
      Game == 'GRIM_FANDANGO_REMASTERED' ~ 'GRIM_FANDANGO',
      Game == 'GUACAMELEE_SUPER_TURBO_CHAMPIONSHIP' ~ 'GUACAMELEE',
      Game == 'HELLDIVERS_DEMOCRACY_STRIKES_BACK' ~ 'HELLDIVERS',
      Game == 'HITMAN_THE_COMPLETE_FIRST_SEASON' ~ 'HITMAN',
      Game == 'HOLLOW_KNIGHT_VOIDHEART' ~ 'HOLLOW_KNIGHT',
      Game == 'LETTER_QUEST_REMASTERED' ~ 'LETTER_QUEST',
      Game == 'MONSTER_ENERGY_SUPERCROSS' ~ 'Monster_Energy_Supercross_The_Official_Videogame',
      Game == 'ODDWORLD_NEW_N_TASTY' ~ 'Oddworld_New-n-Tasty',
      Game == 'OLLIOLLI2_WELCOME_TO_OLLIWOOD' ~ 'OlliOlli_2_Welcome_to_Olliwood',
      Game == 'RISE_OF_THE_TOMB_RAIDER_20_YEAR_CELEBRATION' ~ 'RISE_OF_THE_TOMB_RAIDER',
      Game == 'SKULLS_OF_THE_SHOGUN_BONE-A-FIDE' ~ 'SKULLS_OF_THE_SHOGUN',
      Game == 'SUPER_TIME_FORCE_ULTRA' ~ 'SUPER_TIME_FORCE',
      Game == 'THE_LAST_OF_US_REMASTERED' ~ 'THE_LAST_OF_US',
      Game == 'THE_WALKING_DEAD_SEASON_TWO' ~ 'THE_WALKING_DEAD_SEASON_2',
      Game == 'TRINE_2_COMPLETE_STORY' ~ 'TRINE_2',
      Game == 'TYPERIDER' ~ 'TYPE_RIDER',
      Game == 'UNCHARTED_4_A_THIEFS_END' ~ 'UNCHARTED_4',
      Game == 'UNCHARTED_THE_NATHAN_DRAKE_COLLECTION' ~ 'Uncharted_Drakes_Fortune',
      Game == 'UNMECHANICAL_EXTENDED' ~ 'UNMECHANICAL',
      Game == 'WORMS_RUMBLE' ~ 'Worms_Battlegrounds',
      Game == 'INJUSTICE_GODS_AMONG_US_–_ULTIMATE' ~ 'INJUSTICE_GODS_AMONG_US',
      Game == 'KINGS_QUEST_–_CHAPTER_I_A_KNIGHT_TO_REMEMBER' ~ 'Kings_Quest',
      Game == 'PUMPED_BMX+' ~ 'Pumped_BMX_plus',
      Game == 'INVISIBLE,_INC.' ~ 'Invisible_Inc',
      Game == 'ABZÛ' ~ 'ABZU',
      Game == 'DONT_DIE,_MR._ROBOT' ~ 'Dont_Die_Mr_Robot',
      Game == 'FORMA.8' ~ 'Forma_8',
      Game == 'RATCHET_&CLANK' ~ 'Ratchet_and_Clank_2016',
      Game == 'MIGHTY_NO._9' ~ 'Mighty_No_9',
      Game == 'Q*BERT_REBOOTED' ~ 'Q_bert_Rebooted',
      Game == 'ANOTHER_WORLD_–_20TH_ANNIVERSARY' ~ 'ANOTHER_WORLD',
      Game == 'Q.U.B.E_DIRECTORS_CUT' ~ 'QUBE',
      Game == 'DETROIT_BECOME_HUMAN_–_DIGITAL_DELUXE' ~ 'DETROIT_BECOME_HUMAN',
      Game == 'DIRT_RALLY_2.0' ~ 'Dirt_Rally_2_0',
      TRUE ~ Game
    ),
    Game = toupper(Game)
  ) %>%
  pull()

failed_game_scrapes <- c()

for (game in re_run_list) {

  download_url <- paste0("https://gamstat.com/csv/PSN-", game, ".csv")

  download_path <- paste0("posts/ps_plus/csv_scrape/", game, ".csv")

  # create list of failed games
  tryCatch(
    download.file(download_url, download_path),
    error = function(e){
      message(paste("Scrape failed for", game))
      failed_game_scrapes <<- c(failed_game_scrapes, game)
      }
    )

  Sys.sleep(5) # wait x seconds before running loop again (we don't want to be seen as a DDOS attack)

}

import <-
  fs::dir_ls("posts/ps_plus/csv_scrape/", regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source")

import_df <-
  import %>%
  mutate(
    Game = word(source, -1, -1, "/"),
    Game = str_replace_all(Game, ".csv", "")
    ) %>%
  inner_join(
    as_tibble(ps4_and_5) %>%
      mutate( # clean special characters, these are not in URLs
        Game = toupper(Game),
        Game = str_replace_all(Game, ":", ""),
        Game = str_replace_all(Game, "'", ""),
        Game = str_replace_all(Game, " ", "_"),
        Game = str_replace_all(Game, "_\\(....\\)", ""), # gets rid of (1994) notation
        Game = str_replace_all(Game, "__", "_"),
        Game = str_replace_all(Game, "!", ""),
        Game = str_replace_all(Game, "_EDITION", ""),
        Game = case_when(
          Game == '10_SECOND_NINJA' ~ '10_SECOND_NINJA_X',
          Game == 'BADLAND_GAME_OF_THE_YEAR' ~ 'BADLAND',
          Game == 'BIOSHOCK_THE_COLLECTION' ~ 'BIOSHOCK',
          Game == 'BORDERLANDS_THE_HANDSOME_COLLECTION' ~ 'BORDERLANDS',
          Game == 'BULLETSTORM_FULL_CLIP' ~ 'BULLETSTORM',
          Game == 'CALL_OF_DUTY_MODERN_WARFARE_2_CAMPAIGN_REMASTERED' ~ 'CALL_OF_DUTY_MODERN_WARFARE_2',
          Game == 'CALL_OF_DUTY_MODERN_WARFARE_REMASTERED' ~ 'Call_of_Duty_Modern_Warfare_2016',
          Game == 'CLAIRE_EXTENDED_CUT' ~ 'CLAIRE',
          Game == 'DARKSIDERS_II_DEATHINITIVE' ~ 'DARKSIDERS_II',
          Game == 'DAY_OF_THE_TENTACLE_REMASTERED' ~ 'DAY_OF_THE_TENTACLE',
          Game == 'DEAD_NATION_APOCALYPSE' ~ 'DEAD_NATION',
          Game == 'DRIVECLUB_PLAYSTATION_PLUS' ~ 'DRIVECLUB',
          Game == 'GAME_OF_THRONES_' ~ 'GAME_OF_THRONES',
          Game == 'GAUNTLET_SLAYER' ~ 'GAUNTLET',
          Game == 'GOD_OF_WAR_III_REMASTERED' ~ 'GOD_OF_WAR_III',
          Game == 'GONE_HOME_CONSOLE' ~ 'GONE_HOME',
          Game == 'GRIM_FANDANGO_REMASTERED' ~ 'GRIM_FANDANGO',
          Game == 'GUACAMELEE_SUPER_TURBO_CHAMPIONSHIP' ~ 'GUACAMELEE',
          Game == 'HELLDIVERS_DEMOCRACY_STRIKES_BACK' ~ 'HELLDIVERS',
          Game == 'HITMAN_THE_COMPLETE_FIRST_SEASON' ~ 'HITMAN',
          Game == 'HOLLOW_KNIGHT_VOIDHEART' ~ 'HOLLOW_KNIGHT',
          Game == 'LETTER_QUEST_REMASTERED' ~ 'LETTER_QUEST',
          Game == 'MONSTER_ENERGY_SUPERCROSS' ~ 'Monster_Energy_Supercross_The_Official_Videogame',
          Game == 'ODDWORLD_NEW_N_TASTY' ~ 'Oddworld_New-n-Tasty',
          Game == 'OLLIOLLI2_WELCOME_TO_OLLIWOOD' ~ 'OlliOlli_2_Welcome_to_Olliwood',
          Game == 'RISE_OF_THE_TOMB_RAIDER_20_YEAR_CELEBRATION' ~ 'RISE_OF_THE_TOMB_RAIDER',
          Game == 'SKULLS_OF_THE_SHOGUN_BONE-A-FIDE' ~ 'SKULLS_OF_THE_SHOGUN',
          Game == 'SUPER_TIME_FORCE_ULTRA' ~ 'SUPER_TIME_FORCE',
          Game == 'THE_LAST_OF_US_REMASTERED' ~ 'THE_LAST_OF_US',
          Game == 'THE_WALKING_DEAD_SEASON_TWO' ~ 'THE_WALKING_DEAD_SEASON_2',
          Game == 'TRINE_2_COMPLETE_STORY' ~ 'TRINE_2',
          Game == 'TYPERIDER' ~ 'TYPE_RIDER',
          Game == 'UNCHARTED_4_A_THIEFS_END' ~ 'UNCHARTED_4',
          Game == 'UNCHARTED_THE_NATHAN_DRAKE_COLLECTION' ~ 'Uncharted_Drakes_Fortune',
          Game == 'UNMECHANICAL_EXTENDED' ~ 'UNMECHANICAL',
          Game == 'WORMS_RUMBLE' ~ 'Worms_Battlegrounds',
          Game == 'INJUSTICE_GODS_AMONG_US_–_ULTIMATE' ~ 'INJUSTICE_GODS_AMONG_US',
          Game == 'KINGS_QUEST_–_CHAPTER_I_A_KNIGHT_TO_REMEMBER' ~ 'Kings_Quest',
          Game == 'PUMPED_BMX+' ~ 'Pumped_BMX_plus',
          Game == 'INVISIBLE,_INC.' ~ 'Invisible_Inc',
          Game == 'ABZÛ' ~ 'ABZU',
          Game == 'DONT_DIE,_MR._ROBOT' ~ 'Dont_Die_Mr_Robot',
          Game == 'FORMA.8' ~ 'Forma_8',
          Game == 'RATCHET_&CLANK' ~ 'Ratchet_and_Clank_2016',
          Game == 'MIGHTY_NO._9' ~ 'Mighty_No_9',
          Game == 'Q*BERT_REBOOTED' ~ 'Q_bert_Rebooted',
          Game == 'ANOTHER_WORLD_–_20TH_ANNIVERSARY' ~ 'ANOTHER_WORLD',
          Game == 'Q.U.B.E_DIRECTORS_CUT' ~ 'QUBE',
          Game == 'DETROIT_BECOME_HUMAN_–_DIGITAL_DELUXE' ~ 'DETROIT_BECOME_HUMAN',
          Game == 'DIRT_RALLY_2.0' ~ 'Dirt_Rally_2_0',
          TRUE ~ Game
          ),
        Game = toupper(Game)
        ) %>%
      distinct()
    )

import_df <-
import_df %>%
  group_by(Game) %>%
  mutate(daily_ps4_entitlements = `PS4 player count` - lag(`PS4 player count`)) %>%
  ungroup(Game) %>%
  mutate(
    ps_plus_period = if_else(
      (Date >= date_added & Date < date_removed) == TRUE,
      TRUE,
      FALSE)
    )

import_df %>%
  filter(Game == 'FALL_GUYS_ULTIMATE_KNOCKOUT') %>%
  ggplot(aes(x = Date, y = daily_ps4_entitlements, color = ps_plus_period)) +
  geom_point() +
  labs(y = 'Daily Playstation Entitlements') +
  scale_y_continuous(labels = comma)

import_df %>%
  ggplot(aes(x = Date, y = daily_ps4_entitlements, color = ps_plus_period, group = ps_plus_period)) +
  geom_point() +
  geom_smooth()

import_df %>%
  group_by(Game) %>%
  filter(ps_plus_period == TRUE) %>%
  group_by(Game, date_added) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(ps_plus_downloads > 0) %>%
  ggplot(aes(x = date_added, y = ps_plus_downloads)) +
    geom_point() +
    geom_smooth() +
    labs(y = '', x = '') +
    scale_y_continuous(labels = scales::comma)

import_df %>%
  group_by(Game) %>%
  filter(ps_plus_period == TRUE) %>%
  group_by(Game, date_added) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(ps_plus_downloads > 0) %>%
  ungroup(Game, date_added) %>%
  mutate(year_added = year(date_added)) %>%
  group_by(year_added) %>%
  summarise(median = median(ps_plus_downloads, na.rm = T))

import_df %>%
  group_by(Game) %>%
  filter(ps_plus_period == TRUE) %>%
  group_by(Game, date_added) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(ps_plus_downloads > 0) %>%
  ungroup(Game, date_added) %>%
  mutate(year_added = year(date_added)) %>%
  group_by(year_added) %>%
  summarise(median = median(ps_plus_downloads, na.rm = T))

import_df %>%
  group_by(Game) %>%
  filter(ps_plus_period == TRUE) %>%
  group_by(Game, date_added) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(ps_plus_downloads > 0) %>%
  arrange(desc(ps_plus_downloads))

import_df %>%
  group_by(Game) %>%
  mutate(
    launch_date = first(Date),
    ps_plus_added_from_launch = time_length(date_added - launch_date, unit = "weeks"),
    ) %>%
  select(Game, ps_plus_added_from_launch, launch_date, date_added) %>%
  distinct() %>%
  ggplot(aes(y = ps_plus_added_from_launch, x = date_added)) +
  geom_point() +
  geom_smooth() +
    labs(y = 'Weeks Since First PS Launch', x = 'PS+ Launch') +
    scale_y_continuous(labels = scales::comma)

import_df <-
import_df %>%
  mutate(
    ps_leagth = date_removed - date_added,
    months_since_psplus = case_when(
      ps_plus_period == TRUE ~ 0,
      TRUE ~ ceiling(time_length(Date - date_removed, unit = 'months'))
      )
    )

import_df %>%
  group_by(Game, months_since_psplus, ps_plus_period) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(
    months_since_psplus >= -1 & months_since_psplus <= 1,
    !(ps_plus_period == FALSE & months_since_psplus == 0)
    ) %>%
  ggplot(aes(x = months_since_psplus, y = ps_plus_downloads, color = ps_plus_period, group = Game)) +
    geom_line() +
    geom_point() +
    labs(y = "Daily New Entitlements", x = "Months Since PS+") +
    scale_y_continuous(label = comma)

import_df %>%
  group_by(Game) %>%
  mutate(min_months_since_ps_plus = min(months_since_psplus)) %>%
  group_by(Game, months_since_psplus, ps_plus_period, min_months_since_ps_plus) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(
    min_months_since_ps_plus <= -2,
    months_since_psplus >= -1 & months_since_psplus <= 1 & months_since_psplus != 0,
    !(ps_plus_period == FALSE & months_since_psplus == 0)
    ) %>%
    ggplot(aes(x = months_since_psplus, y = ps_plus_downloads, group = Game)) +
    geom_line() +
    geom_point() +
    labs(y = "Monthly New Entitlements", x = "Change in Downloads 1 Month Before and After PS+") +
    scale_y_continuous(label = comma)+
    scale_x_discrete()

import_df %>%
  group_by(Game) %>%
  mutate(min_months_since_ps_plus = min(months_since_psplus)) %>%
  group_by(Game, months_since_psplus, ps_plus_period, min_months_since_ps_plus) %>%
  summarise(ps_plus_downloads = sum(daily_ps4_entitlements, na.rm = T)) %>%
  filter(
    min_months_since_ps_plus <= -2,
    months_since_psplus >= -1 & months_since_psplus <= 1 & months_since_psplus != 0,
    !(ps_plus_period == FALSE & months_since_psplus == 0)
    ) %>%
  ungroup() %>%
  group_by(Game) %>%
  mutate(net_change = ps_plus_downloads - lag(ps_plus_downloads)) %>%
  filter(!is.na(net_change)) %>%
  ungroup() %>%
  summarise(mean(net_change), sd(net_change), median(net_change))

