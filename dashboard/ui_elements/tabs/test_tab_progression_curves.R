vector <-
  tibble(levels = seq(1, 10)) %>%
  mutate(sigmoid_progression = scurve(levels, ymin = 4, ymax = 5, x50L = 6, x50U = 7)) %>%
  arrange(sigmoid_progression) %>%
  mutate(levels = seq(1, 10)) %>%
  add_row(
    levels = seq(11, 20),
    sigmoid_progression= rev(.$sigmoid_progression)
    )




progression_df <-
  rep(c(
    vector, rev(vector) #do a sequence, then combine it with the opposite one
    ), times = ceiling((100/2)/10) #since we're binding 2 seqeunces, we want to repeat this half as many times (/2).
    ) %>%
  tibble::enframe(name = NULL) %>%
  mutate(
    level = seq(1, 100),
    total_time_to_level = cumsum(value),
    ) %>%
  rename(time_to_level = value)


vector <-
  tibble(levels = seq(1, 10)) %>%
  mutate(sigmoid_progression = scurve(levels, ymin = 4, ymax = 5, x50L = 6, x50U = 7)) %>%
  arrange(sigmoid_progression) %>%
  mutate(levels = seq(1, 10)) %>%
  add_row(
    levels = seq(11, 20),
    sigmoid_progression= rev(.$sigmoid_progression)
    )


   tibble(level = seq(1, 100)) %>%
    mutate(
      hours = 22 + (53-22) * exp(-.02 * level)
    ) %>%
     ggplot(aes(x = level, y = hours)) +
       geom_line()
