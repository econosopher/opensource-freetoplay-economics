pacman::p_load(tidyverse, stringr, purrr, scales, StatsBombR, extrafont)

fonts()
font_import(prompt = TRUE)
fonts()

base_df <-
  data.frame(
    utility = c(1, 4, 4, 2),
    time = c(1, 2, 3, 3)
    )

base_df %>%
ggplot(aes(x = time, y = utility)) +
  geom_point(shape = 21, size = 2, stroke = 0.4) +
  labs(x = "time", y = "utility") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_x_continuous(limits = c(0, 10)) +
  # theme_SB() +
  annotate(geom = 'curve', x = 4, y = 4, xend = 3.2, yend = 4.1,
           curvature = .3, color = 'darkblue', arrow = arrow(length = unit(2, 'mm'))) +
  annotate("text", x = 1.3, y = .8, label = 'C[1]', parse = TRUE) +
  annotate("text", x = 2.3, y = 3.8, label = 'C[2]', parse = TRUE) +
  annotate("text", x = 3.3, y = 3.8, label = 'C[2]', parse = TRUE) +
  annotate("text", x = 3.3, y = 1.8, label = 'C[3]', parse = TRUE) +
  annotate(geom = 'curve', x = 4, y = 2, xend = 3.2, yend = 2.1,
           curvature = .3, color = 'darkblue', arrow = arrow(length = unit(2, 'mm'))) +

  geom_segment(aes(x = 0, xend = .9, y = 1, yend = 1), linetype = "dashed") +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = .9), linetype = "dashed") +

  geom_segment(aes(x = 0, xend = 1.9, y = 4, yend = 4), linetype = "dashed") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 3.9), linetype = "dashed")

  geom_segment(aes(x = 0, xend = 1.9, y = 4, yend = 4), linetype = "dashed") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 3.9), linetype = "dashed")

    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
      )
