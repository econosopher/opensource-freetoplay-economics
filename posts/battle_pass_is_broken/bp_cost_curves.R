pacman::p_load(tidyverse, stringr, purrr, scales)

xp_per_level <- c(
  100,
  200,
  300,
  400,
  750,
  850,
  950,
  1050,
  1150,
  1250,
  1350,
  1450,
  1550,
  1650,
  1750,
  1850,
  1950,
  2050,
  2150,
  2300,
  2450,
  2600,
  2750,
  2900,
  3050,
  3200,
  3350,
  3500,
  3650,
  3800,
  3950,
  4100,
  4250,
  4400,
  4550,
  4700,
  4850,
  5000,
  5150,
  5300,
  5500,
  5700,
  5900,
  6100,
  6300,
  6500,
  6700,
  6900,
  7100,
  7300,
  7600,
  7900,
  8200,
  8500,
  8800,
  9100,
  9400,
  9700,
  10000,
  10300,
  10700,
  11100,
  11500,
  11900,
  12300,
  12700,
  13100,
  13500,
  13900,
  14300,
  14800,
  15300,
  15800,
  16300,
  16800,
  17300,
  17800,
  18300,
  18800,
  19300,
  20300,
  21300,
  22300,
  23300,
  24300,
  25300,
  26300,
  27300,
  28300,
  30300,
  32300,
  34300,
  36300,
  38300,
  40300,
  43300,
  46300,
  49300,
  52300
 )

tiers <- seq(1, 99)

xp_per_1.50 <- seq(1000, 10000, by = 1000)

total_cost_varation <- data.frame()

for(xp in xp_per_150){

  cost_sequence <-
  data.frame(tiers, xp_per_level) %>%
    mutate(
      xp_per_1.50 = xp,
      price_per_tier = xp_per_level/xp_per_1.50,
      total_cost = sum(price_per_tier)
    ) %>%
    slice(1) %>%
    select(xp_per_1.50, total_cost)

  total_cost_varation <- bind_rows(total_cost_varation, cost_sequence)

}

total_cost_varation %>%
ggplot(aes(x = xp_per_1.50, y = total_cost)) +
  geom_line() +
  labs(title = "Total Tier Costs based on Differing XP Costs", x = "Amount of XP Purchased for $1.50", y = "Total Battle Pass Tier Cost") +
  scale_y_continuous(label = dollar) +
  scale_x_continuous(label = comma) +
  geom_hline(yintercept = 148.5)
