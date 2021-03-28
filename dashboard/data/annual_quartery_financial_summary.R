pacman::p_load('simfinR', 'tidyverse', 'gt', 'scales')

api_key <- 'hOiZRbX3aZSQUA2xBNhUBCIehtkOGBJZ'

companies <- simfinR_get_available_companies(api_key)

game_companies <-
  companies %>%
  filter(ticker %in% c(
    "EA",
    "ATVI",
    "TTWO",
    "ZNGA",
    "UBSFY",
    "GLUU",
    "NTDOY",
    "NTES",
    "TCEHY",
    "PLTK",
    "CCOEF",
    "OTGLY",
    "NEXOF",
    "NCBDF",
    "SQNXF",
    "THQQF",
    "PRXXF",
    "^GSPC"
  ))

game_companies_statments <-
  simfinR_get_fin_statements((
      game_companies %>%
      select(simId) %>%
      pull()
    ),
    type_statements = 'pl',
    periods =  c('Q1', 'Q2', 'Q3', 'Q4'),
    years = 2011:2020,
    api_key = api_key
    )

game_companies_statments %>%
  filter(acc_name %in% c(
      "Revenue",
      "Cost of revenue",
      "Gross Profit",
      "Operating Expenses"
      ),
    year >= 2019) %>%
  group_by(company_name) %>%
  mutate(
    fy_q = paste0("FY", str_sub(year, -2), "-", period)
  ) %>%
  arrange(desc(year, period, revenue)) %>%
  select(company_name, acc_name, fy_q, acc_value) %>%
  mutate_if(is.double, label_number_si(prefix = "$", accuracy = .1)) %>%
  mutate_all(as.character) %>%
  pivot_longer(-c(fy_q, company_name, acc_name))  %>%
  pivot_wider(names_from = fy_q) %>%
  gt(
    rowname_col = "company_name",
    groupname_col = "acc_name"
    ) %>%
  cols_hide(vars(name)) %>%
  tab_header(
    title = md("Quarterly Financial Summary")
    ) %>%
  tab_options(
    table.border.top.style = "none",
    table_body.border.top.style = "none",
    table_body.border.bottom.color = "#0000001A",
    table.font.size = 12,

    column_labels.font.size = 14,
    column_labels.border.top.style = "none",
    column_labels.border.bottom.style = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",

    row_group.border.right.style = "none",
    row_group.font.size = 14,
    data_row.padding = px(2)
  ) %>%
  cols_align(
    columns = vars(company_name, acc_name),
    align = "left"
    )


