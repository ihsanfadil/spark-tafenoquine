
# exploratory data analysis

library(tidyverse)
library(here)
library(haven)
library(readxl)
library(lubridate)

raw_sismal_1920 <- here('0-data',
                        'esismal 2019-2020_ieprocess210801.dta') |>
  read_dta()
raw_sismal_21 <- here('0-data',
                      '2021Regmal1_cleaning_20220905.xlsx') |>
  read_excel()

mimika_1920 <- raw_sismal_1920 |>
  filter(nama_kabupaten == 'MIMIKA') |> 
  select(nama_fasyankes, tglkun, umur_th, jnskel) |> 
  mutate(nama_fasyankes = str_to_lower(nama_fasyankes),
         tglkun = dmy(tglkun),
         umur_th = round(umur_th),
         jnskel = if_else(jnskel == 'L', 'Male', 'Female') |> factor()) |> 
  rename(health_unit = nama_fasyankes,
         date = tglkun,
         age = umur_th,
         sex = jnskel)

mimika_21 <- raw_sismal_21 |>
  rename(nama_kabupaten = nama_kabupaten...7) |> 
  filter(nama_kabupaten == 'MIMIKA') |> 
  select(nama_fasyankes, tglkunj, umur, jnskel) |> 
  mutate(nama_fasyankes = str_to_lower(nama_fasyankes),
         tglkunj = ymd(tglkunj),
         umur = round(umur),
         jnskel = if_else(jnskel == 'L', 'Male', 'Female') |> factor()) |>
  rename(health_unit = nama_fasyankes,
         date = tglkunj,
         age = umur,
         sex = jnskel)

mimika_1921 <- bind_rows(mimika_1920, mimika_21)

mimika_1921 |> 
  mutate(year = year(date) |> factor()) |> 
  group_by(health_unit, year) |> 
  count() |> 
  arrange(desc(year), desc(n)) |> 
  write_csv(file = here('0-data', 'highest.csv')) # puskesmas pasar sentral

mimika_highest_1921 <-
  mimika_1921 |> 
  filter(health_unit == 'puskesmas pasar sentral') |> 
  mutate(year = year(date) |> factor(),
         week = isoweek(date))

# weekly time series
mimika_highest_1921 |> 
  ggplot(aes(x = week)) + 
  geom_bar() +
  facet_grid(rows = vars(year)) +
  scale_x_continuous(breaks = seq(1, 49, by = 4)) +
  labs(x = 'N-th week',
       y = 'Number of malaria cases (weekly)',
       title = 'Puskesmas Pasar Sentral',
       subtitle = 'Top health unit in Mimika reporting malaria cases')
ggsave('weekly.png', path = here('0-graph'))

# daily time series
mimika_highest_1921 |> 
  ggplot(aes(x = date)) + 
  geom_bar() +
  scale_x_date(date_breaks = '1 month', date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = 'N-th day',
       y = 'Number of malaria cases (daily)',
       title = 'Puskesmas Pasar Sentral',
       subtitle = 'Top health unit in Mimika reporting malaria cases')
ggsave('daily.png', path = here('0-graph'), width = 10, units = 'in')









