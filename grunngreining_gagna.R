# Grunngreining gagna


library(tidyverse)
library(lubridate)
library(zoo)



# -------------------------------------------------------------------------

df_monthly <- readxl::read_excel("data_recession.xlsx", sheet = 1) %>% janitor::clean_names()


df_quarterly <- readxl::read_excel("data_recession.xlsx", sheet = 2) %>% janitor:::clean_names()
df_quarterly$date <- as.yearqtr(df_quarterly$date)

df_vextir <- readxl::read_excel("data_recession.xlsx", sheet = 3) %>% 
  mutate(date = floor_date(dmy(date), "month")) %>% 
  group_by(date) %>% 
  summarise(vext_90_daga_innstbr = mean(vext_90_daga_innstbr),
            meginvextir = mean(meginvextir, na.rm = TRUE))


df_vextir <- df_vextir %>% 
  mutate(meginvextir = case_when(is.na(meginvextir) ~ vext_90_daga_innstbr,
                                 TRUE ~ meginvextir))



# -------------------------------------------------------------------------
# gdp to montly

df_quarterly <- df_quarterly %>% 
  mutate(q_month = as.numeric(substr(date, 7, 7)))

lookup_month <- data.frame(lookup = 1:4,
                           find = c(2, 5, 8, 11))

df_quarterly <- df_quarterly %>% 
  left_join(lookup_month, by =  c("q_month" = "lookup"))

df_quarterly <- df_quarterly %>% 
  mutate(month = make_date(year = substr(date, 1, 4),
                           month = find,
                           day = 1)) %>% 
  select(month, gdp)

month_seq <- seq.Date(as.Date("1970-01-01"), as.Date("2019-06-01"), by = "months")
month_seq <- tibble(months =month_seq)

gdp_monthly <- month_seq %>% 
  left_join(df_quarterly, by = c("months" = "month"))



# -------------------------------------------------------------------------


# na.spline
gdp_monthly$gdp_na_spline <- na.spline(gdp_monthly$gdp)

# na.locf
gdp_monthly$gdp_locf <- c(NA_real_, na.locf(gdp_monthly$gdp))

# na.approx (linear)
gdp_monthly$gdp_na_approx <- c(NA_real_, na.approx(gdp_monthly$gdp), NA_real_)



# -------------------------------------------------------------------------

gdp_monthly_viz <- gdp_monthly %>% 
  mutate(avg = (gdp_na_spline + gdp_locf + gdp_na_approx)/3) %>% 
  select(-gdp) %>% 
  na.omit() %>% 
  gather("measure", "value", 2:5) %>% 
  group_by(measure) %>% 
  mutate(growth = value/lag(value, 12) - 1) %>% 
  na.omit()

g_gdp <- ggplot(filter(gdp_monthly_viz, measure == "avg"),
                aes(x = months,
                    y = growth,
                    col = measure)) +
  geom_line() + 
  geom_point() + 
  geom_hline(yintercept = 0)

plotly::ggplotly(g_gdp)
