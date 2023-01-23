#this script loads the observed data, the old and new forecasts and creates output files:
# 1:  a wide csv file with observed, old and new forecasts, and their CAGRs.
# 2: a pdf document with plots showing the time series for observed, old and new + CAGRs
#libraries------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(scales)
library(aest)
library(assertthat)
library(ggpmisc)
library(ggpp)
#constants------------
historic_start <- 2010 #post recession
historic_end <- 2019 #pre covid
bcPalette <- c("#1f4181", "#fabc29", "#000000")
# functions-----------------
read_pivot_clean <- function(pattern, skip) {
  read_excel(here("data", list.files(here("data"), pattern = pattern)), skip = skip) %>%
    rename(
      code = contains("code"),
      industry = contains("ind") & !contains("code")
    ) %>%
    pivot_longer(cols = -c(industry, code), names_to = "year", values_to = "value") %>%
    clean_names() %>%
    mutate(year = as.numeric(year)) %>%
    unite(industry, code, industry, sep = ": ")
}
cagr_percent <- function(df, start_year, end_year, thing){
  assert_that(is.data.frame(df))
  assert_that(end_year > start_year)
  employment_start <- df%>%
    filter(year == start_year & series == thing)%>%
    pull(value)
  employment_end <- df%>%
    filter(year == end_year &  series == thing)%>%
    pull(value)
  cagr <- round(((employment_end/employment_start)^(1/(end_year-start_year))-1)*100, 2)
  tibble(start_year, end_year, cagr)
}
get_old_cagr <- function(df){
  bind_rows(cagr_percent(df, historic_start, historic_end, "Historical"),
            cagr_percent(df, forecast_start-1, forecast_start+4, last_years_forecast),
            cagr_percent(df, forecast_start+4, forecast_start+9, last_years_forecast),
            cagr_percent(df, forecast_start-1, forecast_start+9, last_years_forecast))%>%
    rename(cagr_last_year = cagr)
}
get_new_cagr <- function(df){
  bind_rows(cagr_percent(df, historic_start, historic_end, "Historical"),
            cagr_percent(df, forecast_start, forecast_start+5, this_years_forecast),
            cagr_percent(df, forecast_start+5, forecast_start+10, this_years_forecast),
            cagr_percent(df, forecast_start, forecast_start+10, this_years_forecast))%>%
    rename(cagr_this_year = cagr)
}

new_plot <- function(group, tbbl, old_cagr, new_cagr){
  colnames(old_cagr) <- c("Start","End", paste0(last_years_forecast,": CAGR"))
  colnames(new_cagr) <- c("Start","End", paste0(this_years_forecast,": CAGR"))
  ggplot()+ #with geom_table_npc need to start with empty plot
    geom_table_npc(mapping = aes(npcx = c(.05,.95), #add the tables
                                 npcy = c(.05,.05),
                                 label = list(old_cagr, new_cagr)))+
    geom_line(data=tbbl, mapping=aes(year,value, colour=series), linewidth=2, alpha=.5)+
    scale_y_continuous(labels=scales::comma)+
    scale_colour_manual(values = bcPalette)+
    labs(x="",y="Employment",colour="", title=group)
}

# read in the data--------------------------
employment <- read_pivot_clean("Employment", 2)%>%
  mutate(series = "Historical")
old_forecast <- read_pivot_clean("Industry Forecast", 0)%>%
  filter(industry!="ind08: Construction")
forecast_already <- read_csv(here("out", "forecasts.csv")) %>%
  group_by(industry, year) %>%
  summarize(value = last(value)) # only the most recent forecast

# variables--------------------
forecast_start <- min(forecast_already$year)
last_years_forecast <- paste("Forecast", forecast_start-1)
this_years_forecast <- paste("Forecast", forecast_start)
col_names_last_year <- c("Start Year", "End Year", "CAGR", paste0("LMO ", forecast_start-1))
col_names_this_year <- c(paste0("LMO ", forecast_start), "CAGR", "Start Year", "End Year")

#add series labels to dataframes
lmo_observed <- employment%>%
  mutate(series = "Historical")
lmo_old_forecast <- old_forecast%>%
  mutate(series = last_years_forecast)
lmo_new_forecast <- forecast_already%>%
  mutate(series = this_years_forecast)

# row bind the long format dataframes-----
long <- bind_rows(lmo_observed, lmo_old_forecast, lmo_new_forecast)%>%
  mutate(value = round(value))
# convert it to wide format-------
wide <- long%>%
  pivot_wider(id_cols = c(industry, series), names_from = year, values_from = value)%>%
  arrange(industry, series)
#nest the long format dataframe by industry------
nested <- long%>%
  filter(industry != "ind08: Construction")%>%
  group_by(industry)%>%
  nest()%>%
  mutate(new_cagr = map(data, get_new_cagr),
         old_cagr = map(data, get_old_cagr),
         plots = pmap(list(industry, data, old_cagr, new_cagr), new_plot)
        )
# create a new dataframe with the CAGRs of the new forecast-------
new_cagr <- nested%>%
  unnest(new_cagr)%>%
  filter(start_year != historic_start)%>%
  select(industry, start_year, end_year, cagr_this_year)%>%
  mutate(series = this_years_forecast,
         period = case_when(start_year == forecast_start & end_year == forecast_start+5 ~ "1st 5 year CAGR",
                          start_year == forecast_start+5 & end_year == forecast_start+10 ~ "2nd 5 year CAGR",
                          start_year == forecast_start & end_year == forecast_start+10 ~ "10 year CAGR",
                          ))%>%
  select(industry, series, period, cagr_this_year)%>%
  pivot_wider(id_cols = c(industry, series), names_from = period, values_from = cagr_this_year)
# create a wide format dataframe with the CAGRs of the old forecast-------
old_cagr <- nested%>%
  unnest(old_cagr)%>%
  filter(start_year != historic_start)%>%
  select(industry, start_year, end_year, cagr_last_year)%>%
  mutate(series = last_years_forecast,
         period = case_when(start_year == forecast_start-1 & end_year == forecast_start+4 ~ "1st 5 year CAGR",
                          start_year == forecast_start+4 & end_year == forecast_start+9 ~ "2nd 5 year CAGR",
                          start_year == forecast_start-1 & end_year == forecast_start+9 ~ "10 year CAGR",
         ))%>%
  select(industry, series, period, cagr_last_year)%>%
  pivot_wider(id_cols = c(industry, series), names_from = period, values_from = cagr_last_year)
#join the wide format original dataframe with the wide format CAGRs (note that the CAGRs get their own columns so fix)----
wide <- full_join(wide, new_cagr, by = c("industry", "series"))
wide <- full_join(wide, old_cagr, by = c("industry", "series"))%>%
  mutate(`1st 5 year CAGR` = ifelse(is.na(`1st 5 year CAGR.x`), `1st 5 year CAGR.y`, `1st 5 year CAGR.x`),
         `2nd 5 year CAGR` = ifelse(is.na(`2nd 5 year CAGR.x`), `2nd 5 year CAGR.y`, `2nd 5 year CAGR.x`),
         `10 year CAGR` = ifelse(is.na(`10 year CAGR.x`), `10 year CAGR.y`, `10 year CAGR.x`))%>%
  select(-ends_with(".x"), -ends_with(".y"))
# Create the pdf (plot) output--------
pdf(here("out", paste0("LMO_", forecast_start, "_industry_forecast.pdf")), onefile = TRUE, height=8.5, width=11)
nested%>%
  select(plots)%>%
  walk(print)
dev.off()
# write the wide format data to disk--------
write_csv(wide, here("out", paste0("LMO_", forecast_start, "_industry_forecast.csv")), na ="")
