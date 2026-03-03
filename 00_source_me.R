library(bcgovpond)
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(plotly::layout)

clean_normalize <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00A0", " ", x)        # non-breaking space
  x <- gsub("[\t\r\n]", " ", x)      # tabs / newlines
  x <- gsub("&", " and ", x)         # & would be replaced by " " below
  x <- tolower(x)
  x <- gsub("[^a-z]", " ", x)
  x <- gsub("\\s+", " ", x) # Collapse multiple spaces to single space
  trimws(x)
}

#takes the data from data_store/add_to_pond and adds it to the pond

ingest_pond()

# Employment data: THE correct industry names and codes.

employment <-  read_view("historic_lmo_ind_code.xlsx")|>
  select(code=lmo_ind_code, industry=lmo_detailed_industry, year, value=employment)|>
  group_by(code, industry)|>
  nest()|>
  rename(employment=data)|>
  mutate(.key_norm=clean_normalize(industry))

#read in the files with (potentially) incorrect names

old_forecast <- read_view("stokes_forecast.csv", skip = 3)|>
  filter(NOC=="#T", Industry!="All industries", `Geographic Area`=="British Columbia")|>
  select(Industry, starts_with("2"))|>
  pivot_longer(cols = starts_with("2"), names_to = "year")|>
  mutate(year=as.numeric(year))|>
  clean_names()|>
  group_by(industry)|>
  nest()|>
  rename(old_forecast=data)|>
  mutate(.key_norm=clean_normalize(industry))|>
  inner_join(employment, by = join_by(".key_norm"))|>
  select(code, industry=industry.y, old_forecast)|>
  unnest(old_forecast)|>
  unite(industry, code, industry, sep=": ")|>
  mutate(year=as.numeric(year))

driver_data <- read_view("driver.xlsx")|>
  select(industry=(matches("ind") & !matches("code")), starts_with("2"))|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "value")|>
  group_by(industry)|>
  nest()|>
  rename(driver_data=data)|>
  mutate(.key_norm=clean_normalize(industry))|>
  inner_join(employment, by = join_by(".key_norm"))|>
  select(code, industry=industry.y, driver_data)|>
  unnest(driver_data)|>
  unite(industry, code, industry, sep=": ")|>
  mutate(year=as.numeric(year),
         mean_value=mean(value, na.rm = TRUE),
         value=if_else(mean_value<1000, value*1000, value) #if mean value < 1000 must be in 1000's
  )|>
  select(-mean_value)

notes <- read_view("notes.xlsx")|>
  select(industry=contains("name"), starts_with("2"))|>
  mutate(.key_norm=clean_normalize(industry))|>
  inner_join(employment, by = join_by(".key_norm"))|>
  select(contains("20"), code, industry=industry.y)|>
  unite(industry, code, industry, sep=": ")|>
  pivot_longer(cols=contains("20"))|>
  mutate(value=str_replace_all(value, "-"," "),
         name = stringr::str_extract(name, "\\b\\d{4} Edition\\b"))

census <- read_view("census_industry.xlsx")|>
  mutate(.key_norm=clean_normalize(lmo_detailed_industry))|>
  inner_join(employment, by = join_by(".key_norm"))|>
  select(code, industry, value=employment.x)|>
  unite(industry, code, industry, sep=": ")

employment <- employment|>
  unnest(employment)|>
  unite(industry, code, industry, sep=": ")|>
  mutate(year=as.numeric(year))|>
  select(-.key_norm)

budget_constraint <- read_view("constraint.xlsx") #no industry names

rich_fcast <- read_view("richs_forecast.xlsx")|>
  group_by(lmo_ind_code, lmo_detailed_industry, year)|>
  summarize(value=sum(employment))|>
  unite(industry, lmo_ind_code, lmo_detailed_industry, sep=": ")


#write to rds files----------------

write_rds(census, here("app_data", "census.rds"))
write_rds(budget_constraint, here("app_data", "budget_constraint.rds"))
write_rds(driver_data, here("app_data", "driver_data.rds"))
write_rds(employment, here("app_data", "employment.rds"))
write_rds(notes, here("app_data", "notes.rds"))
write_rds(old_forecast, here("app_data", "old_forecast.rds"))
write_rds(rich_fcast, here("app_data", "rich_fcast.rds"))


