library(bcgovpond)
library(tidyverse)
library(readxl)
library(here)
library(janitor)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(plotly::layout)
clean <- function(x) {
  x <- gsub("\u00A0", " ", x)   # non-breaking space
  x <- gsub("[\t\r\n]", " ", x) # tabs / line breaks
  x <- trimws(x)
  x
}

fuzzy_right_join <- function(x, y, by_x, by_y, max_dist = 3) {
  crossing(x = x, y = y)|> #all possible pairings (becomes problematic with large tbbls)
    unnest(c(x, y), names_sep = ".") |> #flatten the paired rows into columns
    mutate(
      dist = stringdist::stringdist(
        .data[[paste0("x.", by_x)]],
        .data[[paste0("y.", by_y)]],
        method = "lv"
      )
    ) |>
    group_by(.data[[paste0("y.", by_y)]]) |>  # keep canonical side
    slice_min(dist, with_ties = FALSE) |>
    ungroup() |>
    filter(dist <= max_dist)
}

#takes the data from data_store/add_to_pond and adds it to the pond

ingest_pond()

# Employment data: THE correct industry names and codes.

employment <-  read_view("historic_lmo_ind_code.xlsx")|>
  select(code=lmo_ind_code, industry=lmo_detailed_industry, year, value=employment)|>
  group_by(code, industry)|>
  nest()|>
  rename(employment=data)

#read in the files with (potentially) incorrect names

old_forecast_wrong_names <- read_view("stokes_forecast.csv", skip = 3)|>
  filter(NOC=="#T", Industry!="All industries", `Geographic Area`=="British Columbia")|>
  select(Industry, starts_with("2"))|>
  pivot_longer(cols = starts_with("2"), names_to = "year")|>
  mutate(year=as.numeric(year))|>
  clean_names()|>
  group_by(industry)|>
  nest()|>
  rename(old_forecast=data)|>
  fuzzy_right_join(
    employment,
    by_x = "industry",
    by_y = "industry"
  )

#names (if any) should refer to same industry
old_forecast_wrong_names[old_forecast_wrong_names$x.industry!=old_forecast_wrong_names$y.industry,]

driver_data_wrong_names <- read_view("driver.xlsx")|>
  select(industry=(matches("ind") & !matches("code")), starts_with("2"))|>
  pivot_longer(cols=starts_with("2"), names_to = "year", values_to = "value")|>
  group_by(industry)|>
  nest()|>
  rename(driver_data=data)|>
  fuzzy_right_join(
    employment,
    by_x = "industry",
    by_y = "industry",
    max_dist = 2
  )

#names (if any) should refer to the same industry
driver_data_wrong_names[driver_data_wrong_names$x.industry!=driver_data_wrong_names$y.industry,]

notes_wrong_names <- read_view("notes.xlsx")|>
  select(industry=contains("name"), starts_with("2"))|>
  fuzzy_right_join(
    employment,
    by_x = "industry",
    by_y = "industry",
    max_dist = 2
  )

#names (if any) should refer to the same industry
notes_wrong_names[notes_wrong_names$x.industry!=notes_wrong_names$y.industry,]


budget_constraint <- read_view("constraint.xlsx") #no industry names

rich_fcast <- read_view("richs_forecast.xlsx")|>
  group_by(lmo_ind_code, lmo_detailed_industry, year)|>
  summarize(value=sum(employment))|>
  unite(industry, lmo_ind_code, lmo_detailed_industry, sep=": ")

census <- read_view("census_industry.xlsx")|>
  unite(industry, lmo_ind_code, lmo_detailed_industry, sep=": ")|>
  rename(value=employment)

census$industry <- clean(census$industry)

# clean up after fuzzyjoins

old_forecast <- old_forecast_wrong_names|>
  ungroup()|>
  select(x.old_forecast, y.code, y.industry)|>
  unnest(x.old_forecast)|>
  unite(industry, y.code, y.industry, sep=": ")|>
  mutate(year=as.numeric(year))

driver_data <- driver_data_wrong_names|>
  ungroup()|>
  select(x.driver_data, y.code, y.industry)|>
  unnest(x.driver_data)|>
  unite(industry, y.code, y.industry, sep=": ")|>
  mutate(year=as.numeric(year),
         mean_value=mean(value, na.rm = TRUE),
         value=if_else(mean_value<1000, value*1000, value) #if mean value < 1000 must be in 1000's
  )|>
  select(-mean_value)

notes <- notes_wrong_names|>
  select(contains("20"), y.code, y.industry)|>
  unite(industry, c(y.code, y.industry), sep=": ")|>
  pivot_longer(cols=contains("20"))|>
  mutate(value=str_replace_all(value, "-"," "),
         name = stringr::str_extract(name, "\\b\\d{4} Edition\\b"))


#write to rds files----------------

write_rds(census, here("app_data", "census.rds"))
write_rds(budget_constraint, here("app_data", "budget_constraint.rds"))
write_rds(driver_data, here("app_data", "driver_data.rds"))
write_rds(employment, here("app_data", "employment.rds"))
write_rds(notes, here("app_data", "notes.rds"))
write_rds(old_forecast, here("app_data", "old_forecast.rds"))
write_rds(rich_fcast, here("app_data", "rich_fcast.rds"))


