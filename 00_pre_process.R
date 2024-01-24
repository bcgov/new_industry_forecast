#strips down stokes employment file down to what we need.

library(tidyverse)
library(here)
library(vroom)
library(janitor)

old_forecast <- vroom::vroom(here("data","current","LMO_2023_employment.csv"), skip = 3)|>
  janitor::remove_constant()|>
  filter(NOC=="#T",
         Industry!="All industries",
         `Geographic Area`=="British Columbia")|>
  select(-NOC,-Description,-Variable, -`Geographic Area`)|>
  pivot_longer(cols=-Industry, names_to = "year")%>%
  mutate(year=as.numeric(year))%>%
  clean_names()%>%
  group_by(industry)%>%
  nest()%>%
  rename(old_forecast=data)

write_rds(old_forecast, here("data","current","old_forecast.rds"))
