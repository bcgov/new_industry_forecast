#this script loads the observed data, the old and new forecasts and creates output files:
# 1:  a wide csv file with observed, old and new forecasts, and their CAGRs.
# 2: a pdf document with plots showing the time series for observed, old and new + CAGRs
#libraries------
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(scales)
library(ggpmisc)
library(patchwork)
library(ggpp)
#library(bcgovpond)
library(conflicted)
# functions-----------------

#calls function get_cagr 3 times for the 3 time periods of interest
get3cagrs <- function(tbbl, series, offset){
  bind_rows(first_five_years = get_cagr(tbbl, forecast_start-offset, forecast_start+5-offset, series),
            second_five_years = get_cagr(tbbl, forecast_start+5-offset, forecast_start+10-offset, series),
            ten_year = get_cagr(tbbl, forecast_start-offset, forecast_start+10-offset, series)
  )
}
#calculates the cagr for a given start, end, and series
get_cagr <- function(tbbl, start, end, series){
  start_val <- tbbl$value[tbbl$series==series & tbbl$year==start]
  end_val <- tbbl$value[tbbl$series==series & tbbl$year==end]
  cagr <- scales::percent((end_val/start_val)^(1/(end-start))-1, accuracy = .01)
  tibble(start=start, end=end, cagr=cagr)
}
#creates a combination plot and tables-----------------------
make_plot <- function(tbbl, industry, old, last, raw, bend, continue){
   #the main plot
   plt <- ggplot()+
     geom_line(data=tbbl, mapping=aes(year,value, colour=series), lwd=1.5)+
     scale_y_continuous(labels=scales::comma)+
     labs(x="",y="Employment",colour="", title=industry)+
     theme_minimal(base_size = 15)
#   table for historical cagr
   old <- ggplot() +
     theme_void() +
     annotate(geom = "table",
              x = 1,
              y = 1,
              label = list(old))+
     labs(title="historical")
#   # 3 cagr table for last year's forecast (which possibly doesnt exist)
   if(nrow(last)==0){
     last <- ggplot()+
       theme_void() +
       labs(title="No forecast last year")
   }else{
     last <- ggplot() +
       theme_void() +
       annotate(geom = "table",
                x = 1,
                y = 1,
                label = list(last))+
       labs(title="last year")
   }
   # 3 cagr table for this year's raw forecast
   raw <- ggplot() +
     theme_void() +
     annotate(geom = "table",
              x = 1,
              y = 1,
              label = list(raw))+
     labs(title="raw")
   # 3 cagr table for bend adjusted
   bend <- ggplot() +
     theme_void() +
     annotate(geom = "table",
              x = 1,
              y = 1,
              label = list(bend))+
     labs(title="bend")
   # 3 cagr table for continue adjusted
   continue <- ggplot() +
     theme_void() +
     annotate(geom = "table",
              x = 1,
              y = 1,
              label = list(continue))+
     labs(title="continue")
   #layout the plots (patchwork)
   plt/(old+last+raw+bend+continue)+
     plot_layout(heights = c(2,1))
 }

# unnests cagrs and makes wider.------------------
unnest_cagrs <- function(tbbl, nest, series){
  tbbl%>%
    unnest({{  nest  }})%>%
    select(industry, start, end, cagr)%>%
    mutate(series=series)%>%
    unite(period, start, end, sep = "-")%>%
    pivot_wider(names_from = period, values_from = cagr, names_prefix = "CAGR: ")
}
# read in the data--------------------------
employment<- read_rds(here("app_data", "employment.rds"))|>
  unite("industry", "code", "industry", sep=": ")
#calculate some "constants"
historic_end <- max(employment$employment[[1]]$year)
historic_start <- historic_end-10

old_forecast<- read_rds(here("app_data", "old_forecast.rds"))|>
  group_by(industry)|>
  nest()

#the raw forecast data------------------
forecast_already <- read_csv(here("out","current", "forecasts.csv")) %>%
  group_by(industry, year) %>%
  summarize(value = last(value))# only the most recent forecast (industry already in "code: name" format)

#Forecasts must equal the constraint (first 5 years)-----------------
constraint <- read_rds(here("app_data", "budget_constraint.rds"))%>%
  rename(constraint=employment)

forecast_totals <- forecast_already%>%
  group_by(year)%>%
  summarise(forecast=sum(value))


# raw forecasts are adjusted to match either by bend or continue method.---------------
adjustment <- full_join(forecast_totals, constraint)%>%
  mutate(forecast_over_constraint=forecast/constraint,
         bend=forecast_over_constraint,
         continue=forecast_over_constraint)%>%
  fill(bend, .direction="down")#the bend method continues scaling at the same rate as the last value of forecast/constraint
#the continue adjustment follows the trend in the forecast/constraint beyond its last value
unconstrained_years <- (max(constraint$year)+1):(max(constraint$year)+6)
adjustment$continue[6:11] <- predict(lm(forecast_over_constraint~year, data=adjustment), newdata = tibble(year=unconstrained_years))
#take a look at the adjustments
View(adjustment)
#attach the adjustment info to the raw forecasts-----------------
forecast_with_adj <- adjustment%>%
  select(year, bend, continue)%>%
  full_join(forecast_already)
# variables--------------------
forecast_start <- min(forecast_already$year)
last_years_forecast <- paste("Forecast", forecast_start-1)
#add series labels to dataframes-------------
lmo_observed <- employment%>%
  mutate(series = "Historical")
lmo_old_forecast <- old_forecast%>%
  mutate(series = paste("Final forecast", forecast_start-1, sep=": "))
lmo_raw_forecast <- forecast_already%>%
  mutate(series = paste("Raw Forecast", forecast_start, sep=": "))
lmo_bend <- forecast_with_adj%>%
  mutate(value=value/bend)%>%
  select(industry,year,value)%>%
  mutate(series=paste("Bend adjustment",forecast_start, sep = ": "))
lmo_continue <- forecast_with_adj%>%
  mutate(value=value/continue)%>%
  select(industry,year,value)%>%
  mutate(series=paste("Continue adjustment",forecast_start, sep = ": "))
# row bind the long format dataframe, relevel series so they can be arranged below--------------
long <- bind_rows(lmo_observed, lmo_old_forecast, lmo_raw_forecast,lmo_bend, lmo_continue)%>%
  mutate(value = round(value))%>%
  mutate(series=fct_relevel(series,
                            "Historical",
                            paste("Final forecast", forecast_start-1, sep=": "),
                            paste("Raw Forecast",forecast_start, sep=": "),
                            paste("Bend adjustment",forecast_start, sep = ": "),
                            paste("Continue adjustment",forecast_start, sep = ": ")
                            )
         )

#aggregate and add in as ind00: All

all <- long|>
  group_by(year, series)|>
  summarize(value=sum(value))|>
  mutate(industry="ind00: All")

# convert it to wide format-------
wide <- bind_rows(all, long)%>%
  filter(year>2009)%>%
  pivot_wider(id_cols = c(industry, series), names_from = year, values_from = value)%>%
  arrange(industry, series)
#nest the long format dataframe by industry, calculate cagrs, make plots----------------
nested <- bind_rows(all, long)%>%
  group_by(industry)%>%
  nest()%>%
  mutate(historical_cagr=map(data, get_cagr, historic_start, historic_end, "Historical"),
         last_years_cagrs=map(data, get3cagrs, paste("Final forecast", forecast_start-1, sep=": "), offset=1),
         raw_cagrs=map(data, get3cagrs, paste("Raw Forecast", forecast_start, sep=": "), offset=0),
         bend_cagrs=map(data, get3cagrs, paste("Bend adjustment",forecast_start, sep = ": "), offset=0),
         continue_cagrs=map(data, get3cagrs, paste("Continue adjustment",forecast_start, sep = ": "), offset=0),
         plots=pmap(list(data, industry, historical_cagr, last_years_cagrs, raw_cagrs, bend_cagrs, continue_cagrs), make_plot)
         )
#extract the cagrs--------------
historical_cagr <- nested%>%
  unnest(historical_cagr)%>%
  select(industry, "CAGR: {historic_start}-{historic_end}":=cagr)%>%
  mutate(series="Historical")
last_years_cagrs <- unnest_cagrs(nested, last_years_cagrs, paste("Final forecast", forecast_start-1, sep=": "))
raw_cagrs <- unnest_cagrs(nested, raw_cagrs, paste("Raw Forecast", forecast_start, sep=": "))
bend_cagrs <- unnest_cagrs(nested, bend_cagrs, paste("Bend adjustment",forecast_start, sep = ": "))
continue_cagrs <- unnest_cagrs(nested, continue_cagrs, paste("Continue adjustment",forecast_start, sep = ": "))
current_cagrs <- bind_rows(raw_cagrs, bend_cagrs, continue_cagrs)
#join the extracted cagrs with the wide data--------------
all_the_data <- wide%>%
  full_join(historical_cagr)%>%
  full_join(last_years_cagrs)%>%
  full_join(current_cagrs)
# Create the pdf (plot) output--------
pdf(here("out","current", paste0("LMO_", forecast_start, "_industry_forecast.pdf")), onefile = TRUE, height=8.5, width=11)
nested%>%
  select(plots)%>%
  walk(print)
dev.off()
# write the wide format data to disk--------
write_csv(all_the_data, here("out","current", paste0("LMO_", forecast_start, "_industry_adjustments.csv")), na ="")

#prepare file to send to stokes (keep either the continue or bend adjusted)
numbers <- all_the_data|>
  filter(str_detect(series, "Historical|Continue"))|>
  select(-series)|>
  select(industry, starts_with("2"))|>
  pivot_longer(cols=starts_with("2"))|>
  filter(!is.na(value))|>
  pivot_wider()

cagrs <- all_the_data|>
  filter(str_detect(series, "Historical|Continue"))|>
  select(-series)|>
  select(industry, starts_with("C"))|>
  pivot_longer(cols=starts_with("C"))|>
  filter(!is.na(value))|>
  pivot_wider()

full_join(numbers, cagrs)|>
  write_csv(here("out","current", paste0("LMO_", forecast_start, "_industry_forecasts_for_stokes.csv")), na ="")


