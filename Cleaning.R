library(tidyverse)
library(lubridate)
library(stringr)

# Import saved .csv
top_100_timeseries <- read_csv("Data/top_100_timeseries_monthly_2022-06-06.csv", 
                               na = c("-", "NA"))
top_100_games <- read_csv("Data/top_100_games_2022-06-06.csv")

# Cleaning
ts_cleaned <- top_100_timeseries %>%
    filter(Month != "Last 30 Days") %>%
    mutate(Month = parse_date_time(Month, orders = c("bY"))) %>%
    mutate(Rank = as_factor(Rank)) %>%
    relocate(Rank, .before = Name) %>%
    rename(Date = Month,
           Average_Players = Avg_Players,
           Gain_Average = Gain,
           Pct_Gain_Average = Pct_Gain) %>%
    group_by(Name) %>%
    mutate(
        Gain_Peak = (Peak_Players - dplyr::lead(Peak_Players, order_by = Name)),
        Pct_Gain_Peak = paste0(
            round(((Peak_Players / dplyr::lead(Peak_Players, order_by = Name) - 1)*100),
                                     digits = 2), "%")
    )
    
    

top_100_cleaned <- top_100_games %>%
    rename(Current_Players = `Current Players`,
           Peak_30_Day = `Peak Players`,
           Total_Hours_30_Day = `Hours Played`)

rm(top_100_timeseries)
rm(top_100_games)

