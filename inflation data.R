#Script to reproduce the CPI data
library(quantmod)
library(tidyverse)
library(lubridate)

getSymbols(Symbols = "CPIAUCSL", 
           src = "FRED") 

cpi <- CPIAUCSL %>%
  data.frame(date = index(.)) |> 
  remove_rownames() |> 
  mutate(year = year(date), month = month(date)) |> 
  filter(year >= 2001, year <= 2020) |> 
  mutate(avg_cpi = rollmean(CPIAUCSL, k = 12, fill = NA, align = "right")) |> 
  filter(month == 6, !is.na(avg_cpi))
