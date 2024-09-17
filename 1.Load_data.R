#library(readxl)
#library(lubridate)
#library(readr)
library(tidyverse)
library(quantmod)
#library(ggplot2)
#library(ggmosaic)
#library(RColorBrewer)
#library(kableExtra)

# Load the data

announcements <- readxl::read_xlsx("events.xlsx", sheet = 1, skip = 2)

names(announcements) <- c("action_type", "Declared_Date", 
                          "Summary5", "NEWS")
announcements <- announcements |> 
  mutate(Declared_Date = lubridate::as_date(Declared_Date))
announcements <- announcements[!grepl("Execution", announcements$Summary5), ]

################################################################################
################################################################################
# JUST DIVIDENDS
announcements_Div <- announcements[grepl("Dividend", announcements$action_type), ]
# JUST ACQUISITIONS
announcements_acquisition <- announcements[grepl("Acquisition", announcements$action_type), ]
# JUST BUYBACK
announcements_buybacK <- announcements[grepl("Buyback", announcements$action_type), ]
# Divesture and spin-off
announcements_spin_off_divestiture <- announcements[grepl("Spin-off", announcements$action_type) | grepl("Divestiture", announcements$action_type), ]
################################################################################
################################################################################

# download the CAPM
ff_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_Daily_CSV.zip"
temp_file <- tempfile()
download.file(ff_url, temp_file)
ff_factors_raw_data <- unzip(temp_file)
ff_factors_raw_data <- read_csv(ff_factors_raw_data,skip = 4)
ff_factors_raw_data <- ff_factors_raw_data[-nrow(ff_factors_raw_data),]
ff_factors_raw_data <- ff_factors_raw_data |> 
  mutate(date = as_date(`...1`)) |> 
  select(-c(`...1`, "SMB", "HML")) |> 
  relocate(date, .before = "Mkt-RF") |> 
  mutate(`Mkt-RF` = `Mkt-RF` / 100,
         RF = RF / 100)

# download the data for company XXX 
getSymbols('XXX',src='yahoo')
daily_XXX <- dailyReturn(XXX) |> 
  as.data.frame() 
daily_XXX <- daily_XXX |> 
  mutate(date = as_date(rownames(daily_XXX)))

# merge ff and company returns 
returns_mkt_XXX_RF <- inner_join(daily_XXX, ff_factors_raw_data, by = "date") |> 
  relocate(daily.returns, .after = "date") |> 
  mutate(daily.returns.rf = daily.returns - RF) |> 
  relocate()

# convert from return to log returns
returns_mkt_XXX_RF[,2:5] <- log(returns_mkt_XXX_RF[,2:5] + 1)

output <- list(announcements_Div,
               announcements_acquisition,
               announcements_buybacK,
               announcements_spin_off_divestiture,
               returns_mkt_XXX_RF)

save(output, file = "Output.RData")
