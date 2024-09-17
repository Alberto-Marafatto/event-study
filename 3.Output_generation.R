library(tidyverse)
library(quantmod)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)

# import data
load("Output.RData") 
announcements_Div <- output[[1]]
announcements_acquisition <- output[[2]]
announcements_buybacK <- output[[3]]
announcements_spin_off_divestiture <- output[[4]]
returns_mkt_XXX_RF <- output[[5]]

# import function
function_V1 <- readRDS("function_V1.rds")

# graphical settings
myColors <- c("red", "green", "black")
names(myColors) <- levels(as.factor(c("NEUTRAL", "GOOD", "BAD")))
colScale <- scale_colour_manual(name = "grp",values = myColors)

# generate output
# widows: from day -5 to day 5 from the event, from day -5 to day -1 from the event, from day 0 to day 5 from the event.

Wndw1_div <- function_V1(-5, 5, announcements_Div, 
                         data_check = "Dividends")
Wndw2_div <- function_V1(-5, -1, announcements_Div, 
                         data_check = "Dividends")
Wndw3_div <- function_V1(0, 5, announcements_Div, 
                         data_check = "Dividends")

Wndw1_acq <- function_V1(-5, 5, announcements_acquisition, 
                         data_check = "Other")
Wndw2_acq <- function_V1(-5, -1, announcements_acquisition, 
                         data_check = "Other")
Wndw3_acq <- function_V1(0, 5, announcements_acquisition, 
                         data_check = "Other")

Wndw1_buy <- function_V1(-5, 5, announcements_buybacK, 
                         data_check = "Other")
Wndw2_buy <- function_V1(-5, -1, announcements_buybacK, 
                         data_check = "Other")
Wndw3_buy <- function_V1(0, 5, announcements_buybacK, 
                         data_check = "Other")

Wndw1_spoff_div <- function_V1(-5, 5, announcements_spin_off_divestiture, 
                               data_check = "Other")
Wndw2_spoff_div <- function_V1(-5, -1, announcements_spin_off_divestiture, 
                               data_check = "Other")
Wndw3_spoff_div <- function_V1(0, 5, announcements_spin_off_divestiture, 
                               data_check = "Other")
output <- list(Wndw1_div,
               Wndw3_div,
               Wndw2_div,
               Wndw3_spoff_div,
               Wndw2_spoff_div,
               Wndw1_spoff_div,
               Wndw3_buy,
               Wndw2_buy,
               Wndw1_buy,
               Wndw3_acq,
               Wndw2_acq,
               Wndw1_acq)
