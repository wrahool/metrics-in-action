# this script checks the stationarity assumption of the time series

library(rjson)
library(tidyverse)
library(lubridate)
library(lfe)
library(tidymodels)
library(modelsummary)
library(gt)
library(glue)
library(ggtext)
library(cowplot)
library(ggrepel)
library(ggpubr)
library(plm)
library(tseries)

# set lag and window size
lag <- 1 # in days
window_size <- 1 # in days
start_season <- 1
end_season <- 60
all_seasons <- start_season:end_season

agg_data_folder <- "C:/Users/Subhayan/Desktop/RnR code/model_data"
model_tbl <- read_csv(glue("{agg_data_folder}/aggregated_data-60_periods-window-1.csv"))

# test for staionarity

model_tbl_z <- model_tbl %>%
  mutate(media_topic = as_factor(glue("{media_id}_{topic}")))

pdata <- pdata.frame(model_tbl_z, index=c("media_topic","window"))

adf.test(pdata$curr_topic_prop,k=1) %>%
  tidy()

