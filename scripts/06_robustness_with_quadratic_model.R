# this script produces Table A2 in the Appendix (check last digits)

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


# set lag and window size
lag <- 1 # in days
window_size <- 1 # in days
start_season <- 1
end_season <- 60
all_seasons <- start_season:end_season

agg_data_folder <- "C:/Users/Subhayan/Desktop/RnR code/model_data"
model_tbl <- read_csv(glue("{agg_data_folder}/aggregated_data-60_periods-window-1.csv"))


model_tbl <- model_tbl %>%
  mutate(log_eng_sig2 = log_eng_sig^2) %>%
  mutate(log_eng_sig2c = log_eng_sig2 - mean(log_eng_sig2, na.rm = T))


m2 <- felm(curr_topic_prop ~ log_eng_sig + log_eng_sig2c + last_topic_prop |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m3 <- felm(curr_topic_prop ~ last_topic_prop + log_eng_sig2c |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m4 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + log_avg_eng_sig + log_eng_sig2c |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m5 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig + log_eng_sig2c |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

###########

topic_entertainment <- read_csv("auxiliary/topic_entertainment_60.csv")
topic_politics <- read_csv("auxiliary/topic_politicization_handcode_60.csv")

topic_politics <- topic_politics %>%
  arrange(season, topic) %>%
  rename(political = 3)%>%
  mutate(political = as.character(political))

topic_pol_ent <- topic_entertainment %>%
  inner_join(topic_politics, by = c("season", "topic"))

topic_pol_ent <- topic_pol_ent %>%
  mutate(topic_other = ifelse(entertainment == 0 & political == 0, 1, 0)) %>%
  mutate(topic_entertainment = ifelse(entertainment == 1, 1, 0)) %>%
  mutate(topic_political = ifelse(political == 2, 1, 0)) %>% 
  mutate(final_topic = glue("{season}_{topic}"))


model_tbl <- model_tbl %>%
  inner_join(topic_pol_ent, by = "final_topic")

m6 <- felm(curr_topic_prop ~ log_eng_sig * topic_other + log_eng_sig * topic_entertainment + last_topic_prop + last_topic_prop_all + log_avg_eng_sig + log_eng_sig2c  |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m7 <- felm(curr_topic_prop ~ log_eng_sig * topic_political + log_eng_sig * topic_entertainment + last_topic_prop + last_topic_prop_all + log_avg_eng_sig + log_eng_sig2c |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

modelsummary(models = list(m2, m4, m5, m7),
             output = "gt", stars = TRUE, statistic = c("std.error"), fmt = 4,
             coef_omit = "^topic") %>%
  gtsave(filename = "results/tableA2.tex")
