# Creates Table A2

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

# set correct path to folder with all CSV files
# data_folder <- "/path/to/data/folder/"

# read aggregated data
model_tbl <- read_csv(glue(data_folder, "aggregated_data-10_periods-window-1.csv"))

# read handcoded files
topic_entertainment <- read_csv(glue(data_folder, "topic_entertainment_10.csv"))
topic_politics <- read_csv(glue(data_folder, "topic_politicization_handcode_10.csv"))

# read media details files
media_details_tbl <- read_csv(glue(data_folder, "media_language.csv"))
media_label_map <- read_csv(glue(data_folder, "media_label_map.csv"))
media_ideology <- read_csv(glue(data_folder, "media_ideo.csv"))


# m1 <- felm(curr_topic_prop  ~ log_eng_sig | 
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)
# 
# m2 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop |
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)
# 
# 
# m3 <- felm(curr_topic_prop ~ last_topic_prop |
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)
# 
# m4 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + log_avg_eng_sig |
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)
# 
# m5 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)

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
  gtsave("results/tableA2-quadratic.tex")
