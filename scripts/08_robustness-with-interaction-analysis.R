# this script produces Table A5 in the Appendix.

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

topic_entertainment <- read_csv("auxiliary/topic_entertainment_60.csv")
topic_politics <- read_csv("auxiliary/topic_politicization_handcode_60.csv")

topic_politics <- topic_politics %>%
  arrange(season, topic) %>%
  rename(political = 3)%>%
  mutate(political = as.character(political))

topic_pol_ent <- topic_entertainment %>%
  inner_join(topic_politics, by = c("season", "topic"))

topic_pol_ent <- topic_pol_ent %>%
  # mutate(topic_other = ifelse(entertainment == 0 & political == 0, 1, 0)) %>%
  mutate(topic_entertainment = ifelse(entertainment == 1, 1, 0)) %>%
  mutate(topic_political = ifelse(political == 2, 1, 0)) %>% 
  mutate(final_topic = glue("{season}_{topic}"))


model_tbl <- model_tbl %>%
  inner_join(topic_pol_ent, by = "final_topic")

# m6 <- felm(curr_topic_prop ~ log_eng_sig * topic_other + log_eng_sig * topic_entertainment + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
#              media_id + window + final_topic | 0 | media_id + window,
#            data = model_tbl)


media_details_tbl <- read_csv("auxiliary/media_language.csv")
media_ideology <- read_csv("auxiliary/media_ideo.csv")
media_ideology2 <- read_csv("auxiliary/media_ideo2.csv")
media_label_map <- read_csv("auxiliary/media_label_map.csv")

media_ideology$ideo <- scale(media_ideology$ideo)
media_ideology2$ideo <- scale(media_ideology2$ideo)

media_ideology <- media_ideology %>%
  inner_join(media_label_map) %>%
  inner_join(media_details_tbl, by = c("label" = "media")) %>%
  select(n, label, short_name, ideo) %>%
  rename(media = label,
         ideo1 = ideo) %>%
  inner_join(media_ideology2) %>%
  rename(ideo2 = ideo) %>%
  # mutate(ideo1_rescaled = rescale(ideo1, to = c(0,1)),
  #        ideo2_rescaled = rescale(ideo2, to = c(0,1))) %>%
  select(n, ideo1, ideo2)

# check that all media ids are here
# the following should be TRUE

sum(!unique(model_tbl$media_id) %in% unique(media_ideology$n)) == 0

model_tbl <- model_tbl %>%
  inner_join(media_ideology, by = c("media_id" = "n"))


m8 <- felm(curr_topic_prop ~ log_eng_sig * ideo1 + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m9 <- felm(curr_topic_prop ~ log_eng_sig * ideo2 + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m10 <- felm(curr_topic_prop  ~ log_eng_sig * ideo1 + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
              media_id + window + final_topic | 0 | media_id + window,
            data = model_tbl[ model_tbl$political == 2,])

m11 <- felm(curr_topic_prop  ~ log_eng_sig * ideo2 + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
              media_id + window + final_topic | 0 | media_id + window,
            data = model_tbl[ model_tbl$political == 2,])


modelsummary(models = list("M1 Partisan Topics" = m10, "M2 Partisan Topics" = m11),
             output = "gt", stars = TRUE, statistic = c("std.error"), fmt = 4,
             coef_omit = "^topic") %>%
  gtsave(filename = glue("results/JOC/seasonal/{params$n_seasons}-seasons/models/overleaf/",
                         "tableA4.tex"))
