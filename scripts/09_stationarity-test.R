# this script tests the stationarity assumption

library(rjson)
library(tidyverse)
library(lubridate)
library(lfe)
library(tseries)
library(plm)
library(tidymodels)
library(modelsummary)
library(gt)
library(glue)
library(ggtext)
library(cowplot)
library(ggrepel)
library(ggpubr)
library(plm)

setwd("C:/Users/Subhayan/Work/media-engagement/")

params <- fromJSON(file = "params/params_joc_60-seasons.json")

list.files(glue("{params$topic_models_results_folder}final/topic_model/seasonal/{params$n_seasons}-seasons/modeling"))

all_seasons <- 1:params$n_seasons

season_topic_tbl <- read_csv(glue("auxiliary/optimal-topics-per-season_cv_{params$n_seasons}-seasons.csv"))

# set lag and window size
lag <- 1 # in days
window_size <- 1 # in days
start_season <- 1
end_season <- params$n_seasons

model_tbl <- NULL
for(s in all_seasons[start_season:end_season]) {
  
  n_topics <- season_topic_tbl %>%
    filter(season == s) %>%
    pull(k)
  
  s_dat <- read_csv(glue('{params$topic_models_results_folder}final/topic_model/seasonal/',
                         '{params$n_seasons}-seasons/modeling/',
                         'season-{s}_{n_topics}-topics-modeling-data.csv'))
  
  s_dat <- s_dat %>%
    mutate(season = s)  %>%
    select(media_id, id, everything()) %>%
    rename(day = date)
  
  # create a new column "window" which can be 1 day, 2 days ... 7 days
  s_dat <- s_dat %>%
    mutate(window = ceiling(day/window_size)) %>%
    select(media_id, id, window, everything())
  
  
  # for each window, for each media outlet,
  # calculate the frequency of posts
  post_freq <- s_dat %>%
    select(media_id, id, window) %>%
    group_by(media_id, window) %>%
    summarize(post_freq = n())
  
  ## for each window, for all outlets,
  # calculate the frequency of posts
  post_freq_all <- s_dat %>%
    select(id, window) %>%
    group_by(window) %>%
    summarize(post_freq = n())
  
  # for each window, for each media outlet,
  # calculate the total frequency of each topic
  topic_freq_tbl <- s_dat %>%
    select(media_id, window, paste0("topic", 1:n_topics)) %>%
    group_by(media_id, window) %>%
    # summarize_at(paste0("topic", 1:n_topics), sum) %>% # sum the probabilities of all topics per outlet per window
    summarize(across(starts_with("topic"), ~ sum(.x, na.rm = TRUE))) %>%
    rename_with(temp <- function(x) { # rename topic1 => topic1_freq, topic2 => topic2_freq
      paste0(x, "_freq")
    }, starts_with("topic"))
  
  all_topic_freq_cols <- paste0("topic", 1:n_topics, "_freq")
  
  topic_freq_long_tbl <- topic_freq_tbl %>%
    pivot_longer(cols = all_of(all_topic_freq_cols), names_to = "topic", values_to = "freq") %>%
    mutate(topic = gsub("topic", "", topic)) %>%
    mutate(topic = gsub("_freq", "", topic))
  
  # for each window, for ALL media outlets,
  # calculate the total frequency of each topic
  topic_freq_tbl_all <- s_dat %>%
    select(window, paste0("topic", 1:n_topics)) %>%
    group_by(window) %>%
    # summarize_at(paste0("topic", 1:n_topics), sum) %>% # sum the probabilities of all topics per window
    summarize(across(starts_with("topic"), ~ sum(.x, na.rm = TRUE))) %>%
    rename_with(temp <- function(x) { # rename topic1 => topic1_freq, topic2 => topic2_freq
      paste0(x, "_freq")
    }, starts_with("topic"))
  
  topic_freq_long_tbl_all <- topic_freq_tbl_all %>%
    pivot_longer(cols = all_of(all_topic_freq_cols), names_to = "topic", values_to = "freq") %>%
    mutate(topic = gsub("topic", "", topic)) %>%
    mutate(topic = gsub("_freq", "", topic))
  
  # Next, engagement
  # for each window, for each outlet calculate the average engagement per topic
  
  topic_AE_tbl <- topic_freq_tbl %>%
    select(media_id, window)
  
  for(topic in 1:n_topics) {
    windowly_media_topic_avg_eng <- s_dat %>%
      
      # create a column with total engagegemt
      mutate(total_engagement = Likes + Comments + Shares + Love + Wow + Haha + Sad + Angry + Care) %>%
      # mutate(total_engagement = Likes) %>%
      # mutate(total_engagement = Comments) %>%
      # mutate(total_engagement = Shares) %>%
      # mutate(total_engagement = OP_score) %>%
      
      # drop topics other than the one currently calculating
      select(media_id, id, window, total_engagement, !!paste0("topic", topic)) %>%
      
      # multiply topic1 prob with that post's total engagement
      mutate(topic_eng = get(paste0("topic", topic)) * total_engagement) %>%
      
      # drop that topic column
      select(-(paste0("topic", topic))) %>%
      
      # for each media id, for each window, calculate the sum of these weighted topical engagement scores
      group_by(media_id, window) %>%
      summarize(topic_tot_eng = sum(topic_eng)) %>%
      ungroup() %>%
      
      # join with the topic frequency tbl
      inner_join(topic_freq_tbl, by = c("media_id", "window")) %>%
      
      # select only the relevant topic (i.e. the column that starts with "topicx_")
      select(media_id, window, topic_tot_eng,
             starts_with(paste0("topic", topic, "_"))) %>%
      
      # create a new column where you divide the weighted sum of topical engagements by the window's topic frequency
      # this is the average engagement with that topic for that outlet on that date
      mutate(topic_AE = topic_tot_eng/get(paste0("topic", topic, "_freq"))) %>%
      select(media_id, window, topic_AE) %>%
      
      # rename that column name to topicx_AE
      rename(!!(paste0("topic", topic, "_AE")) := 3)
    
    # these tbls being joined should have the same number of rowsb
    topic_AE_tbl <- topic_AE_tbl %>%
      inner_join(windowly_media_topic_avg_eng, by = c("media_id", "window"))
  }
  
  all_topic_AE_cols <- paste0("topic", 1:n_topics, "_AE")
  
  topic_AE_long_tbl <- topic_AE_tbl %>%
    pivot_longer(cols = all_of(all_topic_AE_cols), names_to = "topic", values_to = "avg_eng") %>%
    mutate(topic = gsub("topic", "", topic)) %>%
    mutate(topic = gsub("_AE", "", topic))
  
  # calculate topic proportion for each media outlet for each window
  freq_long_tbl <- topic_freq_long_tbl %>%
    inner_join(post_freq, by = c("media_id", "window")) %>%
    mutate(topic_prop = freq/post_freq) %>%
    select(media_id, window, topic, freq, topic_prop) %>%
    mutate(last_window = window - lag)
  
  # calculate topic proportion for ALL media outlets for each window
  freq_all_tbl <- topic_freq_long_tbl_all %>%
    inner_join(post_freq_all, by = c("window"))  %>%
    mutate(topic_prop_all = freq/post_freq) %>%
    select(window, topic, topic_prop_all)
  
  freq_long_lag_tbl <- freq_long_tbl %>%
    inner_join(freq_long_tbl, by = c("media_id", "topic", "last_window" = "window")) %>%
    rename(curr_freq = freq.x,
           curr_topic_prop = topic_prop.x,
           last_freq = freq.y,
           last_topic_prop = topic_prop.y) %>%
    select(-last_window.y) %>%
    inner_join(freq_all_tbl, by = c("last_window" = "window", "topic")) %>% # this is new
    rename(last_topic_prop_all = topic_prop_all) 
  
  s_model_tbl <- freq_long_lag_tbl %>%
    inner_join(topic_AE_long_tbl, by = c("media_id", "topic", "last_window" = "window"))
  
  media_window_avg_eng <- s_dat %>%
    # create a column with total engagegemt
    
    mutate(total_engagement = Likes + Comments + Shares + Love + Wow + Haha + Sad + Angry + Care) %>%
    # mutate(total_engagement = Likes) %>%
    # mutate(total_engagement = Comments) %>%
    # mutate(total_engagement = Shares) %>%
    # mutate(total_engagement = OP_score) %>%
    
    
    # calculate mean engagement by media_id and window
    group_by(media_id, window) %>%
    summarize(mean_eng = mean(total_engagement))
  
  s_model_tbl <- s_model_tbl %>%
    ungroup() %>%
    inner_join(media_window_avg_eng, by = c("media_id", "last_window" = "window")) %>%
    mutate(eng_sig = avg_eng / mean_eng) %>%
    mutate(log_eng_sig = log1p(eng_sig))
  
  # calculate average eng sig across all outlets for topic and time
  avg_eng_sig <- s_model_tbl %>%
    group_by(topic, last_window) %>%
    summarise(log_avg_eng_sig = mean(log1p(eng_sig), na.rm = T))
  
  # join s_model_tbl with these on columns "last_window" and "topic"
  s_model_tbl <- s_model_tbl %>%
    inner_join(avg_eng_sig, by = c("last_window", "topic"))
  
  s_model_tbl <- s_model_tbl %>%
    mutate(season = s,
           final_topic = as.character(glue("{season}_{topic}")))
  
  # calculate curr relative coverage (NEW)
  avg_topic_prop_tbl <- s_model_tbl %>% 
    select(window, topic, curr_topic_prop) %>%
    group_by(window, topic) %>%
    summarize(avg_topic_prop = mean(curr_topic_prop, na.rm = TRUE)) # should the NAs be replaced with 0s?
  
  s_model_tbl <- s_model_tbl %>% 
    inner_join(avg_topic_prop_tbl, by = c("window", "topic")) %>%
    mutate(curr_rel_topic_prop = curr_topic_prop/avg_topic_prop) %>%
    select(-avg_topic_prop)
  
  model_tbl <- model_tbl %>%
    rbind(s_model_tbl)
}

# test for staionarity

model_tbl_z <- model_tbl %>%
  mutate(media_topic = as_factor(glue("{media_id}_{topic}")))

pdata <- pdata.frame(model_tbl_z, index=c("media_topic","window"))

adf.test(pdata$curr_topic_prop,k=1) %>%
  tidy()


