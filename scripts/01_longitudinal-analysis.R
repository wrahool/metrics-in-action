# this script produces the main results (table 1) and figure 1 in the main text.

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

agg_data_folder <- "C:/Users/Subhayan/Desktop/RnR code/data"

# set lag and window size
lag <- 1 # in days
start_season <- 1
end_season <- 60

window_estimates <- NULL

for(window_size in 1:14) {
  
  model_tbl <- read_csv(glue("{agg_data_folder}/aggregated_data-60_periods-window-", window_size, ".csv"))
  
  message(glue("Running models for window size : {window_size}"))
  
  ## DV is curr_topic_prop
  
  m5 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
               media_id + window + final_topic | 0 | media_id + window,
             data = model_tbl)
  
  m5_tidy <- m5 %>% tidy(conf.int = T)
  
  window_estimates <- window_estimates %>%
    rbind(
      tibble(
        window = window_size,
        
        m5_est = m5_tidy %>% filter(term == "log_eng_sig") %>% pull(estimate),
        m5_confl = m5_tidy %>% filter(term == "log_eng_sig") %>% pull(conf.low),
        m5_confu = m5_tidy %>% filter(term == "log_eng_sig") %>% pull(conf.high)
      ))
}

window_estimates_final <- window_estimates %>%
  pivot_longer(cols = -window,
               names_to = "statistic") %>%
  separate(statistic, into = c("model", "statistic"), sep = "_") %>%
  pivot_wider(names_from = statistic)

m5s <- window_estimates_final %>%
  filter(model == "m5")

ggplot(m5s) +
  geom_line(aes(x=window, y=est)) +
  geom_point(aes(x=window, y=est)) +
  geom_errorbar(aes(x=window, y=est, ymin = confl, ymax=confu), width=.2) +
  geom_hline(yintercept = 0, color = "red", lty = "dashed") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,14, 2)) +
  labs(x = "Response window", y = "Responsiveness")

ggsave(glue("figures/fig1.svg"), width = 5, height = 3, units = "in")
