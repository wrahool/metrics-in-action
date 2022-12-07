# this script produces Figures 2 and 3 in the main text.

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

#SD of curr_topic_prop 

sd(model_tbl$curr_topic_prop)

# fit models

m1 <- felm(curr_topic_prop  ~ log_eng_sig | 
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m2 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m3 <- felm(curr_topic_prop ~ last_topic_prop |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m4 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + log_avg_eng_sig |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

m5 <- felm(curr_topic_prop ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)

topic_entertainment <- read_csv(glue("auxiliary//topic_entertainment_60.csv"))
topic_politics <- read_csv(glue("auxiliary//topic_politicization_handcode_60.csv"))

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

m7 <- felm(curr_topic_prop ~ log_eng_sig * topic_political + log_eng_sig * topic_entertainment + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
             media_id + window + final_topic | 0 | media_id + window,
           data = model_tbl)


modelsummary(models = list(m2, m4, m5, m7),
             output = "gt", stars = TRUE, statistic = c("std.error"), fmt = 4,
             coef_omit = "^topic") %>%
  gtsave(filename = glue("results/table1.tex"))

# outlet level

for(m_id in unique(model_tbl$media_id)) {
  
  message(m_id)
  m_m <- felm(curr_topic_prop  ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
                window + final_topic | 0 | window,
              data = model_tbl[model_tbl$media_id == m_id,])
  
  c <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(estimate)
  
  p <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(p.value)
  
  se <- m_m %>%
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(std.error)
  
  m_coef_tbl <- c(m_id, c, p, se) %>%
    matrix() %>%
    t() %>%
    as_tibble() %>%
    rbind(m_coef_tbl)
}

media_details_tbl <- read_csv("auxiliary/media_language.csv")

m_coef_tbl <- m_coef_tbl %>%
  rename(n = 1, coeff = 2, p = 3, se = 4) %>%
  inner_join(media_details_tbl, by = "n") %>%
  select(n, media, coeff, p, se) %>%
  arrange(n)

final_coef_tbl <- m_coef_tbl %>%
  select(media, coeff, p, se) %>%
  rename("responsiveness" = 2, "p_value" = 3, "se" = 4)

final_coef_tbl <- final_coef_tbl %>%
  add_row(media = "All Media",
          responsiveness = m5 %>% tidy() %>% filter(term == "log_eng_sig") %>% pull(estimate),
          p_value = m5 %>% tidy() %>% filter(term == "log_eng_sig") %>% pull(p.value),
          se = m5 %>% tidy() %>% filter(term == "log_eng_sig") %>% pull(std.error)
  )

final_coef_tbl <- final_coef_tbl %>%
  mutate(season = "All Seasons") %>%
  select(media, season, everything())

media_label_map <- read_csv("auxiliary/media_label_map.csv")

outlet_viz_tbl <- final_coef_tbl %>%
  inner_join(media_label_map, by = c("media" = "label")) %>%
  select(short_name, season, responsiveness, p_value, se) %>%
  rename(media = short_name) %>%
  mutate(season = as.character(season)) %>%
  mutate(media_color = ifelse(media == "All Media", "red", "black")) %>%
  mutate(media_name = glue("<p style='color:{media_color}'>{media}</p>")) %>%
  mutate(media = as.factor(media),
         season = as.factor(season))

media_order <- outlet_viz_tbl %>%
  filter(season == "All Seasons") %>%
  arrange(responsiveness) %>%
  pull(media_name) %>%
  as.character()

outlet_viz_tbl <- outlet_viz_tbl %>% 
  mutate(media_name = fct_relevel(media_name, media_order)) %>%
  mutate(upper = responsiveness + 1.96*se,
         lower = responsiveness - 1.96*se)

outlet_responsiveness_hor <- ggplot(outlet_viz_tbl) +
  geom_pointrange(aes(ymin = lower, ymax = upper, x = media_name, y = responsiveness)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  lims(y = c(-0.003, 0.01)) +
  labs(y = "Responsiveness", x = "Media outlet") +
  theme_bw() +
  scale_color_manual(values = c("black" = "black", "red" = "red")) +
  theme(axis.text.x = element_markdown(angle=90, hjust = 1, vjust = 0.5),
        legend.position = "none")

## correlation with ideology

media_ideology <- read_csv("auxiliary/media_ideo.csv")

media_ideology <- media_ideology %>%
  inner_join(media_label_map) %>%
  select(media, label, short_name, ideo)

resp_ideo_tbl <- final_coef_tbl %>%
  filter(season == "All Seasons") %>%
  select(media, responsiveness) %>%
  inner_join(media_ideology, by = c("media" = "label")) %>%
  select(-media.y, -media) %>%
  rename(media = short_name)

resp_ideo_scatterplot <- ggplot(resp_ideo_tbl, aes(x=ideo, y=responsiveness)) +
  geom_point() +
  geom_text_repel(label = resp_ideo_tbl$media) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="Outlet slant", y = "Responsiveness") +
  # lims(x=c(1.5,4.5)) +
  theme_bw()

plot_grid(plotlist = list(outlet_responsiveness_hor, resp_ideo_scatterplot),
          labels = LETTERS)

ggsave("figures/fig2.svg", width = 10, height = 6, units = "in")

# partisan topics

m_coef_tbl <- NULL

for(m_id in unique(model_tbl$media_id)) {
  
  if(m_id %in% to_exclude)
    next
  
  message(m_id)
  m_m <- felm(curr_topic_prop  ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
                window + final_topic | 0 | window,
              data = model_tbl[model_tbl$media_id == m_id & model_tbl$political == 2,])
  
  c <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(estimate)
  
  pv <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(p.value)
  
  se <- m_m %>%
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(std.error)
  
  m_p_coef_tbl <- c(m_id, c, pv, se) %>%
    matrix() %>%
    t() %>%
    as_tibble()
  
  m_coef_tbl <- m_coef_tbl %>%
    rbind(m_p_coef_tbl)
}

partisan_viz_tbl <- m_coef_tbl %>%
  rename(n = 1, coeff = 2, pv = 3, se = 4) %>%
  mutate(n = as.numeric(n), coeff = as.numeric(coeff), pv = as.numeric(pv), se = as.numeric(se)) %>%
  inner_join(media_details_tbl, by = "n") %>%
  select(n, media, coeff, pv, se) %>%
  arrange(n) %>%
  inner_join(media_ideology, by = c("media" = "label")) %>%
  select(short_name, coeff, ideo)

partisan_viz <- ggplot(partisan_viz_tbl, aes(x=ideo, y=coeff)) +
  geom_point() +
  geom_text_repel(label = partisan_viz_tbl$short_name, col = "black",
                  # nudge_x = 0.001, nudge_y = 0.001, 
                  # max.overlaps = 15, size = 3
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="ideology") +
  theme(strip.text.y = element_text(size = 30)) +
  labs(x = "Media slant", y = "Responsiveness") +
  theme_bw()

# entertainment topics

m_coef_tbl <- NULL

for(m_id in unique(model_tbl$media_id)) {
  
  if(m_id %in% to_exclude)
    next
  
  message(m_id)
  m_m <- felm(curr_topic_prop  ~ log_eng_sig + last_topic_prop + last_topic_prop_all + log_avg_eng_sig |
                window + final_topic | 0 | window,
              data = model_tbl[model_tbl$media_id == m_id & model_tbl$entertainment == 1,])
  
  c <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(estimate)
  
  pv <- m_m %>% 
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(p.value)
  
  se <- m_m %>%
    tidy() %>%
    filter(term == 'log_eng_sig') %>%
    pull(std.error)
  
  m_p_coef_tbl <- c(m_id, c, pv, se) %>%
    matrix() %>%
    t() %>%
    as_tibble()
  
  m_coef_tbl <- m_coef_tbl %>%
    rbind(m_p_coef_tbl)
}

entertainment_viz_tbl <- m_coef_tbl %>%
  rename(n = 1, coeff = 2, pv = 3, se = 4) %>%
  mutate(n = as.numeric(n), coeff = as.numeric(coeff), pv = as.numeric(pv), se = as.numeric(se)) %>%
  inner_join(media_details_tbl, by = "n") %>%
  select(n, media, coeff, pv, se) %>%
  arrange(n) %>%
  inner_join(media_ideology, by = c("media" = "label")) %>%
  select(short_name, coeff, ideo)

entertainment_viz <- ggplot(entertainment_viz_tbl, aes(x=ideo, y=coeff)) +
  geom_point() +
  geom_text_repel(label = entertainment_viz_tbl$short_name, col = "black",
                  # max.overlaps = 15, size = 3
  ) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="ideology") +
  theme(strip.text.y = element_text(size = 30)) +
  labs(x = "Outlet slant", y = "Responsiveness") +
  theme_bw()

plot_grid(plotlist = list(partisan_viz, entertainment_viz),
          labels = LETTERS)

ggsave("results/fig3.svg", width = 10, height = 6, units = "in")


