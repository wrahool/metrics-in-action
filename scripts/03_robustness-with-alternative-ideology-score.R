# this script produces Figures A4, A5 and A6 in the Appendix.

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
  mutate(topic_other = ifelse(entertainment == 0 & political == 0, 1, 0)) %>%
  mutate(topic_entertainment = ifelse(entertainment == 1, 1, 0)) %>%
  mutate(topic_political = ifelse(political == 2, 1, 0)) %>%
  mutate(final_topic = glue("{season}_{topic}"))


model_tbl <- model_tbl %>%
  inner_join(topic_pol_ent, by = "final_topic")

to_exclude <- NULL
m_coef_tbl <- NULL
for(m_id in unique(model_tbl$media_id)) {
  
  if(m_id %in% to_exclude)
    next
  
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

## correlation with ideology

media_ideology <- read_csv("auxiliary/media_ideo2.csv")
media_label_map <- read_csv("auxiliary/media_label_map.csv")

media_ideology <- media_ideology %>%
  inner_join(media_label_map, by = c("media" = "label")) %>%
  select(media, short_name, ideo)

resp_ideo_tbl <- final_coef_tbl %>%
  select(media, responsiveness) %>%
  inner_join(media_ideology) %>%
  select(-media) %>%
  rename(media = short_name) %>%
  mutate(ideo = rescale(ideo, to = c(1,5)))

orig_media_ideology <- read_csv("auxiliary/media_ideo.csv")
media_label_map

ideo1_ideo2_scatterplot <- resp_ideo_tbl %>%
  inner_join(media_label_map, by = c("media" = "short_name")) %>%
  inner_join(orig_media_ideology, by = c("media.y" = "media")) %>%
  select(media, ideo.x, ideo.y) %>%
  ggplot(aes(x=ideo.x, y=ideo.y)) +
  geom_point() +
  geom_text_repel(label = resp_ideo_tbl$media) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="Ad Fontes media slant scores", y = "Pew media bias scores") +
  theme_bw()


ggsave("figures/figA4.svg", width = 5, height = 6, units = "in")

resp_ideo_scatterplot <- ggplot(resp_ideo_tbl, aes(x=ideo, y=responsiveness)) +
  geom_point() +
  geom_text_repel(label = resp_ideo_tbl$media) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="Outlet slant", y = "Responsiveness") +
  lims(x=c(1,5)) +
  theme_bw()

# plot_grid(plotlist = list(outlet_responsiveness_hor, resp_ideo_scatterplot),
#           labels = LETTERS)

ggsave("figures/figA5.svg", width = 5, height = 6, units = "in")

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
  inner_join(media_ideology) %>%
  select(short_name, coeff, ideo)

partisan_viz <- ggplot(partisan_viz_tbl, aes(x=ideo, y=coeff)) +
  geom_point() +
  geom_text_repel(label = partisan_viz_tbl$short_name, col = "black",
                  # nudge_x = 0.001, nudge_y = 0.001, 
                  max.overlaps = 15, size = 3) +
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
  inner_join(media_ideology) %>%
  select(short_name, coeff, ideo)

entertainment_viz <- ggplot(entertainment_viz_tbl, aes(x=ideo, y=coeff)) +
  geom_point() +
  geom_text_repel(label = entertainment_viz_tbl$short_name, col = "black",
                  max.overlaps = 15, size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="ideology") +
  theme(strip.text.y = element_text(size = 30)) +
  labs(x = "Media slant", y = "Responsiveness") +
  theme_bw()

plot_grid(plotlist = list(partisan_viz, entertainment_viz),
          labels = LETTERS)

ggsave("figures/figA6.svg", width = 10, height = 6, units = "in")



