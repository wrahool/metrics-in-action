# this script produces figure A3 in the Appendix.

library(tidyverse)
library(scales)
library(ggrepel)
library(ggpubr)
library(glue)

# set correct path to folder with all CSV files
data_folder <- "/path/to/data/folder/"

pew_scores <- read_csv(glue(data_folder, "media_ideo.csv"))
adfontes_scores <- read_csv(glue(data_folder, "media_ideo2.csv"))

media_map <- read_csv(glue(data_folder, "media_label_map.csv"))

ideo_comparison <- media_map %>%
  inner_join(pew_scores, by = c("media")) %>%
  select(label, ideo) %>%
  rename("media" = 1, "pew_ideo" = 2) %>%
  inner_join(adfontes_scores) %>%
  rename("adfontes_ideo" = 3)

ideo_correlation <- ggplot(data = ideo_comparison, aes(x=pew_ideo, y=adfontes_ideo)) +
  geom_point() +
  geom_text_repel(label = ideo_comparison$media, max.overlaps = 15) +
  geom_smooth(method = "lm", se = TRUE) +
  stat_cor(method = "pearson", digits = 3) +
  labs(x="Crowdsourced media slant (Pew)", y = "Expert annotated media slant (Ad Fontes)") +
  theme_bw()

ggsave("figures/figA3.svg", width = 5, height = 6, units = "in")
