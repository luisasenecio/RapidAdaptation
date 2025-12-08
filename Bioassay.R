library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
data <- read_excel("P:/07793_newLEAF/Workfiles/WP4/Bioassay 2025/Pinus_contorta_bioassay.xlsx", sheet = "Results")

colnames(data)

data <- data %>% 
  select(-c(Bd35, Bd42, Bd49, original_block, original_position, Type))


scores <- data %>% 
  pivot_longer(cols = starts_with("Bd"),
               names_to = "day",
               values_to = "score") %>% 
  mutate(
    day = as.numeric(gsub("Bd", "", day)),
    score = as.numeric(score))

str(scores)

ggplot(scores, aes(x = day, y = score,
                   group = id,
                   color = factor(Bioassay))) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.6, size = 1) +
  facet_wrap(~ Block) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Day",
    y = "Score (brownness)",
    color = "Bioassay",
    title = "Brownness Progression Over Time"
  ) +
  theme_bw()

# have one group of facets per bioassay!
