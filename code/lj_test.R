library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(formattable)
#install.packages(c("knitr", "dplyr"))
load("../data/lj_sh_2019.RData")








# group statistics and calculate the proportion
housing_stats <- lj %>%
  mutate(building_age = 2025 - building_year) %>%
  filter(building_age <= 20) %>%
  group_by(building_age) %>%
  summarise(
    building_amount = n(),
    total_area = sum(building_area)
  ) %>%  
  mutate(
    proportion = paste0(round(building_amount/sum(building_amount)*100, 2), "%"),
    building_year = 2025 - building_age
  ) %>%
  arrange(desc(proportion), desc(building_amount)) %>%
  select(building_year, building_age, total_area, building_amount, proportion)

# output table
knitr::kable(
  head(housing_stats, 5000),
  caption = "statistics on the distribution of newly built houses within 30 years (sorted by regional area)",
  align = c("l", "c", "c", "r", "r")
)

housing_stats %>%
  #mutate(building_age = 2025 - building_year) %>%
  #filter(building_age <= 20) %>%
  ggplot() +
  geom_point(aes(building_age,total_area))




lj %>%
  mutate(building_age=(2025-building_year)) %>%
  ggplot() +
  geom_point(aes(building_age,price_ttl))

ggplot(lj, aes(x=building_area, y=price_ttl,color=factor(has_elevator))) +
  geom_point(shape = 1) +
  geom_smooth(method="lm") +
  facet_wrap(~ line) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 1000)) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))
