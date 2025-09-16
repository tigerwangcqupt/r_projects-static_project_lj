library(tidyverse)
library(dplyr)
load("../data/lj_sh_2019.RData")




lj %>%
  mutate(building_age=(2025-building_year)) %>%
  ggplot() +
  geom_point(aes(building_age,building_area))


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
