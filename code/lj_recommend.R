
library(ggplot2)
library(dplyr)

# 模拟数据生成
houses <- lj %>%
  mutate(building_age = 2025 - building_year) %>%
  filter(building_age <= 20) 

# 性价比计算
houses <- houses %>% 
  mutate(
    age_factor = 1/(1 + 0.02*building_age),
    value_score = (scale(building_area)*0.6 + scale(age_factor)*0.4)/scale(price_sqm)
  )

# 可视化分析
ggplot(houses, aes(x=building_age, y=building_area, color=price_ttl)) +
  geom_point(size=3, alpha=0.8) +
  scale_color_gradient(low="#3498db", high="#e74c3c") +
  labs(x="building_age", y="building_area", color="price_ttl") +
  theme_minimal()

# 输出TOP10推荐
top10 <- houses %>% 
  arrange(desc(value_score)) %>% 
  head(10) %>% 
  select(property_name,building_age, building_area, price_ttl, price_sqm, value_score)

print(top10)
