# "Source: Nutrition Month Summary Report 2022 (Figures 15, 16 & 17)"
# 
#load packages

library(tidyverse)
library(cowplot)

# Enter data
ntmnth_age <- tribble(
  ~agegp, ~year, ~stunt, ~waste, ~underwt,
  "Infants", 2022, 6.2, 5.8, 9.7,
  "1-2 years", 2022, 10.8, 8.5, 13.8,
  "3-5 years", 2022, 9.5, 12.0, 17.5,
  "Infants", 2021, 4.7, 4.7, 7.2,
  "1-2 years", 2021, 8.2, 6.9, 10.4,
  "3-5 years", 2021, 8.0, 9.7, 14.4
)

ntmnth_age <- ntmnth_age %>% mutate(agegp = factor(agegp)) %>% 
  mutate(agegp=fct_relevel(agegp,c("Infants", "1-2 years", "3-5 years"))) 
ntmnth_age <- ntmnth_age %>% mutate(year = factor(year))

# Create wide formats for use with geom_segment
# 
stw <- ntmnth_age %>% select(agegp, year, stunt) %>% 
  pivot_wider(names_from = year, values_from = stunt, names_prefix = "y")

wtw <- ntmnth_age %>% select(agegp, year, waste) %>% 
  pivot_wider(names_from = year, values_from = waste, names_prefix = "y")

utw <- ntmnth_age %>% select(agegp, year, underwt) %>% 
  pivot_wider(names_from = year, values_from = underwt, names_prefix = "y")

stunt <- ggplot(data = ntmnth_age, aes(x = agegp, y = stunt, colour = year)) +
geom_segment(data = stw,
aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="grey", linewidth = 4) +
  geom_point(size = 4, show.legend = FALSE) +
  scale_color_manual(values = c("2021" = "darkgreen", "2022" = "maroon")) +
ylim(4,12) +
coord_flip()+
theme_minimal(base_size = 16) +
xlab("") +
ylab("Stunting %")

waste <- ggplot(data = ntmnth_age, aes(x = agegp, y = waste, colour = year)) +
  geom_segment(data = wtw,
               aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="grey", linewidth = 4) +
  geom_point(size = 4, show.legend = FALSE) + 
  scale_color_manual(values = c("2021" = "darkgreen", "2022" = "maroon")) +
  ylim(4,13) +
  coord_flip()+
  theme_minimal(base_size = 16) +
  xlab("") +
  ylab("Wasting %")

under <- ggplot(data = ntmnth_age, aes(x = agegp, y = underwt, colour = year)) +
  geom_segment(data = utw,
               aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="grey", linewidth = 4) +
  geom_point(size = 4, show.legend = TRUE) + 
  scale_color_manual(values = c("2021" = "darkgreen", "2022" = "maroon")) +
  ylim(5,20) +
  coord_flip()+
  theme_minimal(base_size = 16) +
  xlab("") +
  ylab("Underweight %")

grobs <- ggplotGrob(under)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

under <- ggplot(data = ntmnth_age, aes(x = agegp, y = underwt, colour = year)) +
  geom_segment(data = utw,
               aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="grey", linewidth = 4) +
  geom_point(size = 4, show.legend = FALSE) + 
  scale_color_manual(values = c("2021" = "darkgreen", "2022" = "maroon")) +
  ylim(5,20) +
  coord_flip()+
  theme_minimal(base_size = 16) +
  xlab("") +
  ylab("Underweight %")

p3 <- plot_grid(stunt, waste, under, nrow = 1)

p3l <- plot_grid(p3, legend, ncol = 2, rel_widths = c(1, .1))

ggsave("nweek2023_22.jpg", width = 12, height = 3)
