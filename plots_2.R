
library(tidyverse)
ntmnth_age <- tribble(
  ~agegp, ~year, ~stunt, ~waste, ~underwt,
  "u1", 2022, 6.2, 5.8, 9.7,
  "1-2", 2022, 10.8, 8.5, 13.8,
  "3-5", 2022, 9.5, 12.0, 17.5,
  "u1", 2021, 4.7, 4.7, 7.2,
  "1-2", 2021, 8.2, 6.9, 10.4,
  "3-5", 2021, 8.0, 9.7, 14.4
)

ntmnth_age <- ntmnth_age %>% mutate(agegp = factor(agegp)) %>% 
  mutate(agegp=fct_relevel(agegp,c("u1", "1-2", "3-5"))) 
ntmnth_age <- ntmnth_age %>% mutate(year = factor(year))

p1 <- ntmnth_age %>%  
  ggplot(mapping = aes(x = agegp, y = stunt, fill = year)) 
p1 + geom_bar(stat = "identity", position = position_dodge()) 
       
p2 <- ggplot(data = ntmnth_age, mapping = aes(x = year, y = stunt, fill = agegp)) 
p2 + geom_bar(stat = "identity", position = position_dodge())+
  geom_text(
    aes(label = format(stunt, digits = 4), y = stunt + 0.1),
    parse = TRUE,
    position = position_dodge(0.9),
    vjust = 0
  )

stw <- ntmnth_age %>% select(agegp, year, stunt) %>% 
  pivot_wider(names_from = year, values_from = stunt, names_prefix = "y")


ggplot(stw) +
  geom_segment( aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="grey") +
  geom_point( aes(x=agegp, y=y2021), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=agegp, y=y2022), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_ipsum() +
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Stunting %")

ggplot(stw) +
  geom_segment( aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="black") +
  geom_point( aes(x=agegp, y=y2021), color="green", size=3 ) +
  geom_point( aes(x=agegp, y=y2022), color="maroon", size=3 ) +
  ylim(4,12) +
  coord_flip()+
  theme_minimal() +
  xlab("Age Group") +
  ylab("Stunting %")


wtw <- ntmnth_age %>% select(agegp, year, waste) %>% 
  pivot_wider(names_from = year, values_from = waste, names_prefix = "y")

ggplot(wtw) +
  geom_segment( aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="black") +
  geom_point( aes(x=agegp, y=y2021), color="green", size=3 ) +
  geom_point( aes(x=agegp, y=y2022), color="maroon", size=3 ) +
  ylim(4,13) +
  coord_flip()+
  theme_minimal() +
  xlab("Age Group") +
  ylab("Wasting %")

utw <- ntmnth_age %>% select(agegp, year, underwt) %>% 
  pivot_wider(names_from = year, values_from = underwt, names_prefix = "y")

ggplot(utw) +
  geom_segment( aes(x=agegp, xend=agegp, y=y2021, yend=y2022), color="black") +
  geom_point( aes(x=agegp, y=y2021), color="green", size=3 ) +
  geom_point( aes(x=agegp, y=y2022), color="maroon", size=3 ) +
  ylim(5, 20) +
  coord_flip()+
  theme_minimal() +
  xlab("Age Group") +
  ylab("Underweight %")

