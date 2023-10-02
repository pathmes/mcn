# "Source: Nutrition Month Summary Report 2022 (Figures 15, 16 & 17)"
# 2023 report figures 41, 52 & 63
# load packages

library(tidyverse)


# Enter data
ntmnth_age <- tribble(
  ~agegp, ~year, ~stunt, ~waste, ~underwt,
  "Infants", 2023, 6.9, 5.0, 10.5,
  "1-2 years", 2023, 12.0, 8.4, 15.4,
  "3-5 years", 2023, 10.8, 12.0, 19.5,
  "Infants", 2022, 6.2, 5.8, 9.7,
  "1-2 years", 2022, 10.8, 8.5, 13.8,
  "3-5 years", 2022, 9.5, 12.0, 17.5,
  "Infants", 2021, 4.7, 4.7, 7.2,
  "1-2 years", 2021, 8.2, 6.9, 10.4,
  "3-5 years", 2021, 8.0, 9.7, 14.4
)
# Order age groups - infant > 1-2 > 3-5 

ntmnth_age <- ntmnth_age %>% 
  mutate(agegp = factor(agegp)) %>% 
  mutate(agegp=fct_relevel(agegp,c("Infants", "1-2 years", "3-5 years"))) 

#ntmnth_overall

ntmnth_o <- tribble(
  ~year,  ~underwt, ~stunt, ~waste,
  2021, 12.2, 7.4, 8.2,
  2022, 15.3, 9.2, 10.1,
  2023, 17.1, 10.3, 10.0
)

ntmnth_l <- ntmnth_o %>% 
  pivot_longer(
    cols = 2:4, 
    names_to = "ind", 
    values_to = "Percent")

ntmnth_sdg <- ntmnth_o %>% 
  pivot_longer(
    cols = 3:4, 
    names_to = "ind", 
    values_to = "Percent")

#DHS_overall
# Figure 3.2
# Trends in underweight, stunting and wasting in Sri Lanka, 1975-2016
# Childhood malnutrition in Sri Lanka: a road map for the last mile - 
# A review of literature, Analysis ofcorrelates of undernutrition, A qualitative inquiry, 2019

dhs_o <- tribble(
  ~year,  ~underwt, ~stunt, ~waste,
  1993, 33.8, 29.7, 17.5,
  1996, 29.3, 26.1, 15.3,
  2000, 22.8, 18.4, 15.5,
  2007, 21.1, 17.3, 14.7,
  2016, 20.5, 17.3, 15.1
)

dhs_l <- dhs_o %>% 
  pivot_longer(
    cols = 2:4, 
    names_to = "ind", 
    values_to = "Percent")

dhs_sdg <- dhs_o %>% filter(year>1999) %>% 
  pivot_longer(
    cols = 3:4, 
    names_to = "ind", 
    values_to = "Percent")
