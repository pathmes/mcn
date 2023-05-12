library(tidyverse)
library(readr)

ntmnth2022 <- read_csv("ntmnth2022.csv")
lookup <- read_csv("C:/AP/Dropbox/data/WHO_Colombo/manjula/manjula_r/districts_provinces.csv")

ndata <- ntmnth2022 %>% inner_join(., lookup, join_by(District))
names(ndata)[3] <- "Children_Measured"


ndata <- ndata %>% mutate(Growth_Problem_p = Growth_Problem*100/Children_Measured)
ndata <- ndata %>% mutate(Underweight_p = Underweight*100/Children_Measured)
ndata <- ndata %>% mutate(SAM_p = SAM*100/Children_Measured)
ndata <- ndata %>% mutate(MAM_p = MAM*100/Children_Measured)
ndata <- ndata %>% mutate(Stunted_p = Stunted*100/Children_Measured)
ndata <- ndata %>% mutate(Overweight_Obese_p = Overweight_Obese*100/Children_Measured)
