setwd("/Users/rasulibragimov/Cal State Fullerton/Jaynes, Jessica - Group2")
dir()
load("cfCOVIDgroup6.RDATA")

View(d)
library(tidyverse)
library(janitor)

choc <- d

glimpse(choc)

summarise(mean(choc$bmi_percentile))

all_comorb <- select(choc, contains("comorb"))

glimpse(all_comorb)

d %>% 
  group_by(gender) %>% 
  print()
