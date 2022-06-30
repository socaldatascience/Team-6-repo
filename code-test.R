setwd("")
dir()
load(d)

View(d)
library(tidyverse)

choc <- d

glimpse(choc)

summarise(mean(choc$bmi_percentile))

all_comorb <- select(choc, contains("comorb"))

glimpse(all_comorb)
