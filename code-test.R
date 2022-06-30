View(d)
library(tidyverse)

choc <- d

glimpse(choc)

summarise(mean(choc$bmi_percentile))
