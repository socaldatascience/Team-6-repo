setwd("/Users/kate/Cal State Fullerton/Jaynes, Jessica - CSUF CHOC Summer 2022 Research/Group2")
dir()
load("cfCOVIDgroup6.RDATA")

View(d)
library(tidyverse)

choc <- d

glimpse(choc)

summarise(mean(choc$bmi_percentile))

all_comorb <- select(choc, contains("comorb"))

glimpse(all_comorb)

library(ggplot2)
graph <- ggplot(choc, aes(x = comorb_asthma_J45, stat = "count", fill = COVIDseverity)) + 
  geom_bar()
print(graph)
