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

hist(choc$com)

library(ggplot2)
library(tidyverse)

#age and Covid severity

ggplot(choc, aes(x = as.factor(age_at_encounter), fill = as.factor(COVIDseverity))) +
  geom_bar(position = position_dodge()) +
  theme_classic() +
 scale_fill_manual(values=c('lightskyblue1',
                            'sandybrown',
                            'palegreen1',
                            'lightpink'))
#Race and Covid severity

raceandseverity <- ggplot(choc, aes(x = COVIDseverity, stat = "count", fill = race)) +
  geom_bar(position = position_dodge()) + facet_wrap(~COVIDseverity)
print(raceandseverity)