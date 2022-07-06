library(tidyverse)
library(matrixStats)


comorb <- d %>% 
  select(personid, contains("comorb"))

comorb_2 <- d %>% 
  select(COVIDseverity, personid)

comorbidity <- comorb %>% 
  mutate_if(is.factor, is.logical)

comorbidity_severity <- left_join(comorbidity, comorb_2)

glimpse(comorbidity_severity)  


  
rowSums(comorbidity_severity[,2:22])


table(comorbidity)


# test <- comorb %>%
#   mutate_if(is.factor, is.integer)

# comorb_log <- comorb %>%   
# mutate_if(is.factor, is.logical)




