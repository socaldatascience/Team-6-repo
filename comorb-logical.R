colnames(d)

comorb <- d %>% 
  select(personid,contains("comorb"))


view(comorb)
glimpse(comorb)

comorb_log <- comorb %>%   
mutate_if(is.factor, is.logical)




