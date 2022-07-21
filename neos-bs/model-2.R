library(tidyverse)
library(knitr)
library(gt)
library(ggpubr)
library(leaps)
library(glmnet)
library(MASS)
library(Hmisc)
library(reshape2)
library(foreign)
library(ordinal)
library(geepack)
library(mice)

load("C:/Users/neoda/Cal State Fullerton/Jaynes, Jessica - CSUF CHOC Summer 2022 Research/Group2/cfCOVIDgroup6.RDATA")

mice.test <-  d %>% 
  dplyr::select( 
    spo2, bilirubin_total_mg_per_dl, LDH_U_per_L
    )

init = mice(mice.test, maxit = 0)
meth = init$method
predM = init$predictorMatrix



meth[c("spo2", "bilirubin_total_mg_per_dl", "LDH_U_per_L")] = "norm"


set.seed(103)
imputed = mice(mice.test, method = meth, predictorMatrix = predM, m = 5)
