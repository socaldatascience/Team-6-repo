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
library(ROCit)
library(pROC)

load("C:/Users/neoda/Cal State Fullerton/Jaynes, Jessica - CSUF CHOC Summer 2022 Research/Group2/cfCOVIDgroup6.RDATA")


#--- Changing to first encounters and selecting variables of interest ----------

choc <- d %>% 
  mutate(
    servicedate = as.Date(d[,"servicedate"]), 
    personid = as.factor(personid)) %>% 
    arrange_at("servicedate") %>% 
    distinct(personid, .keep_all = TRUE) %>% 
  
  dplyr::select(personid, encounterid, COVIDseverity, age_at_encounter, 
                contains("comorb"), INOTROPES, REMDESIVIR, CCP, DEXAMETHASONE, 
                ENOXAPARIN, HEPARIN, IVIG, METHYLPREDNISOLONE, RITUXIMAB, 
                TOCILIZUMAB, ASPIRIN, LOPINAVIR_OR_RITONAVIR, admit_tempC, 
                heartrate, respiratoryrate, spo2, bmi_percentile, bmi_ratio, 
                systolicBP, diastolicBP, bilirubin_total_mg_per_dl, 
                crp_mg_dl, leukocytes_1000_per_uL, lymphocytes_1000_per_uL, 
                hemoglobin_g_per_dL, platelets_1000_per_uL, ALP_U_per_L, 
                ALT_U_per_L, AST_U_per_L, LDH_U_per_L, ferritin_ng_per_mL, 
                servicedate)

#--- MICE IMPUTING -------------------------------------------------------------

choc.mice <- choc %>% 
  dplyr::select(personid ,
                COVIDseverity,
                admit_tempC ,  
                heartrate, 
                respiratoryrate, 
                spo2, 
                bmi_ratio, 
                crp_mg_dl, 
                leukocytes_1000_per_uL, 
                lymphocytes_1000_per_uL, 
                ALP_U_per_L, ALT_U_per_L, LDH_U_per_L)

init = mice(choc.mice, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("personid", "COVIDseverity")]= ""

meth[c("admit_tempC", "heartrate", "respiratoryrate", "spo2", "bmi_ratio",
       "crp_mg_dl", "leukocytes_1000_per_uL", "lymphocytes_1000_per_uL", 
       "ALP_U_per_L", "ALT_U_per_L", "LDH_U_per_L")] = "norm"

imputed = mice(choc.mice, method = meth, predictorMatrix = predM, m = 5) 

imputed.choc <- complete(imputed)
