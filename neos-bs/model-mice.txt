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

choc <- d %>% 
  mutate(servicedate = as.Date(d[,"servicedate"]), 
         personid = as.factor(personid)) %>% 
  arrange_at("servicedate") %>% 
  distinct(personid, .keep_all = TRUE)

choc <- choc %>% 
  dplyr::select(personid, encounterid, COVIDseverity, age_at_encounter, contains("comorb"), INOTROPES,
                REMDESIVIR, CCP, DEXAMETHASONE,ENOXAPARIN, HEPARIN, IVIG,
                METHYLPREDNISOLONE, RITUXIMAB, TOCILIZUMAB, ASPIRIN,
                LOPINAVIR_OR_RITONAVIR, admit_tempC, heartrate, respiratoryrate,
                spo2, bmi_percentile, bmi_ratio, systolicBP, diastolicBP,
                bilirubin_total_mg_per_dl, crp_mg_dl, leukocytes_1000_per_uL,
                lymphocytes_1000_per_uL, hemoglobin_g_per_dL, platelets_1000_per_uL,
                ALP_U_per_L, ALT_U_per_L, AST_U_per_L, LDH_U_per_L, ferritin_ng_per_mL, 
                servicedate)
# respiratoryrate spo2 lymp ALP ALT LDH crp leuk bmi_ratio admit_tempC heartrate

choc.mice <- choc %>% 
  dplyr::select(personid, admit_tempC , heartrate, respiratoryrate, spo2, 
                bmi_ratio, crp_mg_dl, leukocytes_1000_per_uL, 
                lymphocytes_1000_per_uL, ALP_U_per_L, ALT_U_per_L, LDH_U_per_L
                )
original <- choc.mice

init = mice(choc.mice, maxit=0) 
meth = init$method
predM = init$predictorMatrix

predM[, c("personid")]=0

meth[c("admit_tempC", "heartrate", "respiratoryrate", "spo2", "bmi_ratio",
       "crp_mg_dl", "leukocytes_1000_per_uL", "lymphocytes_1000_per_uL", 
       "ALP_U_per_L", "ALT_U_per_L", "LDH_U_per_L")] = "norm"

imputed = mice(choc.mice, method = meth, predictorMatrix = predM, m = 5)


imputed.choc <- complete(imputed)

filled.choc <- imputed.choc %>% 
  mutate(
    tempC = admit_tempC,
    heart_rate = heartrate,
    resp_rate = respiratoryrate,
    SPO2 = spo2,
    BMI_ratio = bmi_ratio,
    CRP = crp_mg_dl,
    leukocytes = leukocytes_1000_per_uL,
    lymphocytes = lymphocytes_1000_per_uL,
    ALP = ALP_U_per_L,
    ALT = ALT_U_per_L,
    LDH = LDH_U_per_L
  )

choc.full <- choc %>% 
  left_join(filled.choc, by = "personid")

polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + comorb_bronchiectasis_J47 + 
       comorb_pneumothorax_J93 + comorb_malnutrition + comorb_liver_disease_K70_K77 +
       tempC + heart_rate+ resp_rate + SPO2 + BMI_ratio + CRP + leukocytes + 
       lymphocytes + ALP + ALT + LDH, data = choc.full, Hess = T)

e <- seq(1, 4662)
f <- sample(e, 4195, replace = F)

mod.train <- choc.full[f,]
mod.test <- choc.full[-f,]

fill.ord <- polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + comorb_bronchiectasis_J47 + 
       comorb_pneumothorax_J93 + comorb_malnutrition + comorb_liver_disease_K70_K77 +
       tempC + heart_rate+ resp_rate + SPO2 + BMI_ratio + CRP + leukocytes + 
       lymphocytes + ALP + ALT + LDH, data = mod.train, Hess = T)

fill.ord_pred <- predict(fill.ord, newdata = mod.test)

table(mod.test$COVIDseverity, fill.ord_pred)

choc.full <- choc.full %>% 
  mutate(
    bin_COVIDseverity = as.factor(case_when(
      COVIDseverity == 1|
      COVIDseverity == 2 ~ 1,
      COVIDseverity == 0 ~ 0,
      
    ))
  )

choc.full <- choc.full %>% 
  mutate(
    age_group = as.factor(case_when(age_at_encounter <= 11 ~ "0 - 11",
                        age_at_encounter > 11 & 
                          age_at_encounter <= 17 ~ "12 - 17",
                        age_at_encounter > 17 &
                          age_at_encounter < 21 ~ "18 - 20",
                        age_at_encounter > 20 ~ "21 - 25"))
  )

glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47 + comorb_malnutrition +
      comorb_other_GI_notLiver_K_excludesK70K77 +
      comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96 +
      resp_rate + age_group + comorb_hypertensive_heart_disease_I11 + 
      comorb_resp_failure_J96*comorb_malnutrition, data = choc.full, 
    family = binomial(link = "logit"))

mod.train <- choc.full[f,]
mod.test <- choc.full[-f,]

fill.glm <- glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47 + comorb_malnutrition +
                  comorb_other_GI_notLiver_K_excludesK70K77 +
                  comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96 +
                  resp_rate + age_group + comorb_hypertensive_heart_disease_I11 + 
                  comorb_resp_failure_J96*comorb_malnutrition, data = mod.train, 
                family = binomial(link = "logit"))

fill.glm_pred <- predict(fill.glm, newdata = mod.test)

fill.glm_pred_levels <- factor(if_else(fill.glm_pred > 0.5, "1", "0"), levels = c("0", "1"))

table(mod.test$bin_COVIDseverity, fill.glm_pred_levels)
