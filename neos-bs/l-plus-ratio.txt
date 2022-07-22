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

fill.ord_pred <- predict(fill.ord, newdata = mod.test, type = "probs")

fill.ord_pred_levels <- factor(if_else(fill.ord_pred > 0.5, "2", "1", "0"), levels = c("0", "1", "2"))

table(mod.test$COVIDseverity, fill.ord_pred)

multiROC <- multiclass.roc(mod.test$COVIDseverity, fill.ord_pred)

autoplot(multiROC)

rocit(class = fill.ord_pred_levels, score = mod.test$COVIDseverity)

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

mod.train2 <- choc.full[f,]
mod.test2 <- choc.full[-f,]

fill.glm <- glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47 + comorb_malnutrition +
                  comorb_other_GI_notLiver_K_excludesK70K77 +
                  comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96 +
                  resp_rate + age_group + comorb_hypertensive_heart_disease_I11 + 
                  comorb_resp_failure_J96*comorb_malnutrition, data = mod.train2, 
                family = binomial(link = "logit"))

fill.glm_pred <- predict(fill.glm, newdata = mod.test2, type = "response")

fill.glm_pred_levels <- factor(if_else(fill.glm_pred > 0.5, "1", "0"), levels = c("0", "1"))


table(mod.test2$bin_COVIDseverity, fill.glm_pred_levels)


roc_score <- roc(mod.test2$bin_COVIDseverity, fill.glm_pred, levels = c("0", "1", "2"))

ROCit.obj <- rocit(score = fill.glm_pred_levels, class = mod.test2$bin_COVIDseverity)

plot(ROCit.obj)

plot(roc_score)



#===================== Categorizing Imputed Data ===============================

glimpse(choc.full)

choc.fitted <- choc.full %>% 
  mutate(
    resp_rate_lev = as.factor(case_when((age_at_encounter <= 0 & 
                                          resp_rate < 30)|
                                                 ((age_at_encounter == 1 | 
                                                     age_at_encounter== 2) & 
                                                    resp_rate < 24)|
                                                 (age_at_encounter >= 3 & 
                                                    age_at_encounter <= 5 & 
                                                    resp_rate < 22)|
                                                 (age_at_encounter >= 6 &
                                                    age_at_encounter <= 12 &
                                                    resp_rate < 18)|
                                                 (age_at_encounter >= 13 &
                                                    resp_rate < 12) ~ "Low",
                                               (age_at_encounter <= 0 & 
                                                  resp_rate >= 30 &
                                                  resp_rate <= 60)|
                                                 ((age_at_encounter == 1 | 
                                                     age_at_encounter == 2) &
                                                    resp_rate >= 24 &
                                                    resp_rate <= 40)|
                                                 (age_at_encounter >= 3 &
                                                    age_at_encounter <= 5 &
                                                    resp_rate >= 22 &
                                                    resp_rate <= 34)|
                                                 (age_at_encounter >= 6 &
                                                    age_at_encounter <= 12 &
                                                    resp_rate >= 18 &
                                                    resp_rate <= 30)|
                                                 (age_at_encounter >= 13 &
                                                    resp_rate >= 12 &
                                                    resp_rate <= 16) ~ "Normal",
                                               (age_at_encounter <= 0 &
                                                  resp_rate > 60)|
                                                 ((age_at_encounter == 1 |
                                                     age_at_encounter == 2) &
                                                    resp_rate > 40)|
                                                 (age_at_encounter >= 3 &
                                                    age_at_encounter <= 5 &
                                                    resp_rate > 34)|
                                                 (age_at_encounter >= 6 &
                                                    age_at_encounter <= 12 &
                                                    resp_rate > 30)|
                                                 (age_at_encounter >= 13 &
                                                    resp_rate > 16) ~ "High")),
           resp.rate_cat = case_when(resp_rate_lev =="Normal"~ 0,
                                     resp_rate_lev == "Low"| resp_rate_lev == "High"~ 1),
    
    tempC_cat = as.factor(case_when(
      tempC >= 35 &
      tempC <= 37.3 ~ 0,
      
      tempC < 35|
      tempC > 37.3 ~ 1)),
    
    heartrate_cat = as.factor(case_when(
      heart_rate >= 60 &
      heart_rate <= 115 ~ 0,
      
      heart_rate < 60 |
      heart_rate > 115 ~ 1)),
    
    spo2_cat = as.factor(case_when(
      SPO2 >= 95 ~ 0,
      SPO2 < 95 ~1)), 
    
    bmi_ratio_cat = as.factor(case_when(
      BMI_ratio >= 18.5 & 
      BMI_ratio <= 24.5 ~ 0,
      
      BMI_ratio < 18.5 |
      BMI_ratio > 24.5 ~ 1)), 
    
    crp_levels = as.factor(case_when(
      CRP < 0.9 ~ 0,
      CRP > 0.9 ~ 1)),
    
    leukocytes_cat = as.factor(case_when(
      leukocytes >= 4.5 &
      leukocytes <= 11 ~ 0,
              
      leukocytes < 4.5|
      leukocytes > 11 ~ 1)),
    
    lymphocytes_cat = as.factor(case_when(
      lymphocytes >= 1 &
      lymphocytes <= 4.8 ~ 0,
              
      lymphocytes < 1 |                                                     
      lymphocytes > 4.8 ~ 1)),
    
    ALP_cat = as.factor(case_when(
      ALP >= 44 &
      ALP <= 147 ~ 0,
      
      ALP < 44 |
      ALP > 147 ~ 1)),
    
    ALT_cat = as.factor(case_when(
      ALT >= 10 & 
      ALT <= 40 ~ 0,
        
      ALT < 10 |
      ALT > 40 ~ 1))
    )


glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47 + comorb_malnutrition +
      comorb_other_GI_notLiver_K_excludesK70K77 +
      comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96 +
      resp.rate_cat + age_group + comorb_hypertensive_heart_disease_I11 + 
      comorb_resp_failure_J96*comorb_malnutrition, data = choc.full, 
    family = binomial(link = "logit"))

mod.train3 <- choc.fitted[f,]
mod.test3 <- choc.fitted[-f,]

# imput.glm_cat <- glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47 + comorb_malnutrition +
#       comorb_other_GI_notLiver_K_excludesK70K77 +
#       comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96 +
#       resp.rate_cat + age_group + comorb_hypertensive_heart_disease_I11 + 
#       comorb_resp_failure_J96*comorb_malnutrition, data = mod.train3, 
#       family = binomial(link = "logit"))

imput.glm_cat <- glm(bin_COVIDseverity ~ 
                       comorb_bronchiectasis_J47 + 
                       comorb_malnutrition +
                       comorb_other_GI_notLiver_K_excludesK70K77 +
                       comorb_chronic_kidney_disease_N18 + 
                       comorb_resp_failure_J96 +
                       resp.rate_cat + 
                       age_group + 
                       comorb_hypertensive_heart_disease_I11 +
                       comorb_resp_failure_J96*comorb_malnutrition + 
                       spo2_cat + 
                       heartrate_cat + 
                       tempC_cat + 
                       crp_levels + 
                       leukocytes_cat + 
                       lymphocytes_cat + 
                       bmi_ratio_cat + 
                       ALP_cat + 
                       ALT_cat + 
                       comorb_other_GI_notLiver_K_excludesK70K77*ALT_cat + 
                       comorb_bronchiectasis_J47*lymphocytes_cat +
                       leukocytes_cat*tempC_cat +
                       comorb_bronchiectasis_J47*spo2_cat
                       ,data = mod.train3, family = binomial(link = "logit"))

imput.glm_pred <- predict(imput.glm_cat, newdata = mod.test3, type = "response")

imput.glm_pred_levels <- factor(if_else(imput.glm_pred > 0.5, "1", "0"), levels = c("0", "1"))

table(mod.test3$bin_COVIDseverity, imput.glm_pred_levels)

roc(mod.test3$bin_COVIDseverity, imput.glm_pred)

ROCit.obj2 <- rocit(score = imput.glm_pred, class = mod.test3$bin_COVIDseverity)

plot(ROCit.obj2)

plot(roc_score)
