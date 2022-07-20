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

load("C:/Users/neoda/Cal State Fullerton/Jaynes, Jessica - CSUF CHOC Summer 2022 Research/Group2/cfCOVIDgroup6.RDATA")

glimpse(d)
choc.sub <- d %>%
  dplyr::select(personid, encounterid, COVIDseverity, age_at_encounter, contains("comorb"), INOTROPES,
         REMDESIVIR, CCP, DEXAMETHASONE,ENOXAPARIN, HEPARIN, IVIG,
         METHYLPREDNISOLONE, RITUXIMAB, TOCILIZUMAB, ASPIRIN,
         LOPINAVIR_OR_RITONAVIR, admit_tempC, heartrate, respiratoryrate,
         spo2, bmi_percentile, bmi_ratio, systolicBP, diastolicBP,
         bilirubin_total_mg_per_dl, crp_mg_dl, leukocytes_1000_per_uL,
         lymphocytes_1000_per_uL, hemoglobin_g_per_dL, platelets_1000_per_uL,
         ALP_U_per_L, ALT_U_per_L, AST_U_per_L, LDH_U_per_L, ferritin_ng_per_mL, 
         servicedate)


### Categorization ###
first_encounter <- d %>%
  mutate(servicedate = as.Date(d[,"servicedate"]),
         personid = as.factor(personid),
         age_group = as.factor(case_when(age_at_encounter <= 11 ~ "0 - 11",
                                         age_at_encounter > 11 & 
                                           age_at_encounter <= 17 ~ "12 - 17",
                                         age_at_encounter > 17 &
                                           age_at_encounter < 21 ~ "18 - 20",
                                         age_at_encounter > 20 ~ "21 - 25")),
         payer = as.factor(payer)) %>% 
  arrange_at("servicedate") %>%
  distinct(personid, .keep_all = TRUE) %>% 
  mutate(resp_rate_lev = as.factor(case_when((age_at_encounter <= 0 & 
                                                respiratoryrate < 30)|
                                               ((age_at_encounter == 1 | 
                                                   age_at_encounter== 2) & 
                                                  respiratoryrate < 24)|
                                               (age_at_encounter >= 3 & 
                                                  age_at_encounter <= 5 & 
                                                  respiratoryrate < 22)|
                                               (age_at_encounter >= 6 &
                                                  age_at_encounter <= 12 &
                                                  respiratoryrate < 18)|
                                               (age_at_encounter >= 13 &
                                                  respiratoryrate < 12) ~ "Low",
                                             (age_at_encounter <= 0 & 
                                                respiratoryrate >= 30 &
                                                respiratoryrate <= 60)|
                                               ((age_at_encounter == 1 | 
                                                   age_at_encounter == 2) &
                                                  respiratoryrate >= 24 &
                                                  respiratoryrate <= 40)|
                                               (age_at_encounter >= 3 &
                                                  age_at_encounter <= 5 &
                                                  respiratoryrate >= 22 &
                                                  respiratoryrate <= 34)|
                                               (age_at_encounter >= 6 &
                                                  age_at_encounter <= 12 &
                                                  respiratoryrate >= 18 &
                                                  respiratoryrate <= 30)|
                                               (age_at_encounter >= 13 &
                                                  respiratoryrate >= 12 &
                                                  respiratoryrate <= 16) ~ "Normal",
                                             (age_at_encounter <= 0 &
                                                respiratoryrate > 60)|
                                               ((age_at_encounter == 1 |
                                                   age_at_encounter == 2) &
                                                  respiratoryrate > 40)|
                                               (age_at_encounter >= 3 &
                                                  age_at_encounter <= 5 &
                                                  respiratoryrate > 34)|
                                               (age_at_encounter >= 6 &
                                                  age_at_encounter <= 12 &
                                                  respiratoryrate > 30)|
                                               (age_at_encounter >= 13 &
                                                  respiratoryrate > 16) ~ "High")),
         ord_resp.rate = case_when(resp_rate_lev =="Normal"~1,
                                   resp_rate_lev == "Low"| resp_rate_lev == "High"~ 2,
                                   is.na(respiratoryrate) & resp_rate_lev == "Unknown"~0),
         ord_spo2 = as.factor(case_when(spo2 >= 95 ~ 1,
                                        spo2 < 95 ~ 2,
                                        is.na(spo2) ~ 0)),
         ord_lymp = as.factor(case_when(lymphocytes_1000_per_uL >= 1 &
                                          lymphocytes_1000_per_uL <= 4.8 ~ 1,
                                        lymphocytes_1000_per_uL < 1 |                                                     
                                          lymphocytes_1000_per_uL > 4.8 ~ 2,
                                        is.na(lymphocytes_1000_per_uL) ~ 0)), 
         ord_ALP = as.factor(case_when(ALP_U_per_L >= 44 &
                                         ALP_U_per_L <= 147 ~ 1,
                                       ALP_U_per_L < 44 | ALP_U_per_L > 147 ~ 2,
                                       is.na(ALP_U_per_L) ~ 0)), 
         ord_ALT = as.factor(case_when(ALT_U_per_L >= 10 & ALT_U_per_L <= 40 ~ 1,
                                       ALT_U_per_L < 10 | ALT_U_per_L > 40 ~ 2,
                                       is.na(ALT_U_per_L) ~ 0)),
         ord_LDH = as.factor(case_when((age_at_encounter < 18 & LDH_U_per_L >= 160 &
                                          LDH_U_per_L <= 450)|(age_at_encounter >= 18 &
                                                                 LDH_U_per_L >= 140 &
                                                                 LDH_U_per_L <= 280) ~ 1, 
                                       (age_at_encounter  < 18 & (LDH_U_per_L <  260 | 
                                                                    LDH_U_per_L >  450)) | 
                                         (age_at_encounter  >= 18 & (LDH_U_per_L <  140 | 
                                                                       LDH_U_per_L >  280)) ~
                                         2, 
                                       is.na(LDH_U_per_L) ~ 0)))

first_encounter_all_labs <- first_encounter %>%  
  mutate(
    bilirubin_levels = as.factor(case_when(bilirubin_total_mg_per_dl >= 0.1 &
              bilirubin_total_mg_per_dl <= 1.2 ~ 1, 
              
              bilirubin_total_mg_per_dl < 0.1|
              bilirubin_total_mg_per_dl > 1.2 ~ 2,
              
              is.na(bilirubin_total_mg_per_dl) ~ 0)), 
    
    crp_levels = as.factor(case_when(crp_mg_dl < 0.9 ~ 1,
              
              crp_mg_dl > 0.9 ~ 2,
              
              is.na(crp_mg_dl) ~ 0)),
    
    leuk_leves = as.factor(case_when(leukocytes_1000_per_uL >= 4.5 &
              leukocytes_1000_per_uL <= 11 ~ 1,
              
              leukocytes_1000_per_uL < 4.5|
              leukocytes_1000_per_uL > 11 ~ 2,
              
              is.na(leukocytes_1000_per_uL) ~ 0)),
    bmi_ratio_levels = as.factor(case_when(bmi_ratio >= 18.5 &
              bmi_ratio <= 24.9 ~ 1,
              
              bmi_ratio < 18.4|
              bmi_ratio > 24.9 ~ 2,
              
              is.na(bmi_ratio) ~ 0)))


###

polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + comorb_bronchiectasis_J47 + 
       comorb_pneumothorax_J93 + comorb_malnutrition + comorb_liver_disease_K70_K77 + 
       age_group + leuk_leves + crp_levels + bilirubin_levels + ord_LDH + ord_ALT +
       ord_ALP + ord_lymp + ord_spo2 + ord_resp.rate ,
       data = first_encounter_all_labs, Hess = T)

e <- seq(1, 4662)
f <- sample(e, 4195, replace = F)

enc_labs.train <- first_encounter_all_labs[f,]
enc_labs.test  <- first_encounter_all_labs[-f,] 

vital.ord <- polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + comorb_bronchiectasis_J47 + 
       comorb_pneumothorax_J93 + comorb_malnutrition + comorb_liver_disease_K70_K77 + 
       age_group + leuk_leves + crp_levels + bilirubin_levels + ord_LDH + ord_ALT +
       ord_ALP + ord_lymp + ord_spo2 + ord_resp.rate + gender + bmi_ratio_levels, 
       data = enc_labs.train, Hess = T)

vital.ord_pred <- predict(vital.ord, newdata =enc_labs.test)

table(enc_labs.test$COVIDseverity, vital.ord_pred)

###

rownames(choc.sub) <- seq(1, nrow(choc.sub))

resp.olm <- polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + spo2 +
                   TOCILIZUMAB + METHYLPREDNISOLONE , data = choc.sub, Hess = T)
coef(summary(resp.olm))

a = seq(1, 9337)
b = sample(a, 7470, replace = F)

choc_train = choc.sub[b,]
choc_test = choc.sub[-b,]


resp.olm_train <- polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + spo2 +
       TOCILIZUMAB + METHYLPREDNISOLONE , data = choc_train, Hess = T)

resp.olm_pred <- predict(resp.olm_train, newdata = choc_test)

resp.olm_pred_levels <- factor(if_else(resp.olm_pred > 0.5, "2", "1", "0"), levels = c("0", "1", "2"))

cTab <- table(choc_test$COVIDseverity, resp.olm_pred)

(CCR <- sum(diag(cTab))/sum(cTab))
# 0.8722807

resp.olm_pred.df <- as.data.frame(resp.olm_pred)

polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + 
       comorb_bronchiectasis_J471 + comorb_obesity_overweight_E661 + 
       , data = choc_train, Hess = T)

choc_frst_ecntr <- choc.sub %>% 
  arrange_at("servicedate") %>% 
  distinct(personid, .keep_all = TRUE)

glimpse(choc_frst_ecntr)

dim(choc_frst_ecntr)

c <- seq(1, 4662)
#d <- sample(c, 4195, replace = F)

choc_frst_ectr.train <- choc_frst_ecntr[d,]
choc_frst_ectr.test  <- choc_frst_ecntr[-d,] 

choc_frst_ectr.olm <- polr(formula = COVIDseverity ~ age_at_encounter + 
                             comorb_copd_J40_J44 + comorb_nasal_polyps_J33 + 
       comorb_pneumothorax_J93 + comorb_resp_failure_J96 + 
       comorb_asthma_J45 + comorb_bronchiectasis_J47, data = choc_frst_ectr.train, Hess = T)

choc_frst_pred <- predict(choc_frst_ectr.olm, newdata = choc_frst_ectr.test)

choc_frst_pred_levels <- factor(if_else(choc_frst_pred > 0.5, "2", "1", "0"), levels = c("0", "1", "2"))

cTab.2 <- table(choc_frst_ectr.test$COVIDseverity, choc_frst_pred)
(CCR2 <- sum(diag(cTab.2))/sum(cTab.2))
#0.8929336

### CLMM2 ###
str(choc.sub)

resp.clmm <- clmm2(location = COVIDseverity ~ comorb_resp_failure_J96  + TOCILIZUMAB + METHYLPREDNISOLONE, random = as.factor(personid),
      data =  choc.sub, Hess = TRUE)
summary(resp.clmm)

clm_train <- clmm2(location = COVIDseverity ~ as.factor(comorb_bronchiectasis_J47) + 
        as.factor(comorb_resp_failure_J96) + 
        as.factor(comorb_malnutrition), 
        random = as.factor(personid),
        data =  choc_train, Hess = TRUE)

 clm_pred <-predict(clm_train, newdata = choc_test)

 clm_pred_levels <- factor(if_else(clm_pred > 0.5, "2", "1", "0"), levels = c("0", "1", "2"))
 
 cTab.3 <- table(choc_test$COVIDseverity, clm_pred)
 
summary(clm_train)
### Determining Important Variables ###
var.comorb_all <- regsubsets(COVIDseverity ~ comorb_copd_J40_J44 + comorb_nasal_polyps_J33 + 
             comorb_pneumothorax_J93 + comorb_resp_failure_J96 + 
             comorb_asthma_J45 + comorb_bronchiectasis_J47 + comorb_obesity_overweight_E66 + 
             comorb_other_nutritional_deficiencies_E50_E64 + 
             comorb_malnutrition + comorb_typeI_diabetes_E10 + 
             comorb_typeII_diabetes_E11 + comorb_hemoptysisR042 + 
             comorb_other_GI_notLiver_K_excludesK70K77 + 
             comorb_liver_disease_K70_K77 + comorb_hypertensive_heart_disease_I11 +
             comorb_essential_hypertension_I10 + comorb_heart_failure_I50 + 
             comorb_ischemic_heart_disease_I20_I25 + comorb_nicotine_dependence_F17 +
             comorb_chronic_kidney_disease_N18 + comorb_lung_transplant_Z942,
           data = choc.sub, method = "backward")
summary(var.comorb_all)



### top comorbs ###


var.meds <- regsubsets(COVIDseverity ~ INOTROPES + REMDESIVIR + CCP + DEXAMETHASONE + 
             ENOXAPARIN + HEPARIN + IVIG + METHYLPREDNISOLONE + RITUXIMAB +
             TOCILIZUMAB + LOPINAVIR_OR_RITONAVIR, data = choc.sub,
             method = "backward")
summary(var.meds)

var.resp <- regsubsets(COVIDseverity ~ comorb_copd_J40_J44 + comorb_nasal_polyps_J33 + 
             comorb_pneumothorax_J93 + comorb_resp_failure_J96 + 
             comorb_asthma_J45 + comorb_bronchiectasis_J47, data = choc.sub,
           method = "backward")
summary(var.resp)

var.nutr <- regsubsets(COVIDseverity ~ comorb_obesity_overweight_E66 + 
             comorb_other_nutritional_deficiencies_E50_E64 + 
             comorb_malnutrition + comorb_typeI_diabetes_E10 + 
             comorb_typeII_diabetes_E11, data = choc.sub,
           method = "backward")
summary(var.nutr)

### BOOTSTRAP ###



d %>% 
  summarise(
    
  med_heart_rate = median(na.omit(heartrate)),
  mean_heart_rate = mean(na.omit(heartrate)),
  #average hr 60 - 100
  med_resp_rate = median(na.omit(respiratoryrate)),
  mean_resp_rate = mean(na.omit(respiratoryrate)),
  #normal 12 - 16 
  med_spo2 = median(na.omit(spo2)),
  mean_spo2 = mean(na.omit(spo2)),
  #normal > 95
  med_bmi_per = median(na.omit(bmi_percentile)),
  median(na.omit(d$lymphocytes_1000_per_uL)),
  
  median(na.omit(d$ALP_U_per_L)),
  median(na.omit(d$LDH_U_per_L))
  )

# categories 
# - comorbidites
    
# - vitals  
# - labs


first_enc_all <- d %>% 
  mutate(servicedate = as.Date(d[,"servicedate"]), 
         personid = as.factor(personid)) %>% 
  arrange_at("servicedate") %>% 
  distinct(personid, .keep_all = TRUE)

polr(formula = COVIDseverity ~ age_at_encounter + comorb_bronchiectasis_J47 + 
       comorb_resp_failure_J96 +  comorb_malnutrition + comorb_other_GI_notLiver_K_excludesK70K77 +
       comorb_chronic_kidney_disease_N18 + ord_spo2*ord_resp.rate + ord_lymp + ord_ALP + ord_ALT + 
       ord_LDH, data = first_enc_labs, Hess = T)

e <- seq(1, 4662)
f <- sample(e, 4195, replace = F)

enc_labs.train <- first_encounter[f,]
enc_labs.test  <- first_encounter[-f,] 

mod1 <- polr(formula = COVIDseverity ~ age_at_encounter + comorb_bronchiectasis_J47*ord_resp.rate + 
               comorb_resp_failure_J96 +  comorb_malnutrition* + comorb_other_GI_notLiver_K_excludesK70K77 +
               comorb_chronic_kidney_disease_N18 + ord_spo2*ord_resp.rate + ord_lymp + ord_ALP + ord_ALT + 
               ord_LDH + ord_resp.rate, data = first_encounter, Hess = T)

mod_pred <- predict(mod1, newdata = enc_labs.test)

# mod_pred_levels <- factor(if_else(mod_pred > 0.5, "2", "1", "0"), levels = c("0", "1", "2"))

cTab.3 <- table(enc_labs.test$COVIDseverity, mod_pred)
(CCR3 <- sum(diag(cTab.3))/sum(cTab.3))

# first_enc_labs <- first_enc_labs %>% 
#   mutate(bin_COVIDseverity = as.factor(case_when(COVIDseverity == 0~0,
#                                                  
#                                        COVIDseverity == 1 |
#                                        COVIDseverity ==2~1)))
# first_enc_labs <- first_enc_labs %>% 
#   mutate(
#     bin_COVIDseverity = as.character(bin_COVIDseverity)
#   )
# 
# first_enc_labs <-first_enc_labs %>% 
#   mutate(
#     bin_COVIDseverity = as.factor(bin_COVIDseverity)
#   )
# 
# first.mod1 <- glm(bin_COVIDseverity ~ comorb_bronchiectasis_J47
#                   +comorb_malnutrition + comorb_other_GI_notLiver_K_excludesK70K77
#                   +comorb_chronic_kidney_disease_N18 + comorb_resp_failure_J96
#                   +ord_resp.rate + ord_spo2+ ord_lymp
#                   +ord_ALP+ ord_ALT + ord_LDH + ord_resp.rate*comorb_resp_failure_J96 + ord_spo2*ord_resp.rate, data = enc_labs.train, family = binomial(link = "logit"))
# 
# first.mod_pred <- predict(first.mod1, newdata = enc_labs.test)
# 
# table(enc_labs.test$COVIDseverity, first.mod_pred)
