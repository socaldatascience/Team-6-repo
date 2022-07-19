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
d <- sample(c, 4195, replace = F)

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


  