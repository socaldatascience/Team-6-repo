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

choc.sub <- d %>%
  select(encounterid, COVIDseverity,contains("comorb"), INOTROPES,
         REMDESIVIR, CCP, DEXAMETHASONE,ENOXAPARIN, HEPARIN, IVIG,
         METHYLPREDNISOLONE, RITUXIMAB, TOCILIZUMAB, ASPIRIN,
         LOPINAVIR_OR_RITONAVIR, admit_tempC, heartrate, respiratoryrate,
         spo2, bmi_percentile, bmi_ratio, systolicBP, diastolicBP,
         bilirubin_total_mg_per_dl, crp_mg_dl, leukocytes_1000_per_uL,
         lymphocytes_1000_per_uL, hemoglobin_g_per_dL, platelets_1000_per_uL,
         ALP_U_per_L, ALT_U_per_L, AST_U_per_L, LDH_U_per_L, ferritin_ng_per_mL )

rownames(choc.sub) <- seq(1, nrow(choc.sub))

polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + spo2 +
                   TOCILIZUMAB + METHYLPREDNISOLONE , data = choc.sub, Hess = T)
coef(summary(resp.olm))

a = seq(1, 9337)
b = sample(a, 7470, replace = F)

choc_train = choc.sub[b,]
choc_test = choc.sub[-b,]


resp.olm_train <- polr(formula = COVIDseverity ~ comorb_resp_failure_J96 + spo2 +
       TOCILIZUMAB + METHYLPREDNISOLONE , data = choc_train, Hess = T)

resp.olm_pred <- predict(resp.olm_train, newdata = choc_test)

