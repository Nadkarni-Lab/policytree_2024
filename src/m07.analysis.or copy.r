
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);
library(policytree);





# ==================================================================================================
# Load
# ==================================================================================================

# eICU
load("data/eicu.rawdata.20240505.rd");
load("data/eicu.study0.20240505.rd");
load("data/eicu.study.20240505.rd");
d.e_cohort = d.cohort;

# MIMIC
load("data/mimic.rawdata.20240505.rd");
load("data/mimic.study0.20240505.rd");
load("data/mimic.study.20240505.rd");
# 0005
# 0020
# 0027
# 54, 55, 56, 57
load("data/mimic.policytree.20240505.rd");
load("data/mimic.ite.20240505.rd");
d.m_cohort = d.cohort;





# ==================================================================================================
# Preprocessing
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# MIMIC Tree
# --------------------------------------------------------------------------------------------------

d.ft_dv <- d.m_ft2[, head(v.tau_m_cf, 25)];
d.ft_vd <- d.e_ft2[, head(v.tau_m_cf, 25)];





# ==================================================================================================
# Policy tree
# ==================================================================================================

# cat(DiagrammeRsvg::export_svg(plot(pt.m_d5)), file = 'fig/pt.m_d5.svg')



# --------------------------------------------------------------------------------------------------
# Action
# --------------------------------------------------------------------------------------------------

# MIMIC: Entire data, PT from entire data
a.dv <- predict(pt.m_d5, d.ft_dv, type="action.id");
a.dv <- a.dv==2;

# eICU: Entire data, PT from entire data
a.vd <- predict(pt.m_d5, d.ft_vd, type="action.id");
a.vd <- a.vd==2;





# ==================================================================================================
# 
# ==================================================================================================

fit.dv.raki_e.1      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)                             , family=binomial);
fit.vd.raki_e.1      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)                             , family=binomial);
fit.dv.raki_s.1      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)                             , family=binomial);
fit.vd.raki_s.1      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)                             , family=binomial);
fit.dv.make_eoh.1    <- glm(y ~ tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)                          , family=binomial);
fit.vd.make_eoh.1    <- glm(y ~ tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)                          , family=binomial);

fit.dv.raki_e.2      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[a.dv                            , ], family=binomial);
fit.vd.raki_e.2      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[a.vd                            , ], family=binomial);
fit.dv.raki_s.2      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[a.dv                            , ], family=binomial);
fit.vd.raki_s.2      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[a.vd                            , ], family=binomial);
fit.dv.make_eoh.2    <- glm(y ~ tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[a.dv                         , ], family=binomial);
fit.vd.make_eoh.2    <- glm(y ~ tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[a.vd                         , ], family=binomial);

fit.dv.raki_e.3      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[ !a.dv                             , ], family=binomial);
fit.vd.raki_e.3      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[ !a.vd                             , ], family=binomial);
fit.dv.raki_s.3      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[ !a.dv                             , ], family=binomial);
fit.vd.raki_s.3      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[ !a.vd                             , ], family=binomial);
fit.dv.make_eoh.3    <- glm(y ~ tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[ !a.dv                          , ], family=binomial);
fit.vd.make_eoh.3    <- glm(y ~ tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[ !a.vd                          , ], family=binomial);

fit.dv.raki_e.4      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)                             , family=binomial);
fit.vd.raki_e.4      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)                             , family=binomial);
fit.dv.raki_s.4      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)                             , family=binomial);
fit.vd.raki_s.4      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)                             , family=binomial);
fit.dv.make_eoh.4    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)                          , family=binomial);
fit.vd.make_eoh.4    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)                          , family=binomial);

fit.dv.raki_e.5      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[a.dv                            , ], family=binomial);
fit.vd.raki_e.5      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[a.vd                            , ], family=binomial);
fit.dv.raki_s.5      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[a.dv                            , ], family=binomial);
fit.vd.raki_s.5      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[a.vd                            , ], family=binomial);
fit.dv.make_eoh.5    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[a.dv                         , ], family=binomial);
fit.vd.make_eoh.5    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[a.vd                         , ], family=binomial);

fit.dv.raki_e.6      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[ !a.dv                             , ], family=binomial);
fit.vd.raki_e.6      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[ !a.vd                             , ], family=binomial);
fit.dv.raki_s.6      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[ !a.dv                             , ], family=binomial);
fit.vd.raki_s.6      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[ !a.vd                             , ], family=binomial);
fit.dv.make_eoh.6    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[ !a.dv                          , ], family=binomial);
fit.vd.make_eoh.6    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[ !a.vd                          , ], family=binomial);

fit.dv.raki_e.7      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                             , ], family=binomial);
fit.vd.raki_e.7      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                             , ], family=binomial);
fit.dv.raki_s.7      <- glm(y ~ tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                             , ], family=binomial);
fit.vd.raki_s.7      <- glm(y ~ tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                             , ], family=binomial);
fit.dv.make_eoh.7    <- glm(y ~ tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                          , ], family=binomial);
fit.vd.make_eoh.7    <- glm(y ~ tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                          , ], family=binomial);

fit.dv.raki_e.8      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_e$status, d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                             , ], family=binomial);
fit.vd.raki_e.8      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_e$status, d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                             , ], family=binomial);
fit.dv.raki_s.8      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_raki_s$status, d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                             , ], family=binomial);
fit.vd.raki_s.8      <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_raki_s$status, d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                             , ], family=binomial);
fit.dv.make_eoh.8    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.m_fu_make_eoh$status , d.m_ft2, tx_fi=d.m_tx)[ (a.dv & d.m_tx==1) | ((!a.dv) & d.m_tx==0)                          , ], family=binomial);
fit.vd.make_eoh.8    <- glm(y ~ dm_age + dm_genderF + dm_raceB + dm_raceH + dm_raceO + sc_aki_st + ce_fluids_vol + sc_sofa_st + chem_creatinine_bl + tx_fi, data.frame(y=d.e_fu_make_eoh$status , d.e_ft2, tx_fi=d.e_tx)[ (a.vd & d.e_tx==1) | ((!a.vd) & d.e_tx==0)                          , ], family=binomial);



round(table(tx=d.m_tx[a.dv], taki=d.m_fu_raki_e$status[a.dv]) / as.vector(table(d.m_tx[a.dv])), 3);
round(table(tx=d.m_tx[!a.dv], taki=d.m_fu_raki_e$status[!a.dv]) / as.vector(table(d.m_tx[!a.dv])), 3);
chisq.test(table(tx=d.m_tx[a.dv], taki=d.m_fu_raki_e$status[a.dv]))$p.value
chisq.test(table(tx=d.m_tx[!a.dv], taki=d.m_fu_raki_e$status[!a.dv]))$p.value
round(table(tx=d.e_tx[a.vd], taki=d.e_fu_raki_e$status[a.vd]) / as.vector(table(d.e_tx[a.vd])), 3);
round(table(tx=d.e_tx[!a.vd], taki=d.e_fu_raki_e$status[!a.vd]) / as.vector(table(d.e_tx[!a.vd])), 3);
chisq.test(table(tx=d.e_tx[a.vd], taki=d.e_fu_raki_e$status[a.vd]))$p.value
chisq.test(table(tx=d.e_tx[!a.vd], taki=d.e_fu_raki_e$status[!a.vd]))$p.value

round(table(tx=d.m_tx[a.dv], taki=d.m_fu_raki_s$status[a.dv]) / as.vector(table(d.m_tx[a.dv])), 3);
round(table(tx=d.m_tx[!a.dv], taki=d.m_fu_raki_s$status[!a.dv]) / as.vector(table(d.m_tx[!a.dv])), 3);
chisq.test(table(tx=d.m_tx[a.dv], taki=d.m_fu_raki_s$status[a.dv]))$p.value
chisq.test(table(tx=d.m_tx[!a.dv], taki=d.m_fu_raki_s$status[!a.dv]))$p.value
round(table(tx=d.e_tx[a.vd], taki=d.e_fu_raki_s$status[a.vd]) / as.vector(table(d.e_tx[a.vd])), 3);
round(table(tx=d.e_tx[!a.vd], taki=d.e_fu_raki_s$status[!a.vd]) / as.vector(table(d.e_tx[!a.vd])), 3);
chisq.test(table(tx=d.e_tx[a.vd], taki=d.e_fu_raki_s$status[a.vd]))$p.value
chisq.test(table(tx=d.e_tx[!a.vd], taki=d.e_fu_raki_s$status[!a.vd]))$p.value

round(table(tx=d.m_tx[a.dv], taki=d.m_fu_make_eoh$status[a.dv]) / as.vector(table(d.m_tx[a.dv])), 3);
round(table(tx=d.m_tx[!a.dv], taki=d.m_fu_make_eoh$status[!a.dv]) / as.vector(table(d.m_tx[!a.dv])), 3);
chisq.test(table(tx=d.m_tx[a.dv], taki=d.m_fu_make_eoh$status[a.dv]))$p.value
chisq.test(table(tx=d.m_tx[!a.dv], taki=d.m_fu_make_eoh$status[!a.dv]))$p.value
round(table(tx=d.e_tx[a.vd], taki=d.e_fu_make_eoh$status[a.vd]) / as.vector(table(d.e_tx[a.vd])), 3);
round(table(tx=d.e_tx[!a.vd], taki=d.e_fu_make_eoh$status[!a.vd]) / as.vector(table(d.e_tx[!a.vd])), 3);
chisq.test(table(tx=d.e_tx[a.vd], taki=d.e_fu_make_eoh$status[a.vd]))$p.value
chisq.test(table(tx=d.e_tx[!a.vd], taki=d.e_fu_make_eoh$status[!a.vd]))$p.value



rbind(
tail(round(exp(cbind(coef(fit.dv.raki_e.2), confint(fit.dv.raki_e.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.5), confint(fit.dv.raki_e.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.3), confint(fit.dv.raki_e.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.6), confint(fit.dv.raki_e.6))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.7), confint(fit.dv.raki_e.7))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.8), confint(fit.dv.raki_e.8))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.1), confint(fit.dv.raki_e.1))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.4), confint(fit.dv.raki_e.4))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.raki_e.2), confint(fit.vd.raki_e.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.5), confint(fit.vd.raki_e.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.3), confint(fit.vd.raki_e.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.6), confint(fit.vd.raki_e.6))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.7), confint(fit.vd.raki_e.7))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.8), confint(fit.vd.raki_e.8))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.1), confint(fit.vd.raki_e.1))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.4), confint(fit.vd.raki_e.4))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.dv.raki_s.2), confint(fit.dv.raki_s.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.5), confint(fit.dv.raki_s.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.3), confint(fit.dv.raki_s.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.6), confint(fit.dv.raki_s.6))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.7), confint(fit.dv.raki_s.7))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.8), confint(fit.dv.raki_s.8))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.1), confint(fit.dv.raki_s.1))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.4), confint(fit.dv.raki_s.4))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.raki_s.2), confint(fit.vd.raki_s.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.5), confint(fit.vd.raki_s.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.3), confint(fit.vd.raki_s.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.6), confint(fit.vd.raki_s.6))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.7), confint(fit.vd.raki_s.7))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.8), confint(fit.vd.raki_s.8))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.1), confint(fit.vd.raki_s.1))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.4), confint(fit.vd.raki_s.4))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.dv.make_eoh.2), confint(fit.dv.make_eoh.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.5), confint(fit.dv.make_eoh.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.3), confint(fit.dv.make_eoh.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.6), confint(fit.dv.make_eoh.6))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.7), confint(fit.dv.make_eoh.7))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.8), confint(fit.dv.make_eoh.8))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.1), confint(fit.dv.make_eoh.1))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.4), confint(fit.dv.make_eoh.4))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.make_eoh.2), confint(fit.vd.make_eoh.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.5), confint(fit.vd.make_eoh.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.3), confint(fit.vd.make_eoh.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.6), confint(fit.vd.make_eoh.6))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.7), confint(fit.vd.make_eoh.7))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.8), confint(fit.vd.make_eoh.8))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.1), confint(fit.vd.make_eoh.1))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.4), confint(fit.vd.make_eoh.4))), 2), 1)
)








