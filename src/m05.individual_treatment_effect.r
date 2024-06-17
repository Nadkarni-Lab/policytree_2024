
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);





# ==================================================================================================
# Load
# ==================================================================================================

load("data/mimic.study.20240505.rd");





# ==================================================================================================
# Entire data
# ==================================================================================================

library(grf);
library(policytree);

fit.m_cf <- causal_forest(
    X = d.m_ft2,
    Y = d.m_fu_raki_e$status,
    W = d.m_tx,
    num.trees = 10000,
    tune.parameters = "all"
);

tau.m_cf <- double_robust_scores(fit.m_cf);

v.m_tau_cf <- variable_importance(fit.m_cf);
v.m_tau_cf <- colnames(d.m_ft2)[order(v.m_tau_cf, decreasing=T)];





# ==================================================================================================
# Save
# ==================================================================================================

save(
    fit.m_cf, tau.m_cf, v.m_tau_cf, 
    file = "data/mimic.ite.20240505.rd"
);








