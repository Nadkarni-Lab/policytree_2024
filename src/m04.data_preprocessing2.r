
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);





# ==================================================================================================
# Load
# ==================================================================================================

load("data/mimic.study0.20240505.rd");





# ==================================================================================================
# Preprocessing
# ==================================================================================================

d.m_fu_raki_e   <- select(d.fu_raki_e, time=fu_raki_tm , status=fu_raki_st);
d.m_fu_raki_s   <- select(d.fu_raki_s, time=fu_raki_tm , status=fu_raki_st);
d.m_fu_make_eoh <- select(d.fu_make_eoh, status=fu_make_eoh_st);
d.m_fu_death    <- select(d.fu_death, subject_id, time=fu_death_tm, status=fu_death_st);
d.m_ft          <- select(d.ft_imputed, -subject_id);
d.m_tx          <- (d.ce_fi$amount_in < 500) * 1L

# Preprocess: convert numeric
levels(d.m_ft$sc_aki_cr_st) <- c("0", "0", "1", "2", "3");
levels(d.m_ft$sc_aki_uo_st) <- c("0", "0", "1", "2", "3");
d.m_ft$sc_aki_st    = as.numeric(as.character(d.m_ft$sc_aki_st   ));
d.m_ft$sc_aki_cr_st = as.numeric(as.character(d.m_ft$sc_aki_cr_st));
d.m_ft$sc_aki_uo_st = as.numeric(as.character(d.m_ft$sc_aki_uo_st));

# Model matrix
d.m_ft2 <- model.matrix(~ ., d.m_ft)[, -1];





# ==================================================================================================
# Save
# ==================================================================================================

save(
    d.m_fu_raki_e, d.m_fu_raki_s, d.m_fu_death, d.m_fu_make_eoh, 
    d.m_ft, d.m_ft2, d.m_tx, 
    file="data/mimic.study.20240505.rd"
);









