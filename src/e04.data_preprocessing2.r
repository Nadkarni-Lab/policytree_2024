
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);





# ==================================================================================================
# Load
# ==================================================================================================

load("data/eicu.study0.20240505.rd");





# ==================================================================================================
# Preprocessing
# ==================================================================================================

d.e_fu_raki_e   <- select(d.fu_raki_e, time=fu_raki_tm , status=fu_raki_st);
d.e_fu_raki_s   <- select(d.fu_raki_s, time=fu_raki_tm , status=fu_raki_st);
d.e_fu_make_eoh <- select(d.fu_make_eoh, status=fu_make_eoh_st);
d.e_fu_death    <- select(d.fu_death, subject_id, time=fu_death_tm, status=fu_death_st);
d.e_ft          <- select(d.ft_imputed, -subject_id);
d.e_tx          <- (d.ce_fi$amount_in < 500) * 1L

# Preprocess: convert numeric
levels(d.e_ft$sc_aki_cr_st) <- c("0", "0", "1", "2", "3");
levels(d.e_ft$sc_aki_uo_st) <- c("0", "0", "1", "2", "3");
d.e_ft$sc_aki_st    = as.numeric(as.character(d.e_ft$sc_aki_st   ));
d.e_ft$sc_aki_cr_st = as.numeric(as.character(d.e_ft$sc_aki_cr_st));
d.e_ft$sc_aki_uo_st = as.numeric(as.character(d.e_ft$sc_aki_uo_st));

# Model matrix
d.e_ft2 <- model.matrix(~ ., d.e_ft)[, -1];





# ==================================================================================================
# Save
# ==================================================================================================

save(
    d.e_fu_raki_e, d.e_fu_raki_s, d.e_fu_death, d.e_fu_make_eoh, 
    d.e_ft, d.e_ft2, d.e_tx, 
    file="data/eicu.study.20240505.rd"
);









