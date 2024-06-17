
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);
source("src/lib/helper.r");





# ==================================================================================================
# Load
# ==================================================================================================

load("data/mimic.rawdata.20240505.rd");





# ==================================================================================================
# Data cleaning
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# Cohort
# --------------------------------------------------------------------------------------------------

d.cohort <- d.cohort0;
d.cohort <- arrange(d.cohort, subject_id);



# --------------------------------------------------------------------------------------------------
# Demo
# --------------------------------------------------------------------------------------------------

d.demo <- d.demo0;
d.demo <- arrange(d.demo, subject_id);



# --------------------------------------------------------------------------------------------------
# Death
# --------------------------------------------------------------------------------------------------

d.fu_death <- select(d.fu_death0, subject_id, fu_death_st=death_st, fu_death_tm=death_tm);
d.fu_death <- arrange(d.fu_death, subject_id);



# --------------------------------------------------------------------------------------------------
# Early AKI reversal
# --------------------------------------------------------------------------------------------------

d.fu_raki_e <- select(d.fu_raki_e0, subject_id, fu_raki_tm, fu_raki_st);
d.fu_raki_e <- arrange(d.fu_raki_e, subject_id);



# --------------------------------------------------------------------------------------------------
# Sustained AKI reversal
# --------------------------------------------------------------------------------------------------

d.fu_raki_s <- select(d.fu_raki_s0, subject_id, fu_raki_tm, fu_raki_st);
d.fu_raki_s <- arrange(d.fu_raki_s, subject_id);



# --------------------------------------------------------------------------------------------------
# MAKE by discharge
# --------------------------------------------------------------------------------------------------

d.fu_make_eoh <- inner_join(
    x = select(d.cohort, subject_id, disch_tm),
    y = d.sc_aki0,
    by = c('subject_id')
);
d.fu_make_eoh <- filter(d.fu_make_eoh, disch_tm > sc_aki_tm & sc_aki_tm >= 0);
d.fu_make_eoh <- group_by(d.fu_make_eoh, subject_id);
d.fu_make_eoh <- summarise(d.fu_make_eoh, 
    fu_make_eoh_st = (tail(sc_aki_cr_st[!is.na(sc_aki_cr_st)], 1) >= 2) | ((min(sc_aki_rr_st, na.rm=T) >= 2) & !is.infinite(min(sc_aki_rr_st, na.rm=T)))
);
d.fu_make_eoh <- as.data.frame(d.fu_make_eoh);

d.fu_make_eoh <- left_join(
    x = d.fu_make_eoh,
    y = d.fu_death,
    by = c('subject_id')
);

d.fu_make_eoh <- mutate(d.fu_make_eoh,
    fu_make_eoh_st = ((fu_make_eoh_st==1) | (fu_death_st==1)) * 1L,
);



# --------------------------------------------------------------------------------------------------
# Vitals
# --------------------------------------------------------------------------------------------------

d.vitals <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.vitals0,
    by = c("subject_id")
);
d.vitals <- mutate(d.vitals,
    vs_sbp = ifelse(!is.na(vs_sbp), vs_sbp, vs_sbp_ni),
    vs_dbp = ifelse(!is.na(vs_dbp), vs_dbp, vs_dbp_ni),
    vs_mbp = ifelse(!is.na(vs_mbp), vs_mbp, vs_mbp_ni)
);
d.vitals <- filter(d.vitals, 0 >= vs_tm & vs_tm >= (- 2));
d.vitals <- arrange(d.vitals, subject_id, vs_tm);
d.vitals <- select(d.vitals, -vs_tm, -in_tm, -vs_sbp_ni, -vs_dbp_ni, -vs_mbp_ni);

d.vitals_t <- group_by(d.vitals, subject_id);
d.vitals_t <- summarise(d.vitals_t,
    vs_heart_rate_lst  = tail2(vs_heart_rate , n=1    ),
    vs_heart_rate_max  = max  (vs_heart_rate , na.rm=T),
    vs_heart_rate_min  = min  (vs_heart_rate , na.rm=T),
    vs_sbp_lst         = tail2(vs_sbp , n=1           ),
    vs_sbp_min         = min  (vs_sbp        , na.rm=T),
    vs_sbp_max         = max  (vs_sbp        , na.rm=T),
    vs_dbp_lst         = tail2(vs_dbp , n=1           ),
    vs_dbp_max         = max  (vs_dbp        , na.rm=T),
    vs_dbp_min         = min  (vs_dbp        , na.rm=T),
    vs_mbp_lst         = tail2(vs_mbp , n=1           ),
    vs_mbp_max         = max  (vs_mbp        , na.rm=T),
    vs_mbp_min         = min  (vs_mbp        , na.rm=T),
    vs_resp_rate_lst   = tail2(vs_resp_rate , n=1     ),
    vs_resp_rate_max   = max  (vs_resp_rate  , na.rm=T),
    vs_resp_rate_min   = min  (vs_resp_rate  , na.rm=T),
    vs_temperature_lst = tail2(vs_temperature , n=1   ),
    vs_temperature_max = max  (vs_temperature, na.rm=T),
    vs_temperature_min = min  (vs_temperature, na.rm=T),
    vs_spo2_lst        = tail2(vs_spo2 , n=1          ),
    vs_spo2_max        = max  (vs_spo2       , na.rm=T),
    vs_spo2_min        = min  (vs_spo2       , na.rm=T),
    vs_weight          = mid2 (vs_weight              )
);
d.vitals_t <- as.data.frame(d.vitals_t);
d.vitals_t <- mutate_all(d.vitals_t, function(x) { ifelse(!is.infinite(x), x, NA) })

d.vitals <- d.vitals_t;

d.vitals <- arrange(d.vitals, subject_id);

d.vitals <- mutate(d.vitals,
    vs_weight = ifelse(vs_weight <  40,  40, vs_weight),
    vs_weight = ifelse(vs_weight > 200, 200, vs_weight)
);

d.vitals <- inner_join(
    x = select(d.demo, subject_id, dm_gender),
    y = d.vitals,
    by = c("subject_id")
);
d.vitals <- mutate(d.vitals,
    vs_weight = ifelse(!is.na(vs_weight), vs_weight, ifelse(dm_gender=="M", 90.6, 77.5))
);
d.vitals$dm_gender <- NULL;

d.vitals <- arrange(d.vitals, subject_id);



# --------------------------------------------------------------------------------------------------
# Labs
# --------------------------------------------------------------------------------------------------

d.labs <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.labs0,
    by = c("subject_id")
);
d.labs <- filter(d.labs, 0 >= le_tm & le_tm >= (- 2));
d.labs <- arrange(d.labs, subject_id, le_tm);
d.labs <- select(d.labs, -le_tm, -in_tm);

d.labs_t <- group_by(d.labs, subject_id);
d.labs_t <- summarise(d.labs_t,
    cbc_hematocrit_lst      = tail2(cbc_hematocrit       , n=1    ),
    cbc_hematocrit_max      = max  (cbc_hematocrit       , na.rm=T),
    cbc_hematocrit_min      = min  (cbc_hematocrit       , na.rm=T),
    cbc_hemoglobin_lst      = tail2(cbc_hemoglobin       , n=1    ),
    cbc_hemoglobin_max      = max  (cbc_hemoglobin       , na.rm=T),
    cbc_hemoglobin_min      = min  (cbc_hemoglobin       , na.rm=T),
    cbc_mch_lst             = tail2(cbc_mch              , n=1    ),
    cbc_mch_max             = max  (cbc_mch              , na.rm=T),
    cbc_mch_min             = min  (cbc_mch              , na.rm=T),
    cbc_mchc_lst            = tail2(cbc_mchc             , n=1    ),
    cbc_mchc_max            = max  (cbc_mchc             , na.rm=T),
    cbc_mchc_min            = min  (cbc_mchc             , na.rm=T),
    cbc_mcv_lst             = tail2(cbc_mcv              , n=1    ),
    cbc_mcv_max             = max  (cbc_mcv              , na.rm=T),
    cbc_mcv_min             = min  (cbc_mcv              , na.rm=T),
    cbc_platelet_lst        = tail2(cbc_platelet         , n=1    ),
    cbc_platelet_max        = max  (cbc_platelet         , na.rm=T),
    cbc_platelet_min        = min  (cbc_platelet         , na.rm=T),
    cbc_rbc_lst             = tail2(cbc_rbc              , n=1    ),
    cbc_rbc_max             = max  (cbc_rbc              , na.rm=T),
    cbc_rbc_min             = min  (cbc_rbc              , na.rm=T),
    cbc_rdw_lst             = tail2(cbc_rdw              , n=1    ),
    cbc_rdw_max             = max  (cbc_rdw              , na.rm=T),
    cbc_rdw_min             = min  (cbc_rdw              , na.rm=T),
    cbc_wbc_lst             = tail2(cbc_wbc              , n=1    ),
    cbc_wbc_max             = max  (cbc_wbc              , na.rm=T),
    cbc_wbc_min             = min  (cbc_wbc              , na.rm=T),
    chem_aniongap_lst       = tail2(chem_aniongap        , n=1    ),
    chem_aniongap_max       = max  (chem_aniongap        , na.rm=T),
    chem_aniongap_min       = min  (chem_aniongap        , na.rm=T),
    chem_bicarbonate_lst    = tail2(chem_bicarbonate     , n=1    ),
    chem_bicarbonate_max    = max  (chem_bicarbonate     , na.rm=T),
    chem_bicarbonate_min    = min  (chem_bicarbonate     , na.rm=T),
    chem_bun_lst            = tail2(chem_bun             , n=1    ),
    chem_bun_max            = max  (chem_bun             , na.rm=T),
    chem_bun_min            = min  (chem_bun             , na.rm=T),
    chem_calcium_lst        = tail2(chem_calcium         , n=1    ),
    chem_calcium_max        = max  (chem_calcium         , na.rm=T),
    chem_calcium_min        = min  (chem_calcium         , na.rm=T),
    chem_chloride_lst       = tail2(chem_chloride        , n=1    ),
    chem_chloride_max       = max  (chem_chloride        , na.rm=T),
    chem_chloride_min       = min  (chem_chloride        , na.rm=T),
    chem_creatinine_lst     = tail2(chem_creatinine      , n=1    ),
    chem_creatinine_max     = max  (chem_creatinine      , na.rm=T),
    chem_creatinine_min     = min  (chem_creatinine      , na.rm=T),
    chem_glucose_lst        = tail2(chem_glucose         , n=1    ),
    chem_glucose_max        = max  (chem_glucose         , na.rm=T),
    chem_glucose_min        = min  (chem_glucose         , na.rm=T),
    chem_sodium_lst         = tail2(chem_sodium          , n=1    ),
    chem_sodium_max         = max  (chem_sodium          , na.rm=T),
    chem_sodium_min         = min  (chem_sodium          , na.rm=T),
    chem_potassium_lst      = tail2(chem_potassium       , n=1    ),
    chem_potassium_max      = max  (chem_potassium       , na.rm=T),
    chem_potassium_min      = min  (chem_potassium       , na.rm=T),
    coag_inr_lst            = tail2(coag_inr             , n=1    ),
    coag_inr_max            = max  (coag_inr             , na.rm=T),
    coag_inr_min            = min  (coag_inr             , na.rm=T),
    # coag_pt_lst             = tail2(coag_pt              , n=1    ),
    # coag_pt_max             = max  (coag_pt              , na.rm=T),
    # coag_pt_min             = min  (coag_pt              , na.rm=T),
    # coag_ptt_lst            = tail2(coag_ptt             , n=1    ),
    # coag_ptt_max            = max  (coag_ptt             , na.rm=T),
    # coag_ptt_min            = min  (coag_ptt             , na.rm=T),
    bg_po2_lst              = tail2(bg_po2                , n=1   ),
    bg_po2_max              = max  (bg_po2               , na.rm=T),
    bg_po2_min              = min  (bg_po2               , na.rm=T),
    bg_pco2_lst             = tail2(bg_pco2               , n=1   ),
    bg_pco2_max             = max  (bg_pco2              , na.rm=T),
    bg_pco2_min             = min  (bg_pco2              , na.rm=T),
    # bg_fio2_chartevents_lst = tail2(bg_fio2_chartevents   , n=1   ),
    # bg_fio2_chartevents_max = max  (bg_fio2_chartevents  , na.rm=T),
    # bg_fio2_chartevents_min = min  (bg_fio2_chartevents  , na.rm=T),
    # bg_aado2_calc_lst       = tail2(bg_aado2_calc        , n=1    ),
    # bg_aado2_calc_max       = max  (bg_aado2_calc        , na.rm=T),
    # bg_aado2_calc_min       = min  (bg_aado2_calc        , na.rm=T),
    # bg_pao2fio2ratio_lst    = tail2(bg_pao2fio2ratio     , n=1    ),
    # bg_pao2fio2ratio_max    = max  (bg_pao2fio2ratio     , na.rm=T),
    # bg_pao2fio2ratio_min    = min  (bg_pao2fio2ratio     , na.rm=T),
    # bg_ph_lst               = tail2(bg_ph                , n=1    ),
    # bg_ph_max               = max  (bg_ph                , na.rm=T),
    # bg_ph_min               = min  (bg_ph                , na.rm=T),
    # bg_baseexcess_lst       = tail2(bg_baseexcess        , n=1    ),
    # bg_baseexcess_max       = max  (bg_baseexcess        , na.rm=T),
    # bg_baseexcess_min       = min  (bg_baseexcess        , na.rm=T),
    # bg_totalco2_lst         = tail2(bg_totalco2          , n=1    ),
    # bg_totalco2_max         = max  (bg_totalco2          , na.rm=T),
    # bg_totalco2_min         = min  (bg_totalco2          , na.rm=T),
    # bg_lactate_lst          = tail2(bg_lactate       , n=1    ),
    # bg_lactate_max          = max  (bg_lactate           , na.rm=T),
    # bg_lactate_min          = min  (bg_lactate           , na.rm=T)
);
d.labs_t <- as.data.frame(d.labs_t);
d.labs_t <- mutate_all(d.labs_t, function(x) { ifelse(!is.infinite(x), x, NA) })

d.labs <- d.labs_t;

d.labs <- arrange(d.labs, subject_id);

d.labs <- left_join(
    x = select(d.cohort, subject_id),
    y = d.labs,
    by = c("subject_id")
);
d.labs <- arrange(d.labs, subject_id);

# ToDO:
# - Incorporate this in SQL query, not in here. 
d.labs <- mutate(d.labs,
    chem_bun_lst        = ifelse(chem_bun_lst < 3         ,   3, chem_bun_lst       ),
    chem_bun_max        = ifelse(chem_bun_max < 3         ,   3, chem_bun_max       ),
    chem_bun_min        = ifelse(chem_bun_min < 3         ,   3, chem_bun_min       ),
    chem_creatinine_lst = ifelse(chem_creatinine_lst < 0.3, 0.3, chem_creatinine_lst),
    chem_creatinine_max = ifelse(chem_creatinine_max < 0.3, 0.3, chem_creatinine_max),
    chem_creatinine_min = ifelse(chem_creatinine_min < 0.3, 0.3, chem_creatinine_min),
);

d.labs <- arrange(d.labs, subject_id);



# --------------------------------------------------------------------------------------------------
# Labs - baseline creatinine
# --------------------------------------------------------------------------------------------------

d.labs_cr_bl <- d.labs_cr_bl0;
d.labs_cr_bl <- arrange(d.labs_cr_bl, subject_id);



# --------------------------------------------------------------------------------------------------
# AKI stage
# --------------------------------------------------------------------------------------------------

d.sc_aki <- arrange(d.sc_aki0, subject_id, desc(sc_aki_tm));
d.sc_aki <- filter(d.sc_aki, 0 >= sc_aki_tm);

d.sc_aki_t <- group_by(d.sc_aki, subject_id);
d.sc_aki_t <- summarize(d.sc_aki_t, 
    sc_aki_st    = max(sc_aki_st   , na.rm=T),
    sc_aki_cr_st = max(sc_aki_cr_st, na.rm=T),
    sc_aki_uo_st = max(sc_aki_uo_st, na.rm=T),
);
d.sc_aki_t <- as.data.frame(d.sc_aki_t);
d.sc_aki_t <- mutate_all(d.sc_aki_t, function(x) { ifelse(!is.infinite(x), x, NA) })

d.sc_aki <- d.sc_aki_t;

d.sc_aki <- mutate(d.sc_aki, 
    sc_aki_cr_st = ifelse(!is.na(sc_aki_cr_st), sc_aki_cr_st, -1),
    sc_aki_uo_st = ifelse(!is.na(sc_aki_uo_st), sc_aki_uo_st, -1)
);
d.sc_aki <- mutate(d.sc_aki, 
    sc_aki_st = factor(sc_aki_st, levels=1:3),
    sc_aki_cr_st = factor(sc_aki_cr_st, levels=c(-1:3), labels=c("NA", "0", "1", "2", "3")),
    sc_aki_uo_st = factor(sc_aki_uo_st, levels=c(-1:3), labels=c("NA", "0", "1", "2", "3"))
);
d.sc_aki <- arrange(d.sc_aki, subject_id);



# --------------------------------------------------------------------------------------------------
# SOFA score
# --------------------------------------------------------------------------------------------------

d.sc_sofa <- inner_join(
    select(d.cohort, subject_id, sofa_tm_bl = sofa_tm),
    d.sc_sofa0,
    by = c("subject_id")
);
d.sc_sofa <- arrange(d.sc_sofa, subject_id, desc(sc_sofa_tm));
d.sc_sofa <- filter(d.sc_sofa, sc_sofa_tm <= sofa_tm_bl);

d.sc_sofa_t <- group_by(d.sc_sofa, subject_id);
d.sc_sofa_t <- summarize(d.sc_sofa_t, 
    sc_sofa_st = max(sc_sofa_st, na.rm=T)
);
d.sc_sofa_t <- as.data.frame(d.sc_sofa_t);
d.sc_sofa_t <- mutate_all(d.sc_sofa_t, function(x) { ifelse(!is.infinite(x), x, NA) })

d.sc_sofa <- d.sc_sofa_t;

d.sc_sofa <- arrange(d.sc_sofa, subject_id);



# --------------------------------------------------------------------------------------------------
# Fluid baseline
# --------------------------------------------------------------------------------------------------

d.ce_fluids <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.ce_fluids0,
    by = c("subject_id")
);

d.ce_fluids <- filter(d.ce_fluids, 0 >= starttime & starttime >= (in_tm - 0));

d.ce_fluids <- group_by(d.ce_fluids, subject_id);
d.ce_fluids <- summarize(d.ce_fluids, ce_fluids_vol = sum(nettotal));
d.ce_fluids <- as.data.frame(d.ce_fluids);

d.ce_fluids <- left_join(
    x = select(d.cohort, subject_id),
    y = d.ce_fluids,
    by = c("subject_id")
);
d.ce_fluids <- mutate(d.ce_fluids,
    ce_fluids  = (!is.na(ce_fluids_vol)) * 1L
);
d.ce_fluids <- mutate(d.ce_fluids, 
    ce_fluids_vol = ifelse(!is.na(ce_fluids_vol), ce_fluids_vol, 0)
);
dim(d.ce_fluids);

fb_ll <- quantile(d.ce_fluids$ce_fluids_vol[d.ce_fluids$ce_fluids==0], 0.015, type=3);
fb_up <- quantile(d.ce_fluids$ce_fluids_vol[d.ce_fluids$ce_fluids==0], 0.985, type=3);
d.ce_fluids <- mutate(d.ce_fluids, ce_fluids_vol = ifelse(ce_fluids_vol >= fb_ll, ce_fluids_vol, fb_ll) );
d.ce_fluids <- mutate(d.ce_fluids, ce_fluids_vol = ifelse(ce_fluids_vol <= fb_up, ce_fluids_vol, fb_up) );

d.ce_fluids <- arrange(d.ce_fluids, subject_id);



# --------------------------------------------------------------------------------------------------
# Urine output
# --------------------------------------------------------------------------------------------------

d.ce_uo <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.ce_uo0,
    by = c("subject_id")
);

d.ce_uo <- filter(d.ce_uo, 0 >= charttime & charttime >= (in_tm - 0));

d.ce_uo <- group_by(d.ce_uo, subject_id);
d.ce_uo <- summarize(d.ce_uo,
    ce_uo_vol = sum(urineoutput)
);
d.ce_uo <- as.data.frame(d.ce_uo);

d.ce_uo <- left_join(
    x = select(d.cohort, subject_id),
    y = d.ce_uo,
    by = c("subject_id")
);

d.ce_uo <- mutate(d.ce_uo,
    ce_uo = (!is.na(ce_uo_vol)) * 1L
);
d.ce_uo <- mutate(d.ce_uo,
    ce_uo_vol = ifelse(!is.na(ce_uo_vol), ce_uo_vol, 0)
);

d.ce_uo <- arrange(d.ce_uo, subject_id);



# --------------------------------------------------------------------------------------------------
# Tx - Fluid in
# --------------------------------------------------------------------------------------------------

d.ce_fi <- d.ce_fluids0;
dim(d.ce_fi);

d.ce_fi <- inner_join(
    x = d.fu_raki_e,
    y = d.ce_fi,
    by = c('subject_id')
);
d.ce_fi <- filter(d.ce_fi, 1 > starttime & starttime > 0);
d.ce_fi <- filter(d.ce_fi, (fu_raki_st == 0) | (fu_raki_st == 1 & fu_raki_tm > starttime) );

d.ce_fi[is.na(d.ce_fi)] <- 0;
d.ce_fi <- filter(d.ce_fi, amount_colloid!=0 | amount_crystalloid!=0);

d.ce_fi <- mutate(d.ce_fi, amount_in = amount_colloid + amount_crystalloid);
d.ce_fi <- group_by(d.ce_fi, subject_id);
d.ce_fi <- summarize(d.ce_fi, amount_in = sum(amount_in));
d.ce_fi <- as.data.frame(d.ce_fi);

d.ce_fi <- left_join(
    x = select(d.cohort, subject_id),
    y = d.ce_fi,
    by = c("subject_id")
);
d.ce_fi <- mutate(d.ce_fi, 
    amount_in = ifelse(!is.na(amount_in), amount_in, 0)
);



# --------------------------------------------------------------------------------------------------
# Tx - Vasopressor
# --------------------------------------------------------------------------------------------------

d.tx_vaso <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.tx_vaso0,
    by = c("subject_id")
);

d.tx_vaso <- filter(d.tx_vaso, !(endtime < in_tm | 0 < starttime));

d.tx_vaso <- mutate(d.tx_vaso,
    tx_ned_tm = (pmin(endtime, 0) - pmax(starttime, in_tm)) * 24,
    tx_ned_vol = tx_ned * tx_ned_tm
);
d.tx_vaso <- as.data.frame(d.tx_vaso);
d.tx_vaso <- filter(d.tx_vaso, tx_ned_vol!=0);

d.tx_vaso <- group_by(d.tx_vaso, subject_id);
d.tx_vaso <- summarize(d.tx_vaso,
    tx_ned_tm = sum(tx_ned_tm, na.rm=T),
    tx_ned_vol = sum(tx_ned_vol, na.rm=T)
);
d.tx_vaso <- as.data.frame(d.tx_vaso);
d.tx_vaso <- filter(d.tx_vaso, tx_ned_vol!=0);

d.tx_vaso <- left_join(
    x = select(d.cohort, subject_id),
    y = d.tx_vaso,
    by = c("subject_id")
);

d.tx_vaso <- mutate(d.tx_vaso,
    tx_ned    = (!is.na(tx_ned_vol)) * 1L,
    tx_ned_tm = ifelse(!is.na(tx_ned_tm), tx_ned_tm, 0),
    tx_ned_vol = ifelse(!is.na(tx_ned_vol), tx_ned_vol, 0)
);
d.tx_vaso <- select(d.tx_vaso, -tx_ned_vol);

d.tx_vaso <- arrange(d.tx_vaso, subject_id);



# --------------------------------------------------------------------------------------------------
# Tx - Ventilation
# --------------------------------------------------------------------------------------------------

d.tx_vent <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.tx_vent0,
    by = c("subject_id")
);
d.tx_vent <- filter(d.tx_vent, !(endtime < in_tm | 0 < starttime));

d.tx_vent <- mutate(d.tx_vent,
    tx_vent_tm = (pmin(endtime, 0) - pmax(starttime, in_tm)) * 24,
    tx_vent_vol = tx_vent * tx_vent_tm
);
d.tx_vent <- as.data.frame(d.tx_vent);
d.tx_vent <- filter(d.tx_vent, tx_vent_vol!=0);

d.tx_vent <- group_by(d.tx_vent, subject_id);
d.tx_vent <- summarize(d.tx_vent,
    tx_vent_tm = sum(tx_vent_tm, na.rm=T),
    tx_vent_vol = sum(tx_vent_vol, na.rm=T)
);
d.tx_vent <- as.data.frame(d.tx_vent);
d.tx_vent <- filter(d.tx_vent, tx_vent_vol!=0);

d.tx_vent <- left_join(
    x = select(d.cohort, subject_id),
    y = d.tx_vent,
    by = c("subject_id")
);

d.tx_vent <- mutate(d.tx_vent,
    tx_vent    = (!is.na(tx_vent_vol)) * 1L,
    tx_vent_tm = ifelse(!is.na(tx_vent_tm), tx_vent_vol, 0),
    tx_vent_vol = ifelse(!is.na(tx_vent_vol), tx_vent_vol, 0)
);
d.tx_vent <- select(d.tx_vent, -tx_vent_vol);

d.tx_vent <- arrange(d.tx_vent, subject_id);



# --------------------------------------------------------------------------------------------------
# Tx - Nephrotoxins
# --------------------------------------------------------------------------------------------------

d.tx_nt <- inner_join(
    x = select(d.cohort, subject_id, in_tm),
    y = d.tx_nt0,
    by = c("subject_id")
);
d.tx_nt <- filter(d.tx_nt, 0 >= charttime & charttime >= (- 2));

d.tx_nt <- group_by(d.tx_nt, subject_id);
d.tx_nt <- summarize(d.tx_nt, tx_nt = 1);
d.tx_nt <- as.data.frame(d.tx_nt);

d.tx_nt <- left_join(
    x = select(d.cohort, subject_id),
    y = d.tx_nt,
    by = c("subject_id")
);
d.tx_nt <- mutate(d.tx_nt,
    tx_nt = ifelse(!is.na(tx_nt), tx_nt, 0)
);

d.tx_nt <- arrange(d.tx_nt, subject_id);





# ==================================================================================================
# Data merging
# ==================================================================================================

# merge
d.ft_raw <- left_join(x=d.demo  , y=d.vitals    , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.labs      , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.labs_cr_bl, by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.sc_aki    , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.sc_sofa   , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.ce_fluids , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.ce_uo     , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.tx_vaso   , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.tx_vent   , by="subject_id");
d.ft_raw <- left_join(x=d.ft_raw, y=d.tx_nt     , by="subject_id");
d.ft_raw <- arrange(d.ft_raw, subject_id);
all(d.cohort$subject_id == d.ft_raw$subject_id);

# Flag for missing data
d.ft_na <- is.na(d.ft_raw);
d.ft_na <- d.ft_na * 1L
d.ft_na <- d.ft_na[, !(colMeans(d.ft_na) == 0)];
colnames(d.ft_na) <- paste(colnames(d.ft_na), "_na", sep="");
d.ft_na <- data.frame(select(d.cohort, subject_id), d.ft_na);





# ==================================================================================================
# Missing data imputation
# ==================================================================================================

d.ft_imputed <- d.ft_raw
mice:::find.collinear(select(d.ft_imputed, -subject_id));
## character(0)
mice.ft_i <- mice::mice(d.ft_imputed[, -1], m=5, maxit=5, meth='pmm', seed=0);
d.ft_imputed[, 2:ncol(d.ft_imputed)] <- mice::complete(mice.ft_i);





# ==================================================================================================
# save
# ==================================================================================================

save(
    d.cohort, d.fu_raki_e, d.fu_raki_s, d.fu_death, d.fu_make_eoh, 
    d.ft_raw, d.ft_na, d.ft_imputed, d.ce_fi, 
    file="data/mimic.study0.20240505.rd"
);









