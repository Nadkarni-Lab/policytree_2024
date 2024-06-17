
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

# MIMIC
load("data/mimic.rawdata.20240505.rd");
load("data/mimic.study0.20240505.rd");
load("data/mimic.study.20240505.rd");
load("data/mimic.ite.20240505.rd");
load("data/mimic.policytree.20240505.rd");
d.m_cohort    <- d.cohort;
d.m_ft_raw    <- d.ft_raw;
d.m_uo        <- d.ce_uo0;
d.m_fluids    <- d.ce_fluids0;


# eICU
load("data/eicu.rawdata.20240505.rd");
load("data/eicu.study0.20240505.rd");
load("data/eicu.study.20240505.rd");
d.e_cohort    <- d.cohort;
d.e_ft_raw    <- d.ft_raw;
d.e_uo        <- d.ce_uo0;
d.e_fluids    <- d.ce_fluids0;





# ==================================================================================================
# Preprocessing
# ==================================================================================================

d.ft_dv <- d.m_ft2[, head(v.m_tau_cf, 25)];
d.ft_vd <- d.e_ft2[, head(v.m_tau_cf, 25)];

a.dv <- predict(pt.m_d5, d.ft_dv, type="action.id");
a.dv <- a.dv==2;

a.vd <- predict(pt.m_d5, d.ft_vd, type="action.id");
a.vd <- a.vd==2;





# ==================================================================================================
# 
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# MIMIC
# --------------------------------------------------------------------------------------------------

d.m_cohort <- left_join(
    x = d.m_cohort,
    y = select(d.m_fu_death, subject_id, fu_death_st=status),
    by = c('subject_id')
);
d.m_cohort <- mutate(d.m_cohort, 
    sepsis_tm = ifelse(sepsis_tm >= in_tm, sepsis_tm, in_tm) * 24,
    disch_tm  = ifelse(fu_death_st==0, disch_tm, NA),
    in_tm     = in_tm * 24,
    out_tm    = ifelse(fu_death_st==0, out_tm, NA)
);
d.m_fu_death <- mutate(d.m_fu_death,
    fu_death_tm = ifelse(time==1, time, NA),
    fu_death_st = factor(status, levels=0:1, labels=c('Dead: No', 'Dead: Yes'))
)
d.m_fluids <- mutate(d.m_fluids, 
    amount_colloid=ifelse(!is.na(amount_colloid), amount_colloid, 0), 
    amount_crystalloid=ifelse(!is.na(amount_crystalloid), amount_crystalloid, 0)
);
d.m_tx <- factor(d.m_tx, levels=0:1, labels=c('Tx: No', 'Tx: Yes'))
d.m_fu_raki_e <- mutate(d.m_fu_raki_e, 
    fu_raki_e_st = factor(status, levels=0:1)
);
d.m_fu_raki_s <- mutate(d.m_fu_raki_s, 
    fu_raki_s_st = factor(status, levels=0:1)
);
d.m_fu_make_eoh <- mutate(d.m_fu_make_eoh,
    fu_make_eoh_st = factor(status, levels=0:1)
);
d.m_ft_raw <- mutate(d.m_ft_raw,
    ce_fluids= factor(ce_fluids, levels=0:1),
    ce_uo    = factor(ce_uo, levels=0:1), 
    tx_ned   = factor(tx_ned, levels=0:1), 
    tx_vent  = factor(tx_vent, levels=0:1), 
    tx_nt       = factor(tx_nt, levels=0:1)
)
fb_in_aki <- summarize(group_by(left_join(select(d.m_cohort, subject_id), filter(inner_join(select(d.m_cohort, subject_id, in_tm), d.m_fluids, by=c('subject_id')), in_tm <= starttime & starttime < 0), by=c('subject_id')), subject_id), fb=sum(nettotal, na.rm=T))$fb
uo_in_aki <- summarize(group_by(left_join(select(d.m_cohort, subject_id), filter(inner_join(select(d.m_cohort, subject_id, in_tm), d.m_uo, by=c('subject_id')), in_tm <= charttime & charttime <= 0), by=c('subject_id')), subject_id), uo=sum(urineoutput, na.rm=T))$uo

tbl1_m <- arsenal::tableby(tx_rec ~ ., 
    data.frame(
        tx_rec = factor(a.dv, levels=c(F, T), labels=c('Rec: No', 'Rec:Yes')), 
        tx_fi = d.m_tx, 
        select(d.m_cohort, in_tm, sepsis_tm, out_tm, disch_tm),
        select(d.m_fu_raki_e, fu_raki_e_st),
        select(d.m_fu_raki_s, fu_raki_s_st),
        select(d.m_fu_death, fu_death_tm, fu_death_st),
        select(d.m_fu_make_eoh, fu_make_eoh_st),
        select(d.m_ft_raw, -subject_id, -ce_fluids_vol, -ce_uo_vol),
        fb_in_aki = fb_in_aki, 
        uo_in_aki = uo_in_aki
    ), numeric.stats = c("Nmiss", "meansd"), digits=2)
arsenal::write2word(tbl1_m, paste(getwd(), '/tbl/tbl1_m.doc', sep=''))

tbl2_m <- arsenal::tableby(tx_fi ~ ., 
    data.frame(
        tx_fi = d.m_tx, 
        select(d.m_cohort, in_tm, sepsis_tm, out_tm, disch_tm),
        select(d.m_fu_raki_e, fu_raki_e_st),
        select(d.m_fu_raki_s, fu_raki_s_st),
        select(d.m_fu_death, fu_death_tm, fu_death_st),
        select(d.m_fu_make_eoh, fu_make_eoh_st),
        select(d.m_ft_raw, -subject_id, -ce_fluids_vol, -ce_uo_vol),
        fb_in_aki = fb_in_aki, 
        uo_in_aki = uo_in_aki
    )[a.dv, ], numeric.stats = c("Nmiss", "meansd"), digits=2)
arsenal::write2word(tbl2_m, paste(getwd(), '/tbl/tbl2_m.doc', sep=''))



# --------------------------------------------------------------------------------------------------
# 
# --------------------------------------------------------------------------------------------------

d.e_cohort <- left_join(
    x = d.e_cohort,
    y = select(d.e_fu_death, subject_id, fu_death_st=status),
    by = c('subject_id')
);
d.e_cohort <- mutate(d.e_cohort, 
    sepsis_tm = ifelse(sepsis_tm >= in_tm, sepsis_tm, in_tm) * 24,
    disch_tm  = ifelse(fu_death_st==0, disch_tm, NA),
    in_tm     = in_tm * 24,
    out_tm    = ifelse(fu_death_st==0, out_tm, NA)
);
d.e_fu_death <- mutate(d.e_fu_death,
    fu_death_tm = ifelse(time==1, time, NA),
    fu_death_st = factor(status, levels=0:1, labels=c('Dead: No', 'Dead: Yes'))
)
d.e_fluids <- mutate(d.e_fluids, 
    amount_colloid=ifelse(!is.na(amount_colloid), amount_colloid, 0), 
    amount_crystalloid=ifelse(!is.na(amount_crystalloid), amount_crystalloid, 0)
);
d.e_tx <- factor(d.e_tx, levels=0:1, labels=c('Tx: No', 'Tx: Yes'))
d.e_fu_raki_e <- mutate(d.e_fu_raki_e, 
    fu_raki_e_st = factor(status, levels=0:1)
);
d.e_fu_raki_s <- mutate(d.e_fu_raki_s, 
    fu_raki_s_st = factor(status, levels=0:1)
);
d.e_fu_make_eoh <- mutate(d.e_fu_make_eoh,
    fu_make_eoh_st = factor(status, levels=0:1)
);
d.e_ft_raw <- mutate(d.e_ft_raw,
    ce_fluids= factor(ce_fluids, levels=0:1),
    ce_uo    = factor(ce_uo, levels=0:1), 
    tx_ned   = factor(tx_ned, levels=0:1), 
    tx_vent  = factor(tx_vent, levels=0:1), 
    tx_nt       = factor(tx_nt, levels=0:1)
)
fb_in_aki <- summarize(group_by(left_join(select(d.e_cohort, subject_id), filter(inner_join(select(d.e_cohort, subject_id, in_tm), d.e_fluids, by=c('subject_id')), in_tm <= starttime & starttime < 0), by=c('subject_id')), subject_id), fb=sum(nettotal, na.rm=T))$fb
uo_in_aki <- summarize(group_by(left_join(select(d.e_cohort, subject_id), filter(inner_join(select(d.e_cohort, subject_id, in_tm), d.e_uo, by=c('subject_id')), in_tm <= charttime & charttime <= 0), by=c('subject_id')), subject_id), uo=sum(urineoutput, na.rm=T))$uo

tbl1_e <- arsenal::tableby(tx_rec ~ ., 
    data.frame(
        tx_rec = factor(a.vd, levels=c(F, T), labels=c('Rec: No', 'Rec:Yes')), 
        tx_fi = d.e_tx, 
        select(d.e_cohort, in_tm, sepsis_tm, out_tm, disch_tm),
        select(d.e_fu_raki_e, fu_raki_e_st),
        select(d.e_fu_raki_s, fu_raki_s_st),
        select(d.e_fu_death, fu_death_tm, fu_death_st),
        select(d.e_fu_make_eoh, fu_make_eoh_st),
        select(d.e_ft_raw, -subject_id, -ce_fluids_vol, -ce_uo_vol),
        fb_in_aki = fb_in_aki, 
        uo_in_aki = uo_in_aki
    ), numeric.stats = c("Nmiss", "meansd"), digits=2)
arsenal::write2word(tbl1_e, paste(getwd(), '/tbl/tbl1_e.doc', sep=''))

tbl2_e <- arsenal::tableby(tx_fi ~ ., 
    data.frame(
        tx_fi = d.e_tx, 
        select(d.e_cohort, in_tm, sepsis_tm, out_tm, disch_tm),
        select(d.e_fu_raki_e, fu_raki_e_st),
        select(d.e_fu_raki_s, fu_raki_s_st),
        select(d.e_fu_death, fu_death_tm, fu_death_st),
        select(d.e_fu_make_eoh, fu_make_eoh_st),
        select(d.e_ft_raw, -subject_id, -ce_fluids_vol, -ce_uo_vol),
        fb_in_aki = fb_in_aki, 
        uo_in_aki = uo_in_aki
    )[a.vd, ], numeric.stats = c("Nmiss", "meansd"), digits=2)
arsenal::write2word(tbl2_e, paste(getwd(), '/tbl/tbl2_e.doc', sep=''))









