
# ==================================================================================================
# Library
# ==================================================================================================

library(dplyr);
library(tidyr);
library(parallel);





# ==================================================================================================
# Load
# ==================================================================================================

d.cohort0       <- read.csv("data.0/mimic.cohort.20240505.csv"                 );
d.demo0         <- read.csv("data.0/mimic.demo.20240505.csv"                   );
d.vitals0       <- read.csv("data.0/mimic.vitals.20240505.csv"                 );
d.labs0         <- read.csv("data.0/mimic.labs.20240505.csv"                   );
d.labs_cr_bl0   <- read.csv("data.0/mimic.labs_chem_creatinine_bl.20240505.csv");
d.sc_aki0       <- read.csv("data.0/mimic.sc_aki.20240505.csv"                 );
d.sc_sofa0      <- read.csv("data.0/mimic.sc_sofa.20240505.csv"                );
d.ce_fluids0    <- read.csv("data.0/mimic.ce_fluids.20240505.csv"              );
d.ce_uo0        <- read.csv("data.0/mimic.ce_uo.20240505.csv"                  );
d.tx_vaso0      <- read.csv("data.0/mimic.tx_vaso.20240505.csv"                );
d.tx_vent0      <- read.csv("data.0/mimic.tx_vent.20240505.csv"                );
d.tx_nt0        <- read.csv("data.0/mimic.tx_nephrotoxins.20240505.csv"        );
d.fu_death0     <- read.csv("data.0/mimic.fu_death.20240505.csv"               );
d.fu_raki_e0    <- read.csv("data.0/mimic.fu_raki_e.20240505.csv"              );
d.fu_raki_s0    <- read.csv("data.0/mimic.fu_raki_s.20240505.csv"              );





# ==================================================================================================
# Initial preprocessing
# ==================================================================================================

# --------------------------------------------------------------------------------------------------
# cohort
# --------------------------------------------------------------------------------------------------

d.cohort0 <- mutate(d.cohort0,
    sofa_dt     = as.POSIXct(sofa_dt      , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    si_dt       = as.POSIXct(si_dt      , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    sepsis_dt   = as.POSIXct(sepsis_dt  , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    aki_dt      = as.POSIXct(aki_dt     , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    diuretic_dt = as.POSIXct(diuretic_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    baseline_dt = as.POSIXct(baseline_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    admit_dt    = as.POSIXct(admit_dt   , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    disch_dt    = as.POSIXct(disch_dt   , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    in_dt       = as.POSIXct(in_dt      , format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    out_dt      = as.POSIXct(out_dt     , format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.cohort0 <- mutate(d.cohort0,
    sofa_tm     = as.numeric(difftime(sofa_dt    , baseline_dt, units="days")),
    si_tm       = as.numeric(difftime(si_dt      , baseline_dt, units="days")),
    sepsis_tm   = as.numeric(difftime(sepsis_dt  , baseline_dt, units="days")),
    aki_tm      = as.numeric(difftime(aki_dt     , baseline_dt, units="days")),
    diuretic_tm = as.numeric(difftime(diuretic_dt, baseline_dt, units="days")),
    admit_tm    = as.numeric(difftime(admit_dt   , baseline_dt, units="days")),
    disch_tm    = as.numeric(difftime(disch_dt   , baseline_dt, units="days")),
    in_tm       = as.numeric(difftime(in_dt      , baseline_dt, units="days")),
    out_tm      = as.numeric(difftime(out_dt     , baseline_dt, units="days"))
);
d.cohort0 <- select(d.cohort0, 
    subject_id, hadm_id, stay_id, 
    baseline_dt, 
    sofa_tm, si_tm, sepsis_tm, aki_tm, diuretic_tm, admit_tm, disch_tm, in_tm, out_tm
);
d.cohort0 <- arrange(d.cohort0, subject_id);

# Fluid intake constrain. 
d.ce_fi <- mutate(d.ce_fluids0,
    starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.ce_fi <- left_join(
    x = d.ce_fi,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.ce_fi <- mutate(d.ce_fi,
    starttime = as.numeric(difftime(starttime, baseline_dt, units="days"))
);
d.ce_fi <- select(d.ce_fi, -baseline_dt);
d.ce_fi <- arrange(d.ce_fi, subject_id, starttime);

d.ce_fi <- filter(d.ce_fi, starttime <= 1);
d.ce_fi <- filter(d.ce_fi, !is.na(amount_colloid) | !is.na(amount_crystalloid));
d.ce_fi <- filter(d.ce_fi, amount_colloid!=0 | amount_crystalloid!=0);

d.cohort0 <- filter(d.cohort0, subject_id %in% d.ce_fi$subject_id);

d.demo0        <- filter(d.demo0       , subject_id %in% d.cohort0$subject_id);
d.vitals0      <- filter(d.vitals0     , subject_id %in% d.cohort0$subject_id);
d.labs0        <- filter(d.labs0       , subject_id %in% d.cohort0$subject_id);
d.labs_cr_bl0  <- filter(d.labs_cr_bl0 , subject_id %in% d.cohort0$subject_id);
d.sc_aki0      <- filter(d.sc_aki0     , subject_id %in% d.cohort0$subject_id);
d.sc_sofa0     <- filter(d.sc_sofa0    , subject_id %in% d.cohort0$subject_id);
d.ce_uo0       <- filter(d.ce_uo0      , subject_id %in% d.cohort0$subject_id);
d.ce_fluids0   <- filter(d.ce_fluids0  , subject_id %in% d.cohort0$subject_id);
d.tx_vaso0     <- filter(d.tx_vaso0    , subject_id %in% d.cohort0$subject_id);
d.tx_vent0     <- filter(d.tx_vent0    , subject_id %in% d.cohort0$subject_id);
d.tx_nt0       <- filter(d.tx_nt0      , subject_id %in% d.cohort0$subject_id);
d.fu_death0    <- filter(d.fu_death0   , subject_id %in% d.cohort0$subject_id);
d.fu_raki_e0   <- filter(d.fu_raki_e0  , subject_id %in% d.cohort0$subject_id);
d.fu_raki_s0   <- filter(d.fu_raki_s0  , subject_id %in% d.cohort0$subject_id);



# --------------------------------------------------------------------------------------------------
# demo
# --------------------------------------------------------------------------------------------------

sel.w <- grepl("^WHITE", d.demo0$dm_race);
sel.b <- grepl("^BLACK", d.demo0$dm_race);
sel.h <- grepl("^HISPANIC", d.demo0$dm_race);
sel.o <- !(sel.w | sel.b | sel.h);

d.demo0$dm_race[sel.w] = "W";
d.demo0$dm_race[sel.b] = "B";
d.demo0$dm_race[sel.h] = "H";
d.demo0$dm_race[sel.o] = "O";

d.demo0 <- mutate(d.demo0,
    dm_race = factor(x=dm_race, levels=c("W", "B", "H", "O")),
    dm_gender = factor(x=dm_gender, levels=c("M", "F"))
);
d.demo0 <- arrange(d.demo0, subject_id);



# --------------------------------------------------------------------------------------------------
# death
# --------------------------------------------------------------------------------------------------

d.fu_death0 <- mutate(d.fu_death0,
    death_dt = as.POSIXct(death_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.fu_death0 <- left_join(
    x = d.fu_death0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.fu_death0 <- mutate(d.fu_death0,
    death_tm = as.numeric(difftime(death_dt, baseline_dt, units="days"))
);
d.fu_death0 <- select(d.fu_death0, subject_id, death_st, death_tm);
d.fu_death0 <- arrange(d.fu_death0, subject_id);



# --------------------------------------------------------------------------------------------------
# vitals
# --------------------------------------------------------------------------------------------------

d.vitals0 <- mutate(d.vitals0,
    charttime = as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.vitals0 <- left_join(
    x = d.vitals0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.vitals0 <- mutate(d.vitals0,
    vs_tm = as.numeric(difftime(charttime, baseline_dt, units="days"))
);
d.vitals0 <- select(d.vitals0, 
    subject_id, vs_tm, vs_heart_rate:vs_weight
);
d.vitals0 <- arrange(d.vitals0, subject_id, vs_tm);



# --------------------------------------------------------------------------------------------------
# labs
# --------------------------------------------------------------------------------------------------

d.labs0 <- mutate(d.labs0,
    charttime = as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.labs0 <- left_join(
    x = d.labs0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.labs0 <- mutate(d.labs0,
    le_tm = as.numeric(difftime(charttime, baseline_dt, units="days"))
);
d.labs0 <- d.labs0[, c(
    'subject_id', 'le_tm', 
    colnames(d.labs0)[grepl('^cbc', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^chem', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^diff', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^coag', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^enz', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^cm', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^bg', colnames(d.labs0))],
    colnames(d.labs0)[grepl('^cvp', colnames(d.labs0))]
)]
d.labs0 <- select(d.labs0, -cbc_rdwsd);
d.labs0 <- arrange(d.labs0, subject_id, le_tm);



# --------------------------------------------------------------------------------------------------
# labs
# --------------------------------------------------------------------------------------------------

d.labs_cr_bl0 <- arrange(d.labs_cr_bl0, subject_id);



# --------------------------------------------------------------------------------------------------
# sc_aki
# --------------------------------------------------------------------------------------------------

d.sc_aki0 <- mutate(d.sc_aki0,
    sc_aki_dt = as.POSIXct(sc_aki_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.sc_aki0 <- left_join(
    x = d.sc_aki0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.sc_aki0 <- mutate(d.sc_aki0,
    sc_aki_tm = as.numeric(difftime(sc_aki_dt, baseline_dt, units="days"))
);
d.sc_aki0 <- select(d.sc_aki0, subject_id, sc_aki_tm, sc_aki_st, sc_aki_cr_st, sc_aki_uo_st, sc_aki_rr_st);
d.sc_aki0 <- arrange(d.sc_aki0, subject_id, sc_aki_tm);



# --------------------------------------------------------------------------------------------------
# sc_sofa0
# --------------------------------------------------------------------------------------------------

d.sc_sofa0 <- mutate(d.sc_sofa0,
    starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.sc_sofa0 <- left_join(
    x = d.sc_sofa0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.sc_sofa0 <- mutate(d.sc_sofa0,
    sc_sofa_tm = as.numeric(difftime(starttime, baseline_dt, units="days"))
);
d.sc_sofa0 <- select(d.sc_sofa0, subject_id, sc_sofa_tm, sc_sofa_st = sc_sofa);
d.sc_sofa0 <- arrange(d.sc_sofa0, subject_id, sc_sofa_tm);



# --------------------------------------------------------------------------------------------------
# ce_fluids
# --------------------------------------------------------------------------------------------------

d.ce_fluids0 <- mutate(d.ce_fluids0,
    starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.ce_fluids0 <- left_join(
    x = d.ce_fluids0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.ce_fluids0 <- mutate(d.ce_fluids0,
    starttime = as.numeric(difftime(starttime, baseline_dt, units="days"))
);
d.ce_fluids0 <- select(d.ce_fluids0, -baseline_dt);
d.ce_fluids0 <- arrange(d.ce_fluids0, subject_id, starttime);



# --------------------------------------------------------------------------------------------------
# ce_uo
# --------------------------------------------------------------------------------------------------

d.ce_uo0 <- mutate(d.ce_uo0,
   charttime = as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.ce_uo0 <- left_join(
   x = d.ce_uo0,
   y = select(d.cohort0, subject_id, baseline_dt),
   by = c("subject_id")
);
d.ce_uo0 <- mutate(d.ce_uo0,
   charttime = as.numeric(difftime(charttime, baseline_dt, units="days"))
);
d.ce_uo0 <- select(d.ce_uo0, subject_id, charttime, urineoutput);
d.ce_uo0 <- arrange(d.ce_uo0, subject_id, charttime);



# --------------------------------------------------------------------------------------------------
# tx_vaso
# --------------------------------------------------------------------------------------------------

d.tx_vaso0 <- mutate(d.tx_vaso0,
    starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    endtime   = as.POSIXct(endtime  , format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.tx_vaso0 <- left_join(
    x = d.tx_vaso0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.tx_vaso0 <- mutate(d.tx_vaso0,
    starttime = as.numeric(difftime(starttime, baseline_dt, units="days")),
    endtime   = as.numeric(difftime(endtime  , baseline_dt, units="days"))
);
d.tx_vaso0 <- select(d.tx_vaso0, subject_id, starttime, endtime, tx_ned=tx_nee);
d.tx_vaso0 <- arrange(d.tx_vaso0, subject_id, starttime, endtime);



# --------------------------------------------------------------------------------------------------
# tx_vent
# --------------------------------------------------------------------------------------------------

d.tx_vent0 <- mutate(d.tx_vent0,
    starttime = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%S", tz="GMT"),
    endtime   = as.POSIXct(endtime  , format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.tx_vent0 <- left_join(
    x = d.tx_vent0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.tx_vent0 <- mutate(d.tx_vent0,
    starttime = as.numeric(difftime(starttime, baseline_dt, units="days")),
    endtime   = as.numeric(difftime(endtime  , baseline_dt, units="days"))
);
d.tx_vent0 <- select(d.tx_vent0, subject_id, starttime, endtime, tx_vent);
d.tx_vent0 <- arrange(d.tx_vent0, subject_id, starttime, endtime);



# --------------------------------------------------------------------------------------------------
# tx_nephrotoxins
# --------------------------------------------------------------------------------------------------

d.nt_nt0 <- mutate(d.tx_nt0,
    charttime = as.POSIXct(charttime, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.tx_nt0 <- left_join(
    x = d.tx_nt0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.tx_nt0 <- mutate(d.tx_nt0,
    charttime = as.numeric(difftime(charttime, baseline_dt, units="days"))
);
d.tx_nt0 <- select(d.tx_nt0, subject_id, charttime, medication, event_txt, route, drug_type);
d.tx_nt0 <- arrange(d.tx_nt0, subject_id, charttime);



# --------------------------------------------------------------------------------------------------
# fu_raki_e
# --------------------------------------------------------------------------------------------------

d.fu_raki_e0 <- mutate(d.fu_raki_e0,
    raki_dt = as.POSIXct(raki_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.fu_raki_e0 <- left_join(
    x = d.fu_raki_e0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.fu_raki_e0 <- mutate(d.fu_raki_e0,
    fu_raki_tm = as.numeric(difftime(raki_dt, baseline_dt, units="days"))
);
d.fu_raki_e0 <- select(d.fu_raki_e0, subject_id, fu_raki_tm, fu_raki_st=raki_st);
d.fu_raki_e0 <- arrange(d.fu_raki_e0, subject_id, fu_raki_tm);



# --------------------------------------------------------------------------------------------------
# fu_raki_s
# --------------------------------------------------------------------------------------------------

d.fu_raki_s0 <- mutate(d.fu_raki_s0,
    raki_dt = as.POSIXct(raki_dt, format="%Y-%m-%d %H:%M:%S", tz="GMT")
);
d.fu_raki_s0 <- left_join(
    x = d.fu_raki_s0,
    y = select(d.cohort0, subject_id, baseline_dt),
    by = c("subject_id")
);
d.fu_raki_s0 <- mutate(d.fu_raki_s0,
    fu_raki_tm = as.numeric(difftime(raki_dt, baseline_dt, units="days"))
);
d.fu_raki_s0 <- select(d.fu_raki_s0, subject_id, fu_raki_tm, fu_raki_st=raki_st);
d.fu_raki_s0 <- arrange(d.fu_raki_s0, subject_id, fu_raki_tm);





# ==================================================================================================
# Save
# ==================================================================================================

save(
    d.cohort0, d.demo0, d.fu_death0, d.fu_raki_e0, d.fu_raki_s0, 
    d.vitals0, d.labs0, d.labs_cr_bl0, 
    d.sc_aki0, d.sc_sofa0, 
    d.ce_fluids0, d.ce_uo0, 
    d.tx_vaso0, d.tx_vent0, d.tx_nt0,
    file = "data/mimic.rawdata.20240505.rd"
);









