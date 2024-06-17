
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
load("data/mimic.study.20240505.rd");
load("data/mimic.policytree.20240505.rd");
load("data/mimic.ite.20240505.rd");

# eICU
load("data/eicu.study.20240505.rd");






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
# Tree diagram
# ==================================================================================================

cat(DiagrammeRsvg::export_svg(plot(pt.m_d5)), file = 'fig/fig_3.svg')





# ==================================================================================================
# Ratio
# ==================================================================================================

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

d.e_p_r <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Early AKI reversal', 'Sustained AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge', 'MAKE at Discharge'),
    tx = c('No', 'Yes', 'No', 'Yes', 'No', 'Yes'),
    p = c(0.430, 0.799, 0.262, 0.674, 0.315, 0.154)
);
d.m_p_r <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Early AKI reversal', 'Sustained AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge', 'MAKE at Discharge'),
    tx = c('No', 'Yes', 'No', 'Yes', 'No', 'Yes'),
    p = c(0.396, 0.482, 0.274, 0.367, 0.351, 0.293)
);
d.e_p_n <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Early AKI reversal', 'Sustained AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge', 'MAKE at Discharge'),
    tx = c('No', 'Yes', 'No', 'Yes', 'No', 'Yes'),
    p = c(0.404, 0.160, 0.263, 0.147, 0.470, 0.480)
);
d.m_p_n <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Early AKI reversal', 'Sustained AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge', 'MAKE at Discharge'),
    tx = c('No', 'Yes', 'No', 'Yes', 'No', 'Yes'),
    p = c(0.326, 0.270, 0.207, 0.175, 0.500, 0.635)
);

d.p <- rbind(d.e_p_r, d.m_p_r, d.e_p_n, d.m_p_n);
d.p <- mutate(d.p,
    cohort = factor(cohort, levels=c('Development cohort\n(MIMIC-IV)', 'External validation cohort\n(eICU)')),
    rec = factor(rec, levels=c('Identified to benefit\nfrom restrictive fluids', 'Identified to have no benefit\nfrom restrictive fluids')),
    outcomes = factor(outcomes, levels=c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge')),
    tx = factor(tx, levels=c('No', 'Yes'))
);

library(ggplot2);
library(ggsci);
g.fig_4a <- ggplot(d.p, aes(x=outcomes, y=p, fill=tx)) + 
    geom_rect(mapping=aes(fill=cohort), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.05) +
    geom_bar(stat="identity", position=position_dodge(), width=0.5) +
    facet_grid(vars(cohort), vars(rec)) + 
    xlab('Outcomes') +
    ylab('Proportion (%)') + 
    coord_cartesian(ylim = c(0.10, 0.90)) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(
        name='Restrictive Fluid Intake', 
        values=c('Development cohort\n(MIMIC-IV)'='green', 'External validation cohort\n(eICU)'='orange', No='#BC3C29FF', Yes='#0072B5FF'),
        breaks=c('No', 'Yes')
        ) +
    theme_bw(base_size=10) + 
    theme(legend.position='bottom') +
    theme(axis.text.x = element_text(angle = 30, hjust=1))
ggsave('fig/fig_4a.png', width=5, height=5, units='in', dpi=600)
ggsave('fig/fig_4a.pdf', width=5, height=5, units='in', dpi=600)





# ==================================================================================================
# Ratio
# ==================================================================================================

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


rbind(
tail(round(exp(cbind(coef(fit.dv.raki_e.2), confint(fit.dv.raki_e.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.5), confint(fit.dv.raki_e.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.3), confint(fit.dv.raki_e.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_e.6), confint(fit.dv.raki_e.6))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.raki_e.2), confint(fit.vd.raki_e.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.5), confint(fit.vd.raki_e.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.3), confint(fit.vd.raki_e.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_e.6), confint(fit.vd.raki_e.6))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.dv.raki_s.2), confint(fit.dv.raki_s.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.5), confint(fit.dv.raki_s.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.3), confint(fit.dv.raki_s.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.raki_s.6), confint(fit.dv.raki_s.6))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.raki_s.2), confint(fit.vd.raki_s.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.5), confint(fit.vd.raki_s.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.3), confint(fit.vd.raki_s.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.raki_s.6), confint(fit.vd.raki_s.6))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.dv.make_eoh.2), confint(fit.dv.make_eoh.2))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.5), confint(fit.dv.make_eoh.5))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.3), confint(fit.dv.make_eoh.3))), 2), 1),
tail(round(exp(cbind(coef(fit.dv.make_eoh.6), confint(fit.dv.make_eoh.6))), 2), 1)
)

rbind(
tail(round(exp(cbind(coef(fit.vd.make_eoh.2), confint(fit.vd.make_eoh.2))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.5), confint(fit.vd.make_eoh.5))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.3), confint(fit.vd.make_eoh.3))), 2), 1),
tail(round(exp(cbind(coef(fit.vd.make_eoh.6), confint(fit.vd.make_eoh.6))), 2), 1)
)

d.e_or_u_r <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    type = 'Unadjusted OR',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(5.27, 5.81, 0.40),
    ll = c(4.40, 4.95, 0.32),
    ul = c(6.31, 6.81, 0.48)
);
d.e_or_a_r <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    type = 'Adjusted OR',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(5.12, 5.74, 0.42),
    ll = c(4.26, 4.88, 0.34),
    ul = c(6.17, 6.75, 0.52)
);
d.m_or_u_r <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    type = 'Unadjusted OR',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(1.41, 1.54, 0.77),
    ll = c(1.16, 1.24, 0.62),
    ul = c(1.73, 1.90, 0.95)
);
d.m_or_a_r <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    type = 'Adjusted OR',
    rec = 'Identified to benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(1.41, 1.54, 0.78),
    ll = c(1.14, 1.24, 0.62),
    ul = c(1.73, 1.91, 0.99)
);

d.e_or_u_n <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    type = 'Unadjusted OR',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(0.28, 0.48, 1.04),
    ll = c(0.18, 0.29, 0.73),
    ul = c(0.45, 0.78, 1.49)
);
d.e_or_a_n <- data.frame(
    cohort = 'Development cohort\n(MIMIC-IV)',
    type = 'Adjusted OR',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(0.22, 0.40, 1.12),
    ll = c(0.13, 0.23, 0.74),
    ul = c(0.36, 0.67, 1.71)
);
d.m_or_u_n <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    type = 'Unadjusted OR',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(0.76, 0.81, 1.74),
    ll = c(0.40, 0.39, 0.97),
    ul = c(1.44, 1.71, 3.13)
);
d.m_or_a_n <- data.frame(
    cohort = 'External validation cohort\n(eICU)',
    type = 'Adjusted OR',
    rec = 'Identified to have no benefit\nfrom restrictive fluids',
    outcomes = c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'),
    or = c(0.71, 0.78, 2.49),
    ll = c(0.35, 0.35, 1.25),
    ul = c(1.43, 1.73, 4.97)
);

d.or <- rbind(d.e_or_u_r, d.e_or_a_r, d.m_or_u_r, d.m_or_a_r, d.e_or_u_n, d.e_or_a_n, d.m_or_u_n, d.m_or_a_n);
d.or <- mutate(d.or,
    cohort = factor(cohort, levels=c('Development cohort\n(MIMIC-IV)', 'External validation cohort\n(eICU)')),
    type = factor(type, levels=c('Unadjusted OR', 'Adjusted OR')),
    rec = factor(rec, levels=c('Identified to benefit\nfrom restrictive fluids', 'Identified to have no benefit\nfrom restrictive fluids')),
    outcomes = factor(outcomes, levels=c('Early AKI reversal', 'Sustained AKI reversal', 'MAKE at Discharge'))
);

library(ggplot2);
library(ggsci);
g <- ggplot(d.or, aes(x=outcomes, y=or, colour=type, ymin=ll, ymax=ul)) + 
    geom_rect(d.or, mapping=aes(fill=cohort, colour='white'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.05) +
    geom_pointrange(position = position_dodge(width = 0.2), size=0.25) +
    geom_hline(yintercept = 1, linewidth=0.5, color='gray') + 
    facet_grid(vars(cohort), vars(rec)) + 
    scale_y_log10() +
    scale_colour_manual(
        name=NULL, 
        values=c('Unadjusted OR'='#BC3C29FF', 'Adjusted OR'='#0072B5FF'),
        breaks=c('Unadjusted OR', 'Adjusted OR')
        ) +
    scale_fill_manual(guide='none', values = c("green", "orange")) +
    xlab('Outcomes') +
    ylab('Odds ratio (95% CI)') +
    theme_bw(base_size=10) + 
    theme(legend.position='bottom') +
    theme(axis.text.x = element_text(angle = 45, hjust=1))
ggsave('fig/fig_4b.png', width=5, height=5, units='in', dpi=600)
ggsave('fig/fig_4b.pdf', width=5, height=5, units='in', dpi=600)









