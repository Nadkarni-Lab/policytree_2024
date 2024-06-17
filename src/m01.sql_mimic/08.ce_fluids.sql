
-- Fluid balance
DROP TABLE IF EXISTS r_policytree_mimic_2023.ce_fluids;
CREATE TABLE r_policytree_mimic_2023.ce_fluids AS
SELECT pt.subject_id,
       ce.starttime,
       ce.endtime,
       ce.rate_all,
       ce.rate_in,
       ce.rate_out,
       ce.rate_blood_product,
       ce.rate_colloid,
       ce.rate_crystalloid,
       ce.rate_dialysis_input,
       ce.rate_dialysis_output,
       ce.rate_drain,
       ce.rate_irrigant,
       ce.rate_oral,
       ce.rate_general_intake,
       ce.rate_general_output,
       ce.rate_nutrition,
       ce.rate_uo,
       ce.amount_all AS nettotal,
       ce.amount_blood_product,
       ce.amount_colloid,
       ce.amount_crystalloid,
       ce.amount_dialysis_input,
       ce.amount_dialysis_output,
       ce.amount_drain,
       ce.amount_irrigant,
       ce.amount_oral,
       ce.amount_general_intake,
       ce.amount_general_output,
       ce.amount_nutrition,
       ce.amount_uo
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN `physionet-data.mimiciv_icu.icustays` AS ie
                    ON pt.subject_id = ie.subject_id AND
                       pt.hadm_id = ie.hadm_id
         INNER JOIN aims_mimiciv_derived.fluid_balance AS ce
                    ON ie.stay_id = ce.stay_id AND
                       pt.disch_dt >= ce.starttime-- AND
                     --   ce.starttime >= pt.admit_dt
ORDER BY pt.subject_id, ce.starttime, ce.endtime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.ce_fluids;
-- 2418423, 6660









