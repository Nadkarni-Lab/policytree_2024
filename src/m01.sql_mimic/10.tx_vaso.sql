
-- Vasopressor treatment
DROP TABLE IF EXISTS r_policytree_mimic_2023.tx_vaso;
CREATE TABLE r_policytree_mimic_2023.tx_vaso AS
SELECT pt.subject_id,
       starttime,
       endtime,
       norepinephrine_equivalent_dose AS tx_nee
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN `physionet-data.mimiciv_icu.icustays` AS ie
                    ON pt.subject_id = ie.subject_id AND
                       pt.hadm_id = ie.hadm_id
         INNER JOIN `physionet-data.mimiciv_derived.norepinephrine_equivalent_dose` AS tx
                    ON ie.stay_id = tx.stay_id
WHERE pt.disch_dt >= tx.starttime
  -- AND tx.starttime >= pt.admit_dt
ORDER BY pt.subject_id, tx.starttime, tx.endtime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS npid
FROM r_policytree_mimic_2023.tx_vaso;
-- 186172, 4242









