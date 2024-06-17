
-- Ventilation
DROP TABLE IF EXISTS r_policytree_mimic_2023.tx_vent;
CREATE TABLE r_policytree_mimic_2023.tx_vent AS
SELECT pt.subject_id AS subject_id,
       tx.starttime,
       tx.endtime,
       1             AS tx_vent
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN physionet-data.mimiciv_icu.icustays AS ie
                    ON pt.subject_id = ie.subject_id AND
                       pt.hadm_id = ie.hadm_id
         INNER JOIN physionet-data.mimiciv_derived.ventilation AS tx
                    ON ie.stay_id = tx.stay_id
WHERE pt.disch_dt >= tx.starttime
  AND tx.starttime >= pt.admit_dt
  AND ventilation_status = 'InvasiveVent'
ORDER BY pt.subject_id, tx.starttime, tx.endtime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.tx_vent;
-- 7721, 5080









