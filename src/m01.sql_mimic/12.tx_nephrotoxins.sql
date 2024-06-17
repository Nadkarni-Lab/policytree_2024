
-- Nephrotoxic medications
DROP TABLE IF EXISTS r_policytree_mimic_2023.tx_nephrotoxins;
CREATE TABLE r_policytree_mimic_2023.tx_nephrotoxins AS
SELECT tx.subject_id,
       tx.charttime,
       tx.medication,
       tx.event_txt,
       tx.route,
       tx.drug_type
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN aims_mimiciv_derived.nephrotoxin AS tx
                    ON pt.subject_id = tx.subject_id AND
                       pt.hadm_id = tx.hadm_id
WHERE pt.disch_dt >= tx.charttime
  -- AND tx.charttime >= pt.admit_dt
ORDER BY tx.subject_id, tx.charttime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS npid
FROM r_policytree_mimic_2023.tx_nephrotoxins;
-- 35539, 2163









