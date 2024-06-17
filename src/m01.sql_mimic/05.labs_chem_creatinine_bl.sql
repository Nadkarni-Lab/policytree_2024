
-- Baseline creatinine
DROP TABLE IF EXISTS r_policytree_mimic_2023.labs_chem_creatinine_bl;
CREATE TABLE r_policytree_mimic_2023.labs_chem_creatinine_bl AS
SELECT pt.subject_id AS subject_id,
       le.cr_bl      AS chem_creatinine_bl
FROM r_policytree_mimic_2023.cohort AS pt
         LEFT JOIN aims_mimiciv_derived.creatinine_baseline AS le ON
            pt.subject_id = le.subject_id AND
            pt.hadm_id = le.hadm_id
ORDER BY subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.labs_chem_creatinine_bl;
-- 6660, 6660









