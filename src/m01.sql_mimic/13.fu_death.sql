
-- Inhospital mortality
DROP TABLE IF EXISTS r_policytree_mimic_2023.fu_death;
CREATE TABLE r_policytree_mimic_2023.fu_death AS
SELECT pt.subject_id,
       IF(ad.deathtime IS NOT NULL, 1, 0)                      AS death_st,
       IF(ad.deathtime IS NOT NULL, ad.deathtime, pt.disch_dt) AS death_dt
FROM r_policytree_mimic_2023.cohort AS pt
         LEFT JOIN `physionet-data.mimiciv_hosp.admissions` AS ad
                   ON pt.subject_id = ad.subject_id AND
                      pt.hadm_id = ad.hadm_id
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.fu_death;
-- 6660, 6660









