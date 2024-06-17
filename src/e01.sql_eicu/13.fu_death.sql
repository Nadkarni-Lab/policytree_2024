
-- Inhospital mortality
DROP TABLE IF EXISTS r_policytree_eicu_2023.fu_death;
CREATE TABLE r_policytree_eicu_2023.fu_death AS
SELECT pt.subject_id,
       IF(ad.deathtime IS NOT NULL, 1, 0)                      AS death_st,
       IF(ad.deathtime IS NOT NULL, ad.deathtime, pt.disch_dt) AS death_dt
FROM r_policytree_eicu_2023.cohort AS pt
         LEFT JOIN aims_eicu_crd_hosp.admissions AS ad
                   ON pt.subject_id = ad.subject_id AND
                      pt.hadm_id = ad.hadm_id
ORDER BY pt.subject_id;









