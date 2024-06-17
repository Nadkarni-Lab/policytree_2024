
-- Demographics
DROP TABLE IF EXISTS r_policytree_mimic_2023.demo;
CREATE TABLE r_policytree_mimic_2023.demo AS
SELECT pt.subject_id AS subject_id,
       p1.age        AS dm_age,
       p2.race       AS dm_race,
       p3.gender     AS dm_gender
FROM r_policytree_mimic_2023.cohort AS pt
         LEFT JOIN `physionet-data.mimiciv_derived.age` AS p1
                   ON pt.hadm_id = p1.hadm_id
         LEFT JOIN `physionet-data.mimiciv_hosp.admissions` AS p2
                   ON pt.hadm_id = p2.hadm_id
         LEFT JOIN `physionet-data.mimiciv_hosp.patients` AS p3
                   ON pt.subject_id = p3.subject_id
GROUP BY pt.subject_id, p1.age, p2.race, p3.gender
ORDER BY subject_id;









