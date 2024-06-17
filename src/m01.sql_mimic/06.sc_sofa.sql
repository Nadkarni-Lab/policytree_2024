
-- SOFA
DROP TABLE IF EXISTS r_policytree_mimic_2023.sc_sofa;
CREATE TABLE r_policytree_mimic_2023.sc_sofa AS
SELECT pt.subject_id,
       sc.starttime,
       sc.endtime,
       sc.sofa_24hours AS sc_sofa
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN `physionet-data.mimiciv_icu.icustays` AS ie
                    ON pt.subject_id = ie.subject_id AND
                       pt.hadm_id = ie.hadm_id
         INNER JOIN `physionet-data.mimiciv_derived.sofa` AS sc
                    ON ie.stay_id = sc.stay_id
ORDER BY pt.subject_id, sc.starttime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.sc_sofa;
-- 1450723, 6660









