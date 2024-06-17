
-- SOFA
DROP TABLE IF EXISTS r_policytree_eicu_2023.sc_sofa;
CREATE TABLE r_policytree_eicu_2023.sc_sofa AS
SELECT pt.subject_id,
       sc.starttime,
       sc.endtime,
       sc.sofa_24hours AS sc_sofa
FROM r_policytree_eicu_2023.cohort AS pt
         INNER JOIN aims_eicu_crd_derived.sofa AS sc
                    ON pt.subject_id = sc.subject_id AND
                       pt.hadm_id = sc.hadm_id
ORDER BY pt.subject_id, sc.starttime;









