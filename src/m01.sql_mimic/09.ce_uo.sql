
-- Urine output
DROP TABLE IF EXISTS r_policytree_mimic_2023.ce_uo;
CREATE TABLE r_policytree_mimic_2023.ce_uo AS
SELECT uo.subject_id,
       uo.charttime,
       uo.urineoutput
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN aims_mimiciv_derived.urine_output AS uo
                    ON pt.subject_id = uo.subject_id AND
                       pt.hadm_id = uo.hadm_id
WHERE pt.disch_dt >= uo.charttime
  -- AND uo.charttime >= pt.admit_dt
ORDER BY uo.subject_id, uo.charttime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS npid
FROM r_policytree_mimic_2023.ce_uo;
-- 810919, 6514








