
-- AKI
DROP TABLE IF EXISTS r_policytree_mimic_2023.sc_aki;
CREATE TABLE r_policytree_mimic_2023.sc_aki AS
SELECT pt.subject_id,
       hx.aki_stage_smoothed AS sc_aki_st,
       hx.aki_stage_creat_1d AS sc_aki_cr_st,
       hx.aki_stage_uo_1d    AS sc_aki_uo_st,
       hx.aki_stage_rrt_all  AS sc_aki_rr_st,
       hx.charttime          AS sc_aki_dt
FROM r_policytree_mimic_2023.cohort AS pt
         INNER JOIN aims_mimiciv_derived.kdigo_stages AS hx
                    ON pt.subject_id = hx.subject_id AND
                       pt.hadm_id = hx.hadm_id
WHERE pt.disch_dt >= hx.charttime
  -- AND hx.charttime >= pt.admit_dt
ORDER BY pt.subject_id, hx.charttime;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS npid
FROM r_policytree_mimic_2023.sc_aki;
-- 911332, 6660









