
-- Vital signs
DROP TABLE IF EXISTS r_policytree_mimic_2023.vitals;
CREATE TABLE r_policytree_mimic_2023.vitals AS
WITH vt0 AS (
    SELECT pt.subject_id,
           vt.charttime,
           CASE
               WHEN 5 < vt.heart_rate AND vt.heart_rate <= 220
                   THEN vt.heart_rate
               END           vs_heart_rate,
           CASE
               WHEN 12 < vt.sbp AND vt.sbp <= 293
                   THEN vt.sbp
               END           vs_sbp,
           CASE
               WHEN 6 < vt.dbp AND vt.dbp <= 237
                   THEN vt.dbp
               END           vs_dbp,
           CASE
               WHEN 6 < vt.mbp AND vt.mbp <= 285
                   THEN vt.mbp
               END           vs_mbp,
           CASE
               WHEN 12 < vt.sbp_ni AND vt.sbp_ni <= 293
                   THEN vt.sbp_ni
               END           vs_sbp_ni,
           CASE
               WHEN 6 < vt.dbp_ni AND vt.dbp_ni <= 237
                   THEN vt.dbp_ni
               END           vs_dbp_ni,
           CASE
               WHEN 6 < vt.mbp_ni AND vt.mbp_ni <= 285
                   THEN vt.mbp_ni
               END           vs_mbp_ni,
           CASE
               WHEN 5 < vt.resp_rate AND vt.resp_rate <= 47
                   THEN vt.resp_rate
               END           vs_resp_rate,
           CASE
               WHEN 26.7 < vt.temperature AND vt.temperature <= 42.3
                   THEN vt.temperature
               END           vs_temperature,
           CASE
               WHEN 33 < vt.spo2
                   THEN vt.spo2
               END           vs_spo2,
           CASE
               WHEN 33 < vt.glucose AND vt.glucose <= 1293
                   THEN vt.glucose
               END           vs_glucose
    FROM r_policytree_mimic_2023.cohort AS pt
             INNER JOIN `physionet-data.mimiciv_icu.icustays` AS ie
                        ON pt.subject_id = ie.subject_id AND
                           pt.hadm_id = ie.hadm_id
             INNER JOIN `physionet-data.mimiciv_derived.vitalsign` AS vt
                        ON ie.subject_id = vt.subject_id AND
                           ie.stay_id = vt.stay_id
    WHERE pt.disch_dt >= vt.charttime
    --   AND vt.charttime >= pt.admit_dt
), vt1 AS (
    SELECT *
    FROM vt0
    WHERE vs_heart_rate IS NOT NULL
       OR vs_sbp IS NOT NULL
       OR vs_dbp IS NOT NULL
       OR vs_mbp IS NOT NULL
       OR vs_sbp_ni IS NOT NULL
       OR vs_dbp_ni IS NOT NULL
       OR vs_mbp_ni IS NOT NULL
       OR vs_resp_rate IS NOT NULL
       OR vs_temperature IS NOT NULL
       OR vs_spo2 IS NOT NULL
       OR vs_glucose IS NOT NULL
), wt0 AS (
    SELECT COALESCE(wt1.stay_id, wt2.stay_id)     AS stay_id,
           COALESCE(wt1.starttime, wt2.starttime) AS charttime,
           CASE
               WHEN 300 > wt1.weight AND
                    wt1.weight > 20
                   THEN wt1.weight
               WHEN 300 > wt2.patientweight AND
                    wt2.patientweight > 20
                   THEN wt2.patientweight
               ELSE NULL
               END                                AS weight
    FROM `physionet-data.mimiciv_derived.weight_durations` AS wt1
             FULL JOIN `physionet-data.mimiciv_icu.inputevents` AS wt2
                       ON wt1.stay_id = wt2.stay_id AND
                          wt1.starttime = wt2.starttime
), wt1 AS (
    SELECT pt.subject_id,
           wt.charttime,
           wt.weight AS vs_weight
    FROM r_policytree_mimic_2023.cohort AS pt
             INNER JOIN `physionet-data.mimiciv_icu.icustays` AS ie
                        ON pt.subject_id = ie.subject_id AND
                           pt.hadm_id = ie.hadm_id
             INNER JOIN wt0 AS wt
                        ON ie.stay_id = wt.stay_id
    WHERE pt.disch_dt >= wt.charttime
    --   AND wt.charttime >= pt.admit_dt
), wt2 AS (
    SELECT *
    FROM wt1
    WHERE vs_weight IS NOT NULL
), pt AS (
    SELECT subject_id, charttime FROM vt1
    UNION DISTINCT
    SELECT subject_id, charttime FROM wt2
), vt AS (
    SELECT pt.subject_id,
           pt.charttime,
           vt1.vs_heart_rate,
           vt1.vs_sbp,
           vt1.vs_dbp,
           vt1.vs_mbp,
           vt1.vs_sbp_ni,
           vt1.vs_dbp_ni,
           vt1.vs_mbp_ni,
           vt1.vs_resp_rate,
           vt1.vs_temperature,
           vt1.vs_spo2,
           vt1.vs_glucose,
           wt2.vs_weight
    FROM pt
             LEFT JOIN vt1
                       ON pt.subject_id = vt1.subject_id AND
                          pt.charttime = vt1.charttime
             LEFT JOIN wt2
                       ON pt.subject_id = wt2.subject_id AND
                          pt.charttime = wt2.charttime
)
SELECT *
FROM vt
ORDER BY subject_id, charttime;









