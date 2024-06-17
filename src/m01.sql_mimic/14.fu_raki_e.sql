
-- Persistent AKI
-- -- ADQI 16 consensus.
-- -- Transient AKI is a rapid reversal of AKI and needs to start more than
--    48 hours of sustained reversal of AKI during the first 48 hours after
--    AKI onset.
-- -- https://pubmed.ncbi.nlm.nih.gov/28239173/
-- -- See also
--    https://pubmed.ncbi.nlm.nih.gov/34642418/
DROP TABLE IF EXISTS r_policytree_mimic_2023.fu_raki_e;
CREATE TABLE r_policytree_mimic_2023.fu_raki_e AS
WITH episode AS (
    SELECT pt.subject_id,
           h1.aki_stage_smoothed AS h1_aki_st,
           h1.charttime          AS h1_aki_dt,
           h2.aki_stage_smoothed AS h2_aki_st,
           h2.charttime          AS h2_aki_dt
    FROM r_policytree_mimic_2023.cohort AS pt
             INNER JOIN aims_mimiciv_derived.kdigo_stages AS h1
                        ON pt.subject_id = h1.subject_id AND
                           pt.hadm_id = h1.hadm_id AND
                           pt.disch_dt >= h1.charttime AND
                           pt.out_dt >= h1.charttime AND
                           DATETIME_ADD(pt.aki_dt, INTERVAL '48' HOUR) > h1.charttime AND
                           h1.charttime > pt.aki_dt
             INNER JOIN aims_mimiciv_derived.kdigo_stages AS h2
                        ON h1.subject_id = h2.subject_id AND
                           h1.hadm_id = h2.hadm_id AND
                           pt.out_dt >= h2.charttime AND
                           DATETIME_ADD(pt.aki_dt, INTERVAL '48' HOUR) > h2.charttime AND
                           h2.charttime >= h1.charttime
    WHERE h1.aki_stage_smoothed IS NOT NULL
      AND h2.aki_stage_smoothed IS NOT NULL
), recovery0 AS (
    -- Recovery means sustained reversal (AKI remission over 48 hours) of an AKI episode within 48 of its onset.
    -- We will define the recovery date as the last day of the first recovery episode.
    -- -- ToDo: Talk with Ankit and Kullaya about the confirmed recovery date.
    SELECT subject_id,
           h1_aki_dt                     AS start_dt,
           MAX(IF(h2_aki_st >= 1, 1, 0)) AS peak_aki
    FROM episode
    GROUP BY subject_id, h1_aki_dt
), recovery1 AS (
    SELECT subject_id,
           1             AS raki_st,
           MIN(start_dt) AS raki_dt
    FROM recovery0
    WHERE peak_aki = 0
    GROUP BY subject_id
), persistent0 AS (
    SELECT subject_id,
           -- Persistent AKI means anything other than recovery.
           -- We will define the persistent AKI date as the first day that sustained reversal is no longer plausible.
           0              AS raki_st,
           MAX(h1_aki_dt) AS raki_dt
    FROM episode
    WHERE subject_id NOT IN (SELECT subject_id FROM recovery1)
    GROUP BY subject_id
), taki AS (
    SELECT *
    FROM recovery1
    UNION ALL
    SELECT *
    FROM persistent0
)
SELECT *
FROM taki
ORDER BY subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_mimic_2023.fu_raki_e;
-- 6660, 6660









