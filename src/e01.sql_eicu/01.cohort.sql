
-- Cohort
-- Inclusion
--   Adult critically ill SA-AKI patient                                                  12777
-- Exclusion
-- 1. Patients aged less than 18 years old                                       -    0 = 12777
-- 2. History of end-stage renal disease (ESRD)                                  -  449 = 12328
-- 3. History of kidney transplant                                               -   82 = 12246
-- 4. Missing SCr between ICU admission and 24 hours after AKI onset             -   41 = 12205
-- 5. Missing vital signs between ICU admission and 24 hours after AKI onset     -    1 = 12204
-- 6. Any RRT between ICU admission and 24 hours after AKI onset                 - 1168 = 11036
-- 7. ICU discharge within 24 hours after AKI onset                              - 2697 =  8339
-- 8. Death within 48 hours after AKI onset                                      -  265 =  8074
-- 9. More than 2 times of ICU admission during the same hospitalization period  -  635 =  7230

-- ====================================================================================
-- Cohort selection
-- ====================================================================================

-- ------------------------------------------------------------------------------------
-- Inclusion
-- ------------------------------------------------------------------------------------

DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_i;
CREATE TABLE r_policytree_eicu_2023.cohort_i AS
WITH icustays AS (
    SELECT subject_id, hospital_id AS hosp_id, hadm_id, stay_id, 
           intime  AS in_dt,
           outtime AS out_dt,
           ROW_NUMBER() OVER (
               PARTITION BY subject_id, hadm_id
               ORDER BY intime
               )   AS icu_no
    FROM aims_eicu_crd_icu.icustays
    ORDER BY subject_id, hadm_id, intime
), firsticu AS (
    SELECT ie.* EXCEPT (icu_no),
           ad.admittime AS admit_dt,
           ad.dischyear AS disch_yr,
           -- To handle minor discrepancies between ICU and hospital discharge time. 
           LEAST(ad.dischtime, GREATEST(ie.out_dt, ad.dischtime + (60 * 6))) AS disch_dt
    FROM icustays AS ie
             INNER JOIN aims_eicu_crd_hosp.admissions AS ad
                        ON ie.subject_id = ad.subject_id AND
                           ie.hadm_id = ad.hadm_id AND
                           ie.in_dt >= admittime
    WHERE ie.icu_no = 1
      AND ad.dischtime >= ad.admittime
      AND ie.out_dt >= ie.in_dt
      AND ie.in_dt >= ad.admittime
      -- To handle minor discrepancies between ICU and hospital discharge time. 
      AND (ad.dischtime + (60 * 6)) >= ie.out_dt
), hx_preicu AS (
    SELECT ie.subject_id, ie.hadm_id, ie.stay_id,
           MAX(COALESCE(hx.aki_stage, 0)) AS hx_aki_st
    FROM firsticu AS ie
             LEFT JOIN aims_eicu_crd_derived.kdigo_stages AS hx
                       ON ie.subject_id = hx.subject_id AND
                          ie.hadm_id = hx.hadm_id AND
                          ie.in_dt > hx.charttime
    GROUP BY ie.subject_id, ie.hadm_id, ie.stay_id
), tx0 AS (
    SELECT subject_id, hadm_id, stay_id,
           starttime,
           1     AS tx_diuretic_st,
           ROW_NUMBER() OVER (
               PARTITION BY subject_id, hadm_id, stay_id
               ORDER BY starttime
               ) AS tx_no
    FROM aims_eicu_crd_derived.diuretic
    GROUP BY subject_id, hadm_id, stay_id, starttime
    ORDER BY subject_id, hadm_id, stay_id, starttime
), tx AS (
    SELECT * EXCEPT (tx_no)
    FROM tx0
    WHERE tx_no = 1
), sa_aki AS (
    SELECT ie.*,
           h1.suspected_infection_time AS si_dt,
           h1.sofa_time                AS sofa_dt,
           h1.sepsis_time              AS sepsis_dt,
           h2.charttime                AS aki_dt,
           tx.starttime                AS diuretic_dt,
           ROW_NUMBER() OVER (
               PARTITION BY ie.subject_id
               ORDER BY h1.sepsis_time, h2.charttime
               )                       AS obs_no
    FROM firsticu AS ie
             INNER JOIN aims_eicu_crd_derived.sepsis3 AS h1
                        ON ie.subject_id = h1.subject_id AND
                           ie.stay_id = h1.stay_id AND
                           (ie.in_dt + 60*24) >= h1.sepsis_time
             INNER JOIN aims_eicu_crd_derived.kdigo_stages AS h2
                        ON ie.subject_id = h2.subject_id AND
                           ie.hadm_id = h2.hadm_id AND
                           ie.out_dt >= h2.charttime AND
                           (ie.in_dt + 60*48) >= h2.charttime AND
                           h2.charttime >= ie.in_dt
             LEFT JOIN hx_preicu AS h3
                       ON ie.subject_id = h3.subject_id AND
                          ie.hadm_id = h3.hadm_id AND
                          ie.stay_id = h3.stay_id
             LEFT JOIN tx
                       ON ie.subject_id = tx.subject_id AND
                          ie.hadm_id = tx.hadm_id AND
                          ie.stay_id = tx.stay_id
    WHERE (h2.aki_stage IS NOT NULL AND h2.aki_stage >= 1)
      AND (h3.hx_aki_st IS NULL OR h3.hx_aki_st = 0)
    ORDER BY h2.subject_id, h1.sepsis_time, h2.charttime
)
SELECT subject_id,
       hosp_id,
       hadm_id,
       stay_id,
       admit_dt,
       disch_yr,
       disch_dt,
       in_dt,
       out_dt,
       si_dt,
       sofa_dt,
       sepsis_dt,
       aki_dt,
       diuretic_dt
FROM sa_aki
WHERE obs_no = 1
ORDER BY subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_i;
-- 9614, 9614



-- ------------------------------------------------------------------------------------
-- Exclusion
-- ------------------------------------------------------------------------------------

-- 1. Patients aged less than 18 years old.
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e01;
CREATE TABLE r_policytree_eicu_2023.cohort_e01 AS
SELECT pt.*
FROM r_policytree_eicu_2023.cohort_i AS pt
         INNER JOIN aims_eicu_crd_derived.age AS dm
                    ON pt.subject_id = dm.subject_id AND
                       pt.hadm_id = dm.hadm_id
WHERE dm.age >= 18
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e01;
-- 9614, 9614


-- 2. History of end-stage renal disease (ESRD) or kidney transplant
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e02;
CREATE TABLE r_policytree_eicu_2023.cohort_e02 AS
WITH hx AS (
    SELECT pt.subject_id
    FROM r_policytree_eicu_2023.cohort_e01 AS pt
             LEFT JOIN aims_eicu_crd_derived.esrd AS hx
                       ON pt.subject_id = hx.subject_id AND
                          pt.hadm_id = hx.hadm_id
    WHERE hx.esrd IS NULL
       OR hx.esrd = 0
    GROUP BY pt.subject_id
)
SELECT pt.*
FROM r_policytree_eicu_2023.cohort_e01 AS pt
         INNER JOIN hx ON
            pt.subject_id = hx.subject_id
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e02;
-- 9612, 9612


-- 3. Any RRT between ICU admission and AKI onset.
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e03;
CREATE TABLE r_policytree_eicu_2023.cohort_e03 AS
WITH rrt AS (
    SELECT pt.subject_id,
           MAX(IF(tx.charttime IS NOT NULL, 1, 0)) AS tx_rrt,
    FROM r_policytree_eicu_2023.cohort_e02 AS pt
             LEFT JOIN aims_eicu_crd_derived.rrt AS tx
                       ON pt.stay_id = tx.patientunitstayid AND
                          pt.aki_dt > charttime
    GROUP BY subject_id
)
SELECT pt.*
FROM r_policytree_eicu_2023.cohort_e02 AS pt
         LEFT JOIN rrt AS tx
                   ON pt.subject_id = tx.subject_id
WHERE tx.tx_rrt IS NULL
   OR tx.tx_rrt = 0
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e03;
-- 9606, 9606


-- 4. Missing SCr between 48 hours after AKI onset
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e04;
CREATE TABLE r_policytree_eicu_2023.cohort_e04 AS
WITH le AS (
    SELECT pt.subject_id,
           pt.hadm_id,
           MAX(le.creatinine) AS cr,
    FROM r_policytree_eicu_2023.cohort_e03 AS pt
             INNER JOIN aims_eicu_crd_derived.chemistry AS le
                        ON pt.subject_id = le.subject_id AND
                           pt.hadm_id = le.hadm_id AND
                           pt.disch_dt >= le.charttime AND
                           (pt.aki_dt + 60*48) >= le.charttime AND
                           le.charttime > pt.aki_dt
    WHERE le.creatinine IS NOT NULL
    GROUP BY pt.subject_id, pt.hadm_id
)
SELECT pt.*
FROM r_policytree_eicu_2023.cohort_e03 AS pt
         LEFT JOIN le ON
            pt.subject_id = le.subject_id AND
            pt.hadm_id = le.hadm_id
WHERE le.cr IS NOT NULL
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e04;
-- 9075, 9075


-- 5. ICU discharge within 48 hours after baseline
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e05;
CREATE TABLE r_policytree_eicu_2023.cohort_e05 AS
SELECT *
FROM r_policytree_eicu_2023.cohort_e04
WHERE out_dt >= (aki_dt + 60*48)
ORDER BY subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e05;
-- 5484, 5484


-- 6. Death within 48 hours after AKI onset
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e06;
CREATE TABLE r_policytree_eicu_2023.cohort_e06 AS
SELECT pt.*
FROM r_policytree_eicu_2023.cohort_e05 AS pt
         LEFT JOIN aims_eicu_crd_hosp.admissions AS ad
                   ON pt.subject_id = ad.subject_id AND
                      pt.hadm_id = ad.hadm_id
WHERE ad.deathtime IS NULL
   OR ad.deathtime >= (pt.aki_dt + 60*48)
ORDER BY pt.subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort_e06;
-- 5483, 5483





-- ------------------------------------------------------------------------------------
-- Final cohort
-- ------------------------------------------------------------------------------------

DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort;
CREATE TABLE r_policytree_eicu_2023.cohort AS
SELECT subject_id,
       hosp_id,
       hadm_id,
       stay_id,
       admit_dt,
       disch_yr,
       disch_dt,
       in_dt,
       out_dt,
       sofa_dt,
       si_dt,
       sepsis_dt,
       aki_dt,
       diuretic_dt,
       aki_dt AS baseline_dt
FROM r_policytree_eicu_2023.cohort_e06
ORDER BY subject_id;

SELECT COUNT(*)                   AS nobs,
       COUNT(DISTINCT subject_id) AS nsid
FROM r_policytree_eicu_2023.cohort;
-- 5483, 5483

DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_i;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e01;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e02;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e03;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e04;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e05;
DROP TABLE IF EXISTS r_policytree_eicu_2023.cohort_e06;









