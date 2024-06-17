
-- Labs
DROP TABLE IF EXISTS r_policytree_eicu_2023.labs;
CREATE TABLE r_policytree_eicu_2023.labs AS
WITH cbc0 AS (
    SELECT ce.subject_id,
           ce.charttime,
           ce.hematocrit AS cbc_hematocrit,
           ce.hemoglobin AS cbc_hemoglobin,
           ce.mch        AS cbc_mch,
           ce.mchc       AS cbc_mchc,
           ce.mcv        AS cbc_mcv,
           ce.platelet   AS cbc_platelet,
           ce.rbc        AS cbc_rbc,
           ce.rdw        AS cbc_rdw,
           ce.rdwsd      AS cbc_rdwsd,
           ce.wbc        AS cbc_wbc
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.complete_blood_count AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), cbc1 AS (
    SELECT *
    FROM cbc0
    WHERE cbc_hematocrit IS NOT NULL
       OR cbc_hemoglobin IS NOT NULL
       OR cbc_mch IS NOT NULL
       OR cbc_mchc IS NOT NULL
       OR cbc_mcv IS NOT NULL
       OR cbc_platelet IS NOT NULL
       OR cbc_rbc IS NOT NULL
       OR cbc_rdw IS NOT NULL
       OR cbc_rdwsd IS NOT NULL
       OR cbc_wbc IS NOT NULL
), chem0 AS (
    SELECT ce.subject_id,
           ce.charttime,
           ce.albumin       AS chem_albumin,
           ce.globulin      AS chem_globulin,
           ce.total_protein AS chem_total_protein,
           ce.aniongap      AS chem_aniongap,
           ce.bicarbonate   AS chem_bicarbonate,
           ce.bun           AS chem_bun,
           ce.calcium       AS chem_calcium,
           ce.chloride      AS chem_chloride,
           ce.creatinine    AS chem_creatinine,
           ce.glucose       AS chem_glucose,
           ce.sodium        AS chem_sodium,
           ce.potassium     AS chem_potassium
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.chemistry AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), chem1 AS (
    SELECT *
    FROM chem0
    WHERE chem_albumin IS NOT NULL
       OR chem_globulin IS NOT NULL
       OR chem_total_protein IS NOT NULL
       OR chem_aniongap IS NOT NULL
       OR chem_bicarbonate IS NOT NULL
       OR chem_bun IS NOT NULL
       OR chem_calcium IS NOT NULL
       OR chem_chloride IS NOT NULL
       OR chem_creatinine IS NOT NULL
       OR chem_glucose IS NOT NULL
       OR chem_sodium IS NOT NULL
       OR chem_potassium IS NOT NULL
), diff0 AS (
    SELECT ce.subject_id,
           ce.charttime,
         --   ce.wbc                   AS diff_wbc,
         --   ce.basophils_abs         AS diff_basophils_abs,
         --   ce.eosinophils_abs       AS diff_eosinophils_abs,
         --   ce.lymphocytes_abs       AS diff_lymphocytes_abs,
         --   ce.monocytes_abs         AS diff_monocytes_abs,
         --   ce.neutrophils_abs       AS diff_neutrophils_abs,
           ce.basophils             AS diff_basophils,
           ce.eosinophils           AS diff_eosinophils,
           ce.lymphocytes           AS diff_lymphocytes,
           ce.monocytes             AS diff_monocytes,
           ce.neutrophils           AS diff_neutrophils,
         --   ce.atypical_lymphocytes  AS diff_atypical_lymphocytes,
           ce.bands                 AS diff_bands,
         --   ce.immature_granulocytes AS diff_immature_granulocytes,
         --   ce.metamyelocytes        AS diff_metamyelocytes,
         --   ce.nrbc                  AS diff_nrbc
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.blood_differential AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), diff1 AS (
    SELECT *
    FROM diff0
    WHERE -- diff_basophils_abs IS NOT NULL
      --  OR diff_eosinophils_abs IS NOT NULL
      --  OR diff_lymphocytes_abs IS NOT NULL
      --  OR diff_monocytes_abs IS NOT NULL
      --  OR diff_neutrophils_abs IS NOT NULL
       diff_basophils IS NOT NULL
       OR diff_eosinophils IS NOT NULL
       OR diff_lymphocytes IS NOT NULL
       OR diff_monocytes IS NOT NULL
       OR diff_neutrophils IS NOT NULL
      --  OR diff_atypical_lymphocytes IS NOT NULL
       OR diff_bands IS NOT NULL
      --  OR diff_immature_granulocytes IS NOT NULL
      --  OR diff_metamyelocytes IS NOT NULL
      --  OR diff_nrbc IS NOT NULL
      --  OR diff_wbc IS NOT NULL
), coag0 AS (
    SELECT ce.subject_id,
           ce.charttime,
           ce.d_dimer    AS coag_d_dimer,
           ce.fibrinogen AS coag_fibrinogen,
           ce.thrombin   AS coag_thrombin,
           ce.inr        AS coag_inr,
           ce.pt         AS coag_pt,
           ce.ptt        AS coag_ptt
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.coagulation AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), coag1 AS (
    SELECT *
    FROM coag0
    WHERE coag_d_dimer IS NOT NULL
       OR coag_fibrinogen IS NOT NULL
       OR coag_thrombin IS NOT NULL
       OR coag_inr IS NOT NULL
       OR coag_pt IS NOT NULL
       OR coag_ptt IS NOT NULL
), enz0 AS (
    SELECT ce.subject_id,
           ce.charttime,
           ce.alt                AS enz_alt,
           ce.alp                AS enz_alp,
           ce.ast                AS enz_ast,
           ce.amylase            AS enz_amylase,
           ce.bilirubin_total    AS enz_bilirubin_total,
           ce.bilirubin_direct   AS enz_bilirubin_direct,
           ce.bilirubin_indirect AS enz_bilirubin_indirect,
           ce.ck_cpk             AS enz_ck_cpk,
           ce.ck_mb              AS enz_ck_mb,
           ce.ggt                AS enz_ggt,
           ce.ld_ldh             AS enz_ld_ldh
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.enzyme AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), enz1 AS (
    SELECT *
    FROM enz0
    WHERE enz_alt IS NOT NULL
       OR enz_alp IS NOT NULL
       OR enz_ast IS NOT NULL
       OR enz_amylase IS NOT NULL
       OR enz_bilirubin_total IS NOT NULL
       OR enz_bilirubin_direct IS NOT NULL
       OR enz_bilirubin_indirect IS NOT NULL
       OR enz_ck_cpk IS NOT NULL
       OR enz_ck_mb IS NOT NULL
       OR enz_ggt IS NOT NULL
       OR enz_ld_ldh IS NOT NULL
), cm0 AS (
    SELECT ce.subject_id,
           ce.charttime,
         --   ce.ntprobnp AS cm_ntprobnp,
         --   CAST(ce.troponin_t AS FLOAT64) AS cm_troponin_t,
           ce.ck_mb    AS cm_ck_mb
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.cardiac_marker AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), cm1 AS (
    SELECT *
    FROM cm0
    WHERE -- cm_ntprobnp IS NOT NULL
      --  OR cm_troponin_t IS NOT NULL
      --  OR 
          cm_ck_mb IS NOT NULL
), bg0 AS (
    SELECT ce.subject_id,
           ce.charttime,
           ce.so2               AS bg_so2,
           ce.po2               AS bg_po2,
           ce.pco2              AS bg_pco2,
           ce.fio2_chartevents  AS bg_fio2_chartevents,
           ce.fio2              AS bg_fio2,
           ce.aado2             AS bg_aado2,
           ce.aado2_calc        AS bg_aado2_calc,
           ce.pao2fio2ratio     AS bg_pao2fio2ratio,
           ce.ph                AS bg_ph,
           ce.baseexcess        AS bg_baseexcess,
           ce.bicarbonate       AS bg_bicarbonate,
           ce.totalco2          AS bg_totalco2,
           ce.hematocrit        AS bg_hematocrit,
           ce.hemoglobin        AS bg_hemoglobin,
           ce.carboxyhemoglobin AS bg_carboxyhemoglobin,
           ce.methemoglobin     AS bg_methemoglobin,
           ce.chloride          AS bg_chloride,
           ce.calcium           AS bg_calcium,
           ce.temperature       AS bg_temperature,
           ce.potassium         AS bg_potassium,
           ce.sodium            AS bg_sodium,
           ce.lactate           AS bg_lactate,
           ce.glucose           AS bg_glucose
    FROM r_policytree_eicu_2023.cohort AS pt
             INNER JOIN aims_eicu_crd_derived.bg AS ce
                        ON pt.subject_id = ce.subject_id AND
                           pt.hadm_id = ce.hadm_id
    WHERE pt.disch_dt >= ce.charttime
      -- AND ce.charttime >= pt.admit_dt
), bg1 AS (
    SELECT *
    FROM bg0
    WHERE bg_so2 IS NOT NULL
       OR bg_po2 IS NOT NULL
       OR bg_pco2 IS NOT NULL
       OR bg_fio2_chartevents IS NOT NULL
       OR bg_fio2 IS NOT NULL
       OR bg_aado2 IS NOT NULL
       OR bg_aado2_calc IS NOT NULL
       OR bg_pao2fio2ratio IS NOT NULL
       OR bg_ph IS NOT NULL
       OR bg_baseexcess IS NOT NULL
       OR bg_bicarbonate IS NOT NULL
       OR bg_totalco2 IS NOT NULL
       OR bg_hematocrit IS NOT NULL
       OR bg_hemoglobin IS NOT NULL
       OR bg_carboxyhemoglobin IS NOT NULL
       OR bg_methemoglobin IS NOT NULL
       OR bg_chloride IS NOT NULL
       OR bg_calcium IS NOT NULL
       OR bg_temperature IS NOT NULL
       OR bg_potassium IS NOT NULL
       OR bg_sodium IS NOT NULL
       OR bg_lactate IS NOT NULL
       OR bg_glucose IS NOT NULL
)
-- , cvp0 AS (
--     SELECT ce.subject_id,
--            ce.charttime,
--            ce.valuenum AS cvp_cvp
--     FROM r_policytree_eicu_2023.cohort AS pt
--              INNER JOIN physionet-data.mimiciv_icu.chartevents AS ce
--                         ON pt.subject_id = ce.subject_id AND
--                            pt.hadm_id = ce.hadm_id
--     WHERE itemid = 220074
--       AND pt.disch_dt >= ce.charttime
--       AND ce.charttime >= pt.admit_dt
-- ), cvp1 AS (
--     SELECT *
--     FROM cvp0
--     WHERE cvp_cvp IS NOT NULL
-- )
   , pt AS (
    SELECT subject_id, charttime FROM cbc1
    UNION DISTINCT
    SELECT subject_id, charttime FROM chem1
    UNION DISTINCT
    SELECT subject_id, charttime FROM diff1
    UNION DISTINCT
    SELECT subject_id, charttime FROM coag1
    UNION DISTINCT
    SELECT subject_id, charttime FROM enz1
    UNION DISTINCT
    SELECT subject_id, charttime FROM cm1
    UNION DISTINCT
    SELECT subject_id, charttime FROM bg1
   --  UNION DISTINCT
   --  SELECT subject_id, charttime FROM cvp1
), le AS (
    SELECT pt.subject_id,
           pt.charttime,
           cbc1.cbc_hematocrit,
           cbc1.cbc_hemoglobin,
           cbc1.cbc_mch,
           cbc1.cbc_mchc,
           cbc1.cbc_mcv,
           cbc1.cbc_platelet,
           cbc1.cbc_rbc,
           cbc1.cbc_rdw,
           cbc1.cbc_rdwsd,
           cbc1.cbc_wbc,
           chem1.chem_albumin,
           chem1.chem_globulin,
           chem1.chem_total_protein,
           chem1.chem_aniongap,
           chem1.chem_bicarbonate,
           chem1.chem_bun,
           chem1.chem_calcium,
           chem1.chem_chloride,
           chem1.chem_creatinine,
           chem1.chem_glucose,
           chem1.chem_sodium,
           chem1.chem_potassium,
         --   diff1.diff_wbc,
         --   diff1.diff_basophils_abs,
         --   diff1.diff_eosinophils_abs,
         --   diff1.diff_lymphocytes_abs,
         --   diff1.diff_monocytes_abs,
         --   diff1.diff_neutrophils_abs,
           diff1.diff_basophils,
           diff1.diff_eosinophils,
           diff1.diff_lymphocytes,
           diff1.diff_monocytes,
           diff1.diff_neutrophils,
         --   diff1.diff_atypical_lymphocytes,
           diff1.diff_bands,
         --   diff1.diff_immature_granulocytes,
         --   diff1.diff_metamyelocytes,
         --   diff1.diff_nrbc,
           coag1.coag_d_dimer,
           coag1.coag_fibrinogen,
           coag1.coag_thrombin,
           coag1.coag_inr,
           coag1.coag_pt,
           coag1.coag_ptt,
           enz1.enz_alt,
           enz1.enz_alp,
           enz1.enz_ast,
           enz1.enz_amylase,
           enz1.enz_bilirubin_total,
           enz1.enz_bilirubin_direct,
           enz1.enz_bilirubin_indirect,
           enz1.enz_ck_cpk,
           enz1.enz_ck_mb,
           enz1.enz_ggt,
           enz1.enz_ld_ldh,
         --   cm1.cm_ntprobnp,
         --   cm1.cm_troponin_t,
           cm1.cm_ck_mb,
           bg1.bg_so2,
           bg1.bg_po2,
           bg1.bg_pco2,
           bg1.bg_fio2_chartevents,
           bg1.bg_fio2,
           bg1.bg_aado2,
           bg1.bg_aado2_calc,
           bg1.bg_pao2fio2ratio,
           bg1.bg_ph,
           bg1.bg_baseexcess,
           bg1.bg_bicarbonate,
           bg1.bg_totalco2,
           bg1.bg_hematocrit,
           bg1.bg_hemoglobin,
           bg1.bg_carboxyhemoglobin,
           bg1.bg_methemoglobin,
           bg1.bg_chloride,
           bg1.bg_calcium,
           bg1.bg_temperature,
           bg1.bg_potassium,
           bg1.bg_sodium,
           bg1.bg_lactate,
           bg1.bg_glucose,
--            cvp1.cvp_cvp
    FROM pt
             LEFT JOIN cbc1
                       ON pt.subject_id = cbc1.subject_id AND
                          pt.charttime = cbc1.charttime
             LEFT JOIN chem1
                       ON pt.subject_id = chem1.subject_id AND
                          pt.charttime = chem1.charttime
             LEFT JOIN diff1
                       ON pt.subject_id = diff1.subject_id AND
                          pt.charttime = diff1.charttime
             LEFT JOIN coag1
                       ON pt.subject_id = coag1.subject_id AND
                          pt.charttime = coag1.charttime
             LEFT JOIN enz1
                       ON pt.subject_id = enz1.subject_id AND
                          pt.charttime = enz1.charttime
             LEFT JOIN cm1
                       ON pt.subject_id = cm1.subject_id AND
                          pt.charttime = cm1.charttime
             LEFT JOIN bg1
                       ON pt.subject_id = bg1.subject_id AND
                          pt.charttime = bg1.charttime
            --  LEFT JOIN cvp1
            --            ON pt.subject_id = cvp1.subject_id AND
            --               pt.charttime = cvp1.charttime
)
SELECT *
FROM le
ORDER BY subject_id, charttime;









