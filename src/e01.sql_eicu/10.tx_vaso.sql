
-- Vasopressor treatment
DROP TABLE IF EXISTS r_policytree_eicu_2023.tx_vaso;
CREATE TABLE r_policytree_eicu_2023.tx_vaso AS
SELECT pt.subject_id,
       starttime,
       endtime,
       norepinephrine_equivalent_dose AS tx_nee
FROM r_policytree_eicu_2023.cohort AS pt
         INNER JOIN aims_eicu_crd_derived.norepinephrine_equivalent_dose AS tx
                    ON pt.subject_id = tx.subject_id AND
                       pt.hadm_id = tx.hadm_id
WHERE pt.disch_dt >= tx.starttime
  -- AND tx.starttime >= pt.admit_dt
ORDER BY pt.subject_id, tx.starttime, tx.endtime;









