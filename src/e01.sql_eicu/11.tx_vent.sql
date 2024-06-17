
-- Ventilation
DROP TABLE IF EXISTS r_policytree_eicu_2023.tx_vent;
CREATE TABLE r_policytree_eicu_2023.tx_vent AS
SELECT pt.subject_id,
       tx.starttime,
       tx.endtime,
       1             AS tx_vent
FROM r_policytree_eicu_2023.cohort AS pt
         INNER JOIN aims_eicu_crd_derived.ventilator AS tx
                    ON pt.subject_id = tx.subject_id AND 
                       pt.hadm_id = tx.hadm_id
WHERE pt.disch_dt >= tx.starttime
  -- AND tx.starttime >= pt.admit_dt
  AND tx.mechvent = 1
ORDER BY pt.subject_id, tx.starttime, tx.endtime;









