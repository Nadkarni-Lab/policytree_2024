
-- Nephrotoxic medications
DROP TABLE IF EXISTS r_policytree_eicu_2023.tx_nephrotoxins;
CREATE TABLE r_policytree_eicu_2023.tx_nephrotoxins AS
SELECT tx.subject_id,
       tx.charttime,
       tx.drugname,
       tx.drughiclseqno,
       tx.routeadmin
FROM r_policytree_eicu_2023.cohort AS pt
         INNER JOIN aims_eicu_crd_derived.nephrotoxin AS tx
                    ON pt.subject_id = tx.subject_id AND
                       pt.hadm_id = tx.hadm_id
WHERE pt.disch_dt >= tx.charttime
  -- AND tx.charttime >= pt.admit_dt
ORDER BY tx.subject_id, tx.charttime;









