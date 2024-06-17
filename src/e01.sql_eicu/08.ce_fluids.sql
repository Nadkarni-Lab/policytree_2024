
-- Fluid balance
DROP TABLE IF EXISTS r_policytree_eicu_2023.ce_fluids;
CREATE TABLE r_policytree_eicu_2023.ce_fluids AS
SELECT pt.subject_id,
       ce.starttime,
       ce.intaketotal,
       ce.outputtotal,
       ce.nettotal,
       ce.amount_colloid,
       ce.amount_crystalloid
FROM r_policytree_eicu_2023.cohort AS pt
         INNER JOIN aims_eicu_crd_derived.fluid_balance AS ce
                    ON pt.subject_id = ce.subject_id AND
                       pt.hadm_id = ce.hadm_id AND
                       pt.disch_dt >= ce.starttime-- AND
                     --   ce.starttime >= pt.admit_dt
ORDER BY pt.subject_id, ce.starttime;









