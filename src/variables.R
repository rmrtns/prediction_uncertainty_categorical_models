top_variables <- c(
  'Num_vars', 
  'Age',
  'ALBSE',      # albumin
  'LDSE',       # LD
  'LYMF',       # lymphocytes
  'ALTSE',      # ALT
  'CRPSE',      # CRP
  'LPSSE',      # lipase 
  'BASO', # basophils
  'HB',  # hemoglobin
  'Time_of_presentation',
  'SexM', # male sex
  'TRC', # thrombocytes
  'GGTSE', # gamma-GT
  'GLUPL', # glucose in plasma
  'KALSE', # potassium
  'MCV', # MCV
  'NATSE', # sodium
  'MONO',  # monocytes
  'PCOAB', # PCO2
  'X1m.mortality'
)

selected_variables <- c(
  'Num_vars', 
  'Age',
  'ALBSE',      # albumin
  'LDSE',       # LD
  'LYMF',       # lymphocytes
  'ALTSE',      # ALT
  'CRPSE',      # CRP
  'LPSSE',      # lipase 
  'HB',  # hemoglobin
  'Time_of_presentation',
  'SexM', # male sex
  'TRC', # thrombocyte
  'GGTSE', # gamma-GT
  'GLUPL', # glucose in plasma
  'X1m.mortality'
)

analytical_cv_percent <- round(c(
  'Num_vars' = NA,
  'Age' = NA,
  'ALBSE' = 2.435, # long-term IQC  
  'LDSE' = 1.275, # long-term IQC
  'LYMF' = 2.7, # long-term IQC
  'ALTSE' = 4.695, # long-term IQC
  'CRPSE' = 3.66, # long-term IQC
  'LPSSE' = 3.33, # long-term IQC
  'HB' = 0.55, # long-term IQC
  'Time_of_presentation' = NA,
  'SexM' = NA, 
  'TRC' = 1.5, # long-term IQC
  'GGTSE' = 1.955, # long-term IQC
  'GLUPL' = 1.31, # long-term IQC
  'X1m.mortality' = NA
), 2)

biological_cv_percent <- round(c(
  'Num_vars' = NA,
  'Age' = NA,
  'ALBSE' = 2.5, # within-subject variation EFLM database 
  'LDSE' = 4.4, # within-subject variation EFLM database
  'LYMF' = 10.8, # within-subject variation EFLM database
  'ALTSE' = 11.4, # within-subject variation EFLM database
  'CRPSE' = 33.7, # within-subject variation EFLM database
  'LPSSE' = 7.6, # within-subject variation EFLM database
  'HB' = 2.7, # within-subject variation EFLM database
  'Time_of_presentation' = NA,
  'SexM' = NA, 
  'TRC' = 7.3, # within-subject variation EFLM database
  'GGTSE' = 8.3, # within-subject variation EFLM database
  'GLUPL' = 4.6, # within-subject variation EFLM database
  'X1m.mortality' = NA
), 1)

uncertain_variables_indicator <- c(
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  FALSE
)

selected_variables_egfr <- c(
  'age' = 'Age',
  'sex' = 'SexM',
  'creatinine' = 'KRESE',
  'egfr_ckdepi' = 'CKDESE'
)

analytical_cv_percent_egfr <- round(c(
  'Age' = NA,
  'SexM' = NA,
  'KRESE' = 0.864, # long-term IQC 
  'CKDESE' = NA
), 2)

biological_cv_percent_egfr <- round(c(
  'Age' = NA,
  'SexM' = NA,
  'KRESE' = 4.4, # within-subject variation EFLM database,
  'CKDESE' = NA
), 1)

uncertain_variables_indicator_egfr <- c(
  FALSE,
  FALSE,
  TRUE,
  FALSE
)