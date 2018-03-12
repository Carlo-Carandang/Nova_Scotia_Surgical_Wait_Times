## ---- dataset ----

library(readr)

wait_times <- read_tsv(file = '../data_analysis/data/Surgical_Wait_Times_2017-09-04.csv',
                       col_names = TRUE, col_types = cols())
