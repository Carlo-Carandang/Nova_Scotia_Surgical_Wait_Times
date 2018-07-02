library(readr)
wait_times <- read_csv(file = '../data_analysis/data/Surgical_Wait_Times.csv',
                       col_names = TRUE, col_types = cols())
