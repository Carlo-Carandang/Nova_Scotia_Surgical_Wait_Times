## ---- wrangling ----

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(stringr)
library(tibble)
library(e1071)
library(magrittr)

## ---- column-names ----

wait_times %>% names()

## ---- missing-values-check ----

as.data.frame(tibble(feature = wait_times %>% names(),
       missing_count = c(sum(is.na(wait_times$Period)),
                         sum(is.na(wait_times$Specialty)),
                         sum(is.na(wait_times$Procedure)), 
                         sum(is.na(wait_times$Provider)), 
                         sum(is.na(wait_times$Zone)), 
                         sum(is.na(wait_times$Facility)),
                         sum(is.na(wait_times$Year)),
                         sum(is.na(wait_times$Quarter)),
                         sum(is.na(wait_times$Consult_Median)),
                         sum(is.na(wait_times$Consult_90th)),
                         sum(is.na(wait_times$Surgery_Median)),
                         sum(is.na(wait_times$Surgery_90th))),
       non_missing_count = c(sum(!is.na(wait_times$Period)),
                         sum(!is.na(wait_times$Specialty)),
                         sum(!is.na(wait_times$Procedure)), 
                         sum(!is.na(wait_times$Provider)), 
                         sum(!is.na(wait_times$Zone)), 
                         sum(!is.na(wait_times$Facility)),
                         sum(!is.na(wait_times$Year)),
                         sum(!is.na(wait_times$Quarter)),
                         sum(!is.na(wait_times$Consult_Median)),
                         sum(!is.na(wait_times$Consult_90th)),
                         sum(!is.na(wait_times$Surgery_Median)),
                         sum(!is.na(wait_times$Surgery_90th)))))

if (!sum(wait_times %>% complete.cases())) cat('There are no observations with all features present.')
