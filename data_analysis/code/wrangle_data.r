## ---- wrangling ----

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(stringr)
library(tibble)
library(e1071)
library(magrittr)

## ---- column-names ----

# wait_times %>% names()

feature_name <- wait_times %>% names()
feature_name %>% noquote()

names(wait_times) <- wait_times %>% names() %>% tolower()

wait_times <- wait_times %>%
  mutate(specialty = tolower(specialty),
         specialty = str_replace(specialty, " surgery", ""),
         procedure = tolower(procedure))

specialty_type <- wait_times %>% filter(!is.na(specialty)) %>% select(specialty) %>% 
  distinct() %>% arrange(specialty) %>%  as.data.frame()
specialty_type %>% print.data.frame(right = FALSE, row.names = FALSE)

## ---- missing-values-check ----

# as.data.frame(tibble(feature = wait_times %>% names(),
#        missing_count = c(sum(is.na(wait_times$Period)),
#                          sum(is.na(wait_times$Specialty)),
#                          sum(is.na(wait_times$Procedure)), 
#                          sum(is.na(wait_times$Provider)), 
#                          sum(is.na(wait_times$Zone)), 
#                          sum(is.na(wait_times$Facility)),
#                          sum(is.na(wait_times$Year)),
#                          sum(is.na(wait_times$Quarter)),
#                          sum(is.na(wait_times$Consult_Median)),
#                          sum(is.na(wait_times$Consult_90th)),
#                          sum(is.na(wait_times$Surgery_Median)),
#                          sum(is.na(wait_times$Surgery_90th))),
#        non_missing_count = c(sum(!is.na(wait_times$Period)),
#                          sum(!is.na(wait_times$Specialty)),
#                          sum(!is.na(wait_times$Procedure)), 
#                          sum(!is.na(wait_times$Provider)), 
#                          sum(!is.na(wait_times$Zone)), 
#                          sum(!is.na(wait_times$Facility)),
#                          sum(!is.na(wait_times$Year)),
#                          sum(!is.na(wait_times$Quarter)),
#                          sum(!is.na(wait_times$Consult_Median)),
#                          sum(!is.na(wait_times$Consult_90th)),
#                          sum(!is.na(wait_times$Surgery_Median)),
#                          sum(!is.na(wait_times$Surgery_90th)))))
# 
# if (!sum(wait_times %>% complete.cases())) cat('There are no observations with all features present.')

data_frame(
  feature = c('specialty', 'procedure'),
  missing_count = c(wait_times %>% filter(is.na(specialty)) %>% tally(),
                    wait_times %>% filter(is.na(procedure)) %>% tally()) %>% unlist(),
  nonmissing_count = c(wait_times %>% filter(!is.na(specialty)) %>% tally(),
                       wait_times %>% filter(!is.na(procedure)) %>% tally()) %>% unlist()
) %>% arrange(feature) %>% print.data.frame(right = FALSE, row.names = FALSE)

wait_times %>% group_by(procedure) %>% filter(!is.na(procedure)) %>% tally() %>%
  arrange(desc(n)) %>% rename('observations' = 'n') %>% 
  as.data.frame() %>% head(5) %>% 
  print.data.frame(right = FALSE, row.names = FALSE)

#isolate rows with procedure = 'all'
wait_times <- wait_times %>% filter(procedure == "all")

as.data.frame(
  cbind(
    t(wait_times %>% 
        select(everything()) %>% 
        summarise_all(funs(sum(is.na(.))))), 
    t(wait_times %>% select(everything()) %>% 
        summarise_all(funs(sum(!is.na(.))))))) %>% 
  rownames_to_column() %>% 
  mutate(feature = rowname, missing_count = V1, nonmissing_count = V2) %>% 
  transmute(feature, missing_count, nonmissing_count) %>% arrange(feature) %>%
  print.data.frame(right = FALSE, row.names = FALSE)

complete_observation_count <- sum(wait_times %>% complete.cases())

wait_time_by_specialty <- wait_times %>% 
  filter(!is.na(consult_90th) & !is.na(surgery_90th)) %>%
  select(specialty, consult_90th, surgery_90th) %>% 
  group_by(specialty) %>% 
  summarise(minimum = as.integer(min(consult_90th + surgery_90th, na.rm = TRUE)), 
            maximum = max(consult_90th + surgery_90th, na.rm = TRUE), 
            average = as.integer(median(consult_90th + surgery_90th, na.rm = TRUE)),
            sigma = as.integer(sd(consult_90th + surgery_90th, na.rm = TRUE)),
            total = as.integer(sum(consult_90th + surgery_90th, na.rm = TRUE)),
            observations = n())

as.data.frame(wait_time_by_specialty) %>% 
  print.data.frame(right = FALSE, row.names = FALSE)

#wait_times <- wait_times %>% 
#  filter(!is.na(consult_90th) & !is.na(surgery_90th))
