## ---- linear-regression-model ----
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(stringr)
library(tibble)
library(broom)
library(purrr)
library(e1071)
library(magrittr)

wait_times <- read.delim2(file = '../data_analysis/data/Surgical_Wait_Times_2017-09-04.csv')

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


#isolate rows with procedure = 'all'
wait_times <- wait_times %>% filter(procedure == "all")

#remove rows with null values in Consult_90th column
wait_times <- wait_times[!is.na(wait_times$consult_90th),]

#remove rows with null values in Surgery_90th column
wait_times <- wait_times[!is.na(wait_times$surgery_90th),]

#Prior to building the statistical model the baseline factor is ‘general surgery’ instead of the default ‘cardiac surgery’ to determine the impact, if any, on the linear regression model w.r.t. the null hypothesis.

#A bivariate linear regression model is constructed with two dependent variables (consult_90th and surgery_90th), representing the 90th percentiles for each instance of a surgical specialty’s wait time, added together to give the combined surgical wait time and one independent variable.
specialty_factor <- wait_times %>% select(specialty) %>% flatten_chr() #%>% 
  #as.factor() %>% relevel('general')

wait_times <- wait_times %>% 
  mutate(specialty =  specialty_factor)

specialty_consult90 <- wait_times %>% select(consult_90th) %>% unlist()
specialty_surgery90 <- wait_times %>% select(surgery_90th) %>% unlist()
specialty90 <- specialty_consult90 + specialty_surgery90
specialty <- wait_times %>% select(specialty) %>% unlist()

model <- lm(formula = specialty90 ~ specialty)


## ---- model ----
model
summary(model)
glance(model)

statistics <- glance(model)

k <- statistics %>% select(df) %>% first()
n <- statistics %>% select(df.residual) %>% first()
p <- statistics %>% select(p.value)
rse <- statistics %>% select(sigma)

f <- statistics %>% select(statistic)
f_critical <- qf(0.95, k, n - k - 1)
null_hypothesis_status <- if (f_critical < f) 'reject' else 'accept'
print("NULL Hypothesis:")
null_hypothesis_status

## ---- prediction-model ----
model_coefficient <- coef(model)
model_coefficient

model_coefficient <- data.frame(name = names(model_coefficient), 
                                value = model_coefficient, row.names = NULL)

model_coefficient %>% print.data.frame(right = FALSE, row.names = FALSE)

names(model_coefficient) <- gsub("specialty", "", names(model_coefficient))

model_coefficient <- model_coefficient %>%
  mutate(name = str_replace(name, "specialty", ""),
         name = str_replace(name, "\\(Intercept\\)", "intercept"))

model_coefficient %>% print.data.frame(right = FALSE, row.names = FALSE)

#data.frame(coefficient = model[[1]])

## ---- anova ----

av <- anova(model)
av
av %>% names()
names(av) <- gsub("\\s","", av %>% names() %>% tolower())
av %>% names()
av

n <- av %>% select(df) %>% sum() + 1
k <- av %>% select(df) %>% top_n(-1, df) %>% first()
psumsq <- av %>% select(sumsq) %>% top_n(-1, sumsq) %>% first()
rsumsq <- av %>% select(sumsq) %>% top_n(1, sumsq) %>% first()
se <- sqrt(rsumsq / (n - k - 1))
rsq <- psumsq / (rsumsq + psumsq) %>% first()

f_statistic <- av %>% select(fvalue) %>%top_n(1, fvalue) %>% first()
f_critical <- qf(0.95, k, n - k - 1)
null_hypothesis_status <- if (f_critical < f_statistic) 'reject' else 'accept'
print("ANOVA of the linear regression model; NULL Hypothesis:")
null_hypothesis_status

## ---- anova-coefficients ----

#anova(model)[[1]][1]
#anova(model)[[2]][2]
#nrow(df)
