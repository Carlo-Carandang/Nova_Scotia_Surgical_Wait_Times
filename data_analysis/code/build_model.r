## ---- linear-regression-model ----
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(stringr)
library(tibble)
library(broom)
library(purrr)

#isolate rows with procedure = 'all'
wait_times <- wait_times %>% filter(Procedure == "All")

#remove rows with null values in Consult_90th column
wait_times <- wait_times[!is.na(wait_times$Consult_90th),]

#remove rows with null values in Surgery_90th column
wait_times <- wait_times[!is.na(wait_times$Surgery_90th),]

#Prior to building the statistical model the baseline factor is ‘general surgery’ instead of the default ‘cardiac surgery’ to determine the impact, if any, on the linear regression model w.r.t. the null hypothesis.

#A bivariate linear regression model is constructed with two dependent variables (consult_90th and surgery_90th), representing the 90th percentiles for each instance of a surgical specialty’s wait time, added together to give the combined surgical wait time and one independent variable.
specialty_factor <- wait_times %>% select(Specialty) %>% flatten_chr() %>% 
  as.factor() %>% relevel('General Surgery')

wait_times <- wait_times %>% 
  mutate(Specialty =  specialty_factor)

specialty_consult90 <- wait_times %>% select(Consult_90th) %>% unlist()
specialty_surgery90 <- wait_times %>% select(Surgery_90th) %>% unlist()
specialty90 <- specialty_consult90 + specialty_surgery90
specialty <- wait_times %>% select(Specialty) %>% unlist()

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

anova(model)

## ---- anova-coefficients ----

anova(model)[[1]][1]
anova(model)[[2]][2]
nrow(df)
