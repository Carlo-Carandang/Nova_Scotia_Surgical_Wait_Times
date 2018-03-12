## ---- linear-regression-model ----

specialty <- wait_times %>% filter(Procedure == 'All') %>% select(Specialty) %>% unlist()
specialty90 <- wait_times %>% filter(Procedure == 'All') %>% select(Surgery_90th) %>% unlist()
df <- as.tibble(cbind(specialty, specialty90))
model <- lm(specialty90 ~ specialty, data = df)

## ---- model ----
model
summary(model)

## ---- prediction-model ----

data.frame(coefficient = model[[1]])

## ---- anova ----

anova(model)

## ---- anova-coefficients ----

anova(model)[[1]][1]
anova(model)[[2]][2]
nrow(df)
