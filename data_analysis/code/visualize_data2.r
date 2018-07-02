## ---- feature-relationships ----

library(ggplot2, quietly = TRUE)

ggplot(data = wait_times) +
    geom_point(mapping = aes(x = Consult_90th, y = Surgery_90th)) +
    facet_wrap(~ Specialty, nrow = 4)
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
