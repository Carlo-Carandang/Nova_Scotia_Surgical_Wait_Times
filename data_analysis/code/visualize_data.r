## ---- feature-relationships ----

library(ggplot2, quietly = TRUE)

ggplot(data = wait_times) +
    geom_point(mapping = aes(x = consult_90th, y = surgery_90th)) +
    facet_wrap(~ specialty, nrow = 4)
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

wait <- read.delim2(file = '../data_analysis/data/Surgical_Wait_Times_2017-09-04.csv')
    
plot(wait)

## ---- total-years ----

library(plotly, quietly = TRUE, warn.conflicts = FALSE)

wait_time_by_specialty <- as.data.frame(wait_time_by_specialty)

plot_ly() %>%
  add_bars(x = unique(wait_time_by_specialty$specialty), 
           y = wait_time_by_specialty$total / 365.25, type = "bar", 
           text = ~paste('Surgical Category: ', wait_time_by_specialty$specialty, 
                         '<br>', 'Wait Time (person years): ', 
                         wait_time_by_specialty$total / 365.25), 
           hoverinfo = 'none', showlegend = FALSE) %>%
  layout(margin = list(b = 110, r = 100, t = -2), 
         title = 'Total Wait Time by Surgical Specialty (2014-2016)', 
         xaxis = list(title = 'surgical specialty'), 
         yaxis = list(title = 'wait time (person years)'))

## ---- median-wait ----

wait_time_by_specialty <- as.data.frame(wait_time_by_specialty)

plot_ly() %>%
  add_bars(x = unique(wait_time_by_specialty$specialty), 
           y = wait_time_by_specialty$average, 
           type = "bar", 
           text = ~paste('Surgical Category: ', wait_time_by_specialty$specialty, 
                         '<br>', 'Wait Days: ', wait_time_by_specialty$average), 
           hoverinfo = 'none', showlegend = FALSE) %>%
  layout(margin = list(b = 110, r = 100, t = -2), 
         title = 'Median Wait Time by which Patients Received Consultation & Surgery', 
         xaxis = list(title = 'surgical specialty'), 
         yaxis = list(title = 'wait time (days)'))

## ---- distribution-specialty ----

wait_time_distribution_by_specialty <- wait_times %>% 
  select(specialty, consult_90th, surgery_90th) %>% 
  group_by(specialty)

plot_ly(data = wait_time_distribution_by_specialty, 
        y = ~consult_90th + surgery_90th, color = ~specialty, type = 'box', 
        hoverinfo = 'all', text = ~paste('')) %>% 
  layout(margin = list(b = 5, t = -2), 
         title = 'Wait Time Distribution by Surgical Specialty', 
         yaxis = list(title = 'wait time (days)', range = c(0, 1500))) 

## ---- distribution ----

plot_ly(alpha = 0.2, autobinx = FALSE, histnorm = 'counts') %>%
  add_histogram(name = 'consultation', data = wait_time_distribution_by_specialty, 
                x = ~consult_90th + surgery_90th, hoverinfo = 'none', 
                xend = 800) %>%
  layout(showlegend = FALSE, 
         legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 0.9), 
         title = 'Wait Time Distribution', 
         xaxis = list(title = 'wait time (days)'), 
         yaxis = list(title = 'frequency'))

## ---- distribution2 ----

plot_ly(alpha = 0.2, autobinx = FALSE, histnorm = 'counts', 
        xbins = list(start = 0, end = 1500, size = 30)) %>%
  add_histogram(name = 'consultation', data = wait_time_distribution_by_specialty, 
                x = ~consult_90th + surgery_90th, hoverinfo = 'none', 
                xend = 800) %>%
  layout(showlegend = FALSE, 
         legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 0.9), 
         title = 'Wait Time Distribution', 
         xaxis = list(title = 'wait time (days)'), 
         yaxis = list(title = 'frequency'))

## ---- distribution3 ----

plot_ly(alpha = 0.2, autobinx = FALSE, histnorm = 'counts', 
        xbins = list(start = 0, end = 800, size = 30)) %>%
  add_histogram(name = 'consultation', data = wait_time_distribution_by_specialty, 
                x = ~consult_90th, hoverinfo = 'none', xend = 800) %>%
  add_histogram(name = 'surgery', data = wait_time_distribution_by_specialty, 
                x = ~surgery_90th, hoverinfo = 'none', xend = 800) %>%
  layout(showlegend = TRUE, 
         legend = list(orientation = "h", xanchor = "center", x = 0.5, y = 0.9), 
         title = 'Consultation and Surgery Wait Time Distribution', 
         xaxis = list(title = 'wait time (days)'), 
         yaxis = list(title = 'frequency'))