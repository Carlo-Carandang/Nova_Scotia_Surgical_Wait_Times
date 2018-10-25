library(shiny)

ui <- fluidPage(
  tags$head(
    tags$meta(charset="UTF-8"),
    tags$meta(name="description", content="Surgical wait times in Nova Scotia are at crisis levels, as patients are waiting too long on the wait list. This can lead to worsening illness course, and in some cases premature death. This app gives you the accurate wait times prediction, to help you be an informed patient."),
    tags$meta(name="keywords", content="surgical wait times in Nova Scotia"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")
  ),
  titlePanel(h1("Nova Scotia Surgical Wait Times App",h3("Built by Dr. Carlo Carandang and Gregory Horne")), windowTitle = "Nova Scotia Surgical Wait Times App"),
  sidebarPanel(
    selectInput('Specialty1',
                'Choose a specialty',
                choices = c("General Surgery", "Cardiac Surgery", "Dental","Neurosurgery", "Obstetrics/Gynaecology", "Ophthalmology", "Oral Maxillofacial", "Orthopaedic", "Otolaryngology (ENT)", "Plastic Surgery", "Thoracic Surgery", "Urology", "Vascular Surgery"))
                ),
  mainPanel(
    h5("Estimated Total Wait Time (days)"),
    h5(textOutput("myTime")),
    h5("Preprint- Analysis of Surgical Wait Times in Nova Scotia:", a("https://www.researchgate.net/publication/323837798_Analysis_of_Surgical_Wait_Times_in_Nova_Scotia", href="https://www.researchgate.net/publication/323837798_Analysis_of_Surgical_Wait_Times_in_Nova_Scotia")),
    h5("Code Repository:", a("https://github.com/Carlo-Carandang/Nova_Scotia_Surgical_Wait_Times", href="https://github.com/Carlo-Carandang/Nova_Scotia_Surgical_Wait_Times")),
    h5("Data Analysis:", a("http://rpubs.com/carandangc/Analysis_Nova_Scotia_Surgical_Wait_Times", href="http://rpubs.com/carandangc/Analysis_Nova_Scotia_Surgical_Wait_Times"))
  )
)

server <- function(input, output) {
  output$myTime <- renderText({ 
    
    Specialty1 <- input$Specialty1
    
    library(readr)
    
    wait_times <- read_tsv(file = 'Surgical_Wait_Times_2017-09-04.csv',
                           col_names = TRUE, col_types = cols())
    
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
    
    model_pred = predict(model, data.frame(specialty = Specialty1))
    model_pred
    
  })
}

shinyApp(ui = ui, server = server)