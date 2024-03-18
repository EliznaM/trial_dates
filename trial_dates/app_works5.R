


# functions are in R/utils.R

library(shiny)
library(magrittr)
library(ggplot2)

# Define UI
ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("inputs",
      # inputs
      
      # days of week available for visits
      selectInput(
        "days_of_week",
        label = "days of week available for visits",
        choices = c("Monday", "Tuesday", "Wednesday",
                    "Thursday", "Friday"),
        multiple = TRUE
      ),
      
      
      
      # visits (start at day 1)
      
      textInput("visits", label = "visits"),
      
      # visit names (in same order as visits)
      
      textInput("visit_names", label = "visit names"),
      
      # visit types (in same order as visits)
      
      textInput("visit_type", label = "visit types"),
      
      # number of study days to track
      
      numericInput(
        "total_study_days",
        label = "total study days",
        value = 365,
        min = 1,
        max = ceiling(5 * 365.25)
      ),
      
      # first day of December break
      sliderInput(
        "dec_start",
        label = "first day of December break",
        value = 23,
        min = 1,
        max = 31
      ),
      
      # last day of January break
      
      sliderInput(
        "jan_end",
        label = "last day of January break",
        value = 3,
        min = 1,
        max = 31
      ),
      
      # study start date
      
      dateInput("study_start", label = "study start date"),
      
      # patient start date
      
      
      # enrolment target
      
      numericInput("enrol_target", label = "enrolment target",
                   value = 200),
      
      # number enrolled per day
      numericInput(
        "enrol_per_day",
        label = "target enrolment per day",
        value = 3,
        min = 1,
        max = 10
      ),
      
      # enrolment failure rate
      
      numericInput(
        "enrol_fail",
        label = "enrolment failure rate",
        value = 10,
        min = 0,
        max = 100,
        step = 5
      )
      
    ),
    # end of tabPanel
    
    tabPanel("output 1",
      # textOutput("xx"),
      textOutput("enrol_days"),
      # tableOutput("visit_days"),
      
      downloadButton("download1", label = "Download visit dates")
      
    ),
    # end of tabpanel
    
    tabPanel("plot 1",
      plotOutput("vdays_plot")),
    # end of tabPanel2
    
    tabPanel("plot 2",
      plotOutput("vdays_plot2"))
  )
)





############################################################



# Define server logic
server <- function(input, output, session) {
  study_days <- reactive({
    req(input$days_of_week,
        input$total_study_days,
        input$visits,
        input$visit_type)
    
    x <- make_study_days(
      days_of_w = input$days_of_week,
      total_study_d = input$total_study_days,
      visits = input$visits
    )
    x
  })
  
  
  enrol_days <- reactive({
    req(input$enrol_per_day,
        input$enrol_fail,
        input$visit_type)
    
    x1 <- calc_enrol_days(
      enrol_per_d = input$enrol_per_day,
      enrol_f = input$enrol_fail,
      enrol_t = input$enrol_target
    )
    x1
  })
  
  
  # xx <- reactive({unlist(stringr::str_split(input$visit_type, pattern = ","))})
  #
  # output$xx <- renderPrint({
  #   xx()
  #   })
  # visit_days reactive START
  
  visit_days <- reactive({
    req(
      input$visits,
      input$enrol_per_day,
      input$total_study_days,
      input$study_start,
      input$visit_type
    )
    
    tmp <- make_visit_days(
      enrol_ds = enrol_days(),
      study_ds = study_days(),
      vsts = input$visits,
      enrol_per_d = input$enrol_per_day,
      total_study_ds = input$total_study_days,
      study_s = input$study_start,
      visit_t = input$visit_type
    )
    
    tmp
    
  })
  
  # add enrol days output
  
  output$enrol_days <- renderPrint({
    req(visit_days())
    glue::glue("Number of days to enrol to reach target is {enrol_days()}")
  })
  
  # output$visit_days <- renderTable({
  #   visit_days()})
  #
  # download visit_dates
  
  output$download1 <- downloadHandler(
    filename = function() {
      "visit_dates.csv"
    },
    content = function(file) {
      write.csv(visit_days(), file)
    }
  )
  
  
  # add visit days plot with study day
  
  output$vdays_plot <- renderPlot({
    req(visit_days(),
        input$total_study_days,
        input$visits)
    
    total_study_d <-
      calc_total_study_days(total_study_d = input$total_study_days,
                            visits = input$visits)
    visit_days() %>%
      dplyr::count(visit_type, study_day) %>%
      dplyr::filter(!is.na(study_day)) %>%
      ggplot(aes(study_day, n, fill = visit_type)) +
      geom_col(width = 0.2) +
      # scale_fill_manual(values = themeCols$hex5[c(1,4)])+
      scale_y_continuous(breaks = seq(0, 50, by = input$enrol_per_day)) +
      scale_x_continuous(breaks = seq(1, total_study_d, by = 7)) +
      theme(axis.text.x = element_text(angle = 90))
    
  })
  
  
  
  output$vdays_plot2 <- renderPlot({
    req(visit_days(),
        input$total_study_days,
        input$visits,
        input$enrol_per_day)
    
    total_study_d <-
      calc_total_study_days(total_study_d = input$total_study_days,
                            visits = input$visits)
    visit_days() %>%
      dplyr::count(visit_type, date) %>%
      dplyr::filter(!is.na(date)) %>%
      ggplot(aes(date, n, fill = visit_type)) +
      geom_col(width = 0.2) +
      # scale_fill_manual(values = themeCols$hex5[c(1,4)])+
      # scale_y_continuous(breaks = seq(0, 50, by = input$enrol_per_day)) +
      # scale_x_continuous(breaks = seq(1, total_study_d, by = 7)) +
      theme(axis.text.x = element_text(angle = 90))
    
  })
  
}

###########################################################
# Run the application
shinyApp(ui = ui, server = server)
