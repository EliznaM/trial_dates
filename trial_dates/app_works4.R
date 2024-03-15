

# functions are in R/utils.R

library(shiny)
library(magrittr)
library(ggplot2)

# Define UI 
ui <- fluidPage(

  # inputs
  
  
    # days of week available for visits
  selectInput("days_of_week", 
              label = "days of week available for visits", 
              choices = c("Monday", "Tuesday", "Wednesday",
                          "Thursday", "Friday"),
              multiple = TRUE),
  
  
  
    # visits (start at day 1)
  
    textInput("visits", label = "visits"),
  
    # visit names (in same order as visits)
  
     textInput("visit_names", label = "visit names"),
  
    # visit types (in same order as visits)
  
     textInput("visit_type", label = "visit types"),
     
    # number of study days to track
  
     numericInput("total_study_days", label = "total study days",
                  value = 365, min = 1, max = ceiling(5*365.25)),
  
    # first day of December break
    sliderInput("dec_start", label = "first day of December break",
                 value = 23, min = 1, max = 31),
  
    # last day of January break
  
    sliderInput("jan_end", label = "last day of January break",
                 value = 3, min = 1, max = 31),
    
    # study start date
  
    dateInput("study_start", label = "study start date"),
  
    # patient start date
  
  
    # enrolment target
  
  numericInput("enrol_target", label = "enrolment target",
               value = 200),
  
    # number enrolled per day
  numericInput("enrol_per_day", label = "target enrolment per day",
               value = 3, min = 1, max = 10),
  
    # enrolment failure rate
  
  numericInput("enrol_fail", label = "enrolment failure rate",
               value = 10, min = 0, max = 100, step = 5),
  
  # textOutput("xx"),
  textOutput("enrol_days"),
  tableOutput("visit_days"),

downloadButton("download1", label = "Download visit dates"),

plotOutput("vdays_plot")

)



############################################################



# Define server logic 
server <- function(input, output, session) {

  study_days <- reactive({
    req(input$days_of_week, 
        input$total_study_days,
        input$visits,
        input$visit_type)
    
    x <- make_study_days(days_of_w = input$days_of_week,
                                          total_study_d = input$total_study_days,
                                          visits = input$visits)
    x
    })
  
  
    enrol_days <- reactive({
    req(input$enrol_per_day,
        input$enrol_fail,
        input$visit_type)
    
    x <- calc_enrol_days(enrol_per_d = input$enrol_per_day,
                                          enrol_f = input$enrol_fail,
                    enrol_t = input$enrol_target)
    x
    })
    
   
    # xx <- reactive({unlist(stringr::str_split(input$visit_type, pattern = ","))})
    # 
    # output$xx <- renderPrint({
    #   xx()
    #   })
  # visit_days reactive START
  
  visit_days <- reactive({
    
    req(input$visits,
        input$enrol_per_day,
        input$total_study_days,
        input$study_start,
        input$visit_type)
    
    tmp1 <- purrr::map(1:enrol_days(), 
                              ~df_structure(i = .x,
                                  study_d = study_days(),
                                  visits = input$visits,
                                  enrol_per_d = input$enrol_per_day))
    visit_days1 <- unlist(tmp1)

  #these visits all do not fall on study days
  
  tmp <- purrr::map(1:length(visit_days1),
             ~vdays_not_in_study_days(ii = .x,
                 visit_d = visit_days1,
                 study_d = study_days()))
  
 
  
  x4 <- purrr::map_dbl(1:length(tmp), 
                                 ~tmp[[.x]]$ii_x)
  
  x4 <- x4[!is.na(x4)]
  
  x5 <- purrr::map_dbl(1:length(tmp), 
                                 ~tmp[[.x]]$x)
  
  x5 <- x5[!is.na(x5)]
  
  visit_days1[x5] <- x4
   
  visit_days1 <- as.matrix(dplyr::bind_rows(tmp1))
  
  visit_days1 <- tidyr::pivot_longer(as.data.frame(visit_days1),
                                                     cols = everything(),
                                                     names_to = "visit_nr",
                                                     values_to = "study_day")
  
  total_study_d <- calc_total_study_days(total_study_d = input$total_study_days,
                                         visits = input$visits)
  
  lllll <- purrr::map(1:total_study_d,
             ~get_study_dates(date = input$study_start, x = .x))
  
  tmp1 <- as.vector(visit_days1$study_day)
  tmp2 <- unlist(lllll[tmp1])
 
 # add date and id
  
  xid <- purrr::map(1:input$enrol_per_day,
             ~seq(.x, nrow(visit_days1), by = input$enrol_per_day)) %>% 
    unlist() %>% 
    sort()

  # START HERE - infinite loop from somewhere??  join gives error
  xxtp <- unlist(stringr::str_split(input$visit_type, pattern = ","))
  xxx <- visit_days1
  
  
  visit_types <- tibble::tibble(visit_nr = xxx$visit_nr) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(visit_type = xxtp)
  

 visit_days1 <- visit_days1 %>% 
   dplyr::mutate(date = tmp2,
                 id = xid) %>% 
   dplyr::left_join(visit_types, by = visit_nr) %>% 
   dplyr::select(id, visit_nr, visit_type, study_day, date)
 
 visit_days1
 
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
  
  
  # add visit days plot
  
  output$vdays_plot <- renderPlot({
    
    req(visit_days())
    
    total_study_d <- calc_total_study_days(total_study_d = input$total_study_days,
                                           visits = input$visits)  
    visit_days() %>% 
    dplyr::count(visit_type, study_day) %>% 
    dplyr::filter(!is.na(study_day)) %>% 
    ggplot(aes(study_day, n, fill = visit_type))+
    geom_col(width = 0.2)+
    # scale_fill_manual(values = themeCols$hex5[c(1,4)])+
    scale_y_continuous(breaks= seq(0, 50, by = input$enrol_per_day))+
    scale_x_continuous(breaks = seq(1, total_study_d, by = 7))+
    theme(axis.text.x = element_text(angle = 90))
    
  })

}

###########################################################
# Run the application 
shinyApp(ui = ui, server = server)
