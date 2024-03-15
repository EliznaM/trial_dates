

library(shiny)
library(magrittr)

# Define UI for application that draws a histogram
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
  
     textInput("visit_types", label = "visit types"),
  
    # number of study days to track
  
     numericInput("total_study_days", label = "total study days",
                  value = 1, min = 1, max = ceiling(5*365.25)),
  
    # first day of December break
    numericInput("dec_start", label = "first day of December break",
                 value = 23, min = 1, max = 31),
  
    # last day of January break
  
    numericInput("jan_end", label = "last day of January break",
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
  
# textOutput("X")
textOutput("visits_listed"),
textOutput("total_enrol_days"),
textOutput("st_days"),
tableOutput("visit_days1"),
tableOutput("visit_days3"),
tableOutput("visit_days6")
)



############################################################



# Define server logic required to draw a histogram
server <- function(input, output) {

  days_of_week <- reactive({
    unlist(stringr::str_split(input$days_of_week, pattern = ","))
                              })
  visits <- reactive({
    as.numeric(
      unlist(stringr::str_split(input$visits, pattern = ","))
    )
             })
  # input$visit_names
  # input$visit_types
  total_study_days <- reactive({input$total_study_days + max(visits())})
  # input$dec_start
  # input$jan_end
  # input$study_start
  enrol_target <- reactive({input$enrol_target})
  enrol_per_day <- reactive({input$enrol_per_day})
  enrol_fail <- reactive({input$enrol_fail/100})
  
  
  # output$visits_listed <- renderPrint({
  #   
  #   paste0("visits provided: ", stringr::str_flatten(visits(), collapse = ","))
  #   
  # })
  
  
  
  #  ----------------------
  
  tmp <- reactive({gsub("Monday", 1, days_of_week())})
  tmp1 <-  reactive({gsub("Tuesday", 2, tmp())})
  tmp2 <-  reactive({gsub("Wednesday", 3, tmp1())})
  tmp3 <-  reactive({gsub("Thursday", 4, tmp2())})
  tmp4 <-  reactive({gsub("Friday", 5, tmp3())})
  tmp5 <- reactive({sort(as.numeric(tmp4()))})
  
  
 
  days_of_w <- reactive({tmp5()[-1] - tmp5()[1]})
  
 

  x <- reactive({seq(1,total_study_days(), by = 7)})

  ll <- reactive({purrr::map(x(), 
    ~c(.x, .x+days_of_w()))})
    
  #
  #
  study_days <- reactive({unlist(ll())})
  
  # output$st_days <- renderPrint({
  #   
  #   study_days()
  #   
  # })


  x1 <- reactive({enrol_per_day() - enrol_fail()*enrol_per_day()})  # screen failure
  enrol_days <- reactive({ceiling(enrol_target()/x1())})

  # output$total_enrol_days <- renderPrint({
  #   
  #   paste0("total days enrolling = ", enrol_days())
  #   
  # })
  
  f1 <- function(i, study_d,
                 vists,
                 enrol_per_d) {
    
    tmp7 <- rep(c(study_d[i], study_d[i] + vists[-1]),
                 enrol_per_d)

    tmp8 <- t(matrix(tmp7, ncol = enrol_per_d))
    colnames(tmp8) <- paste0("v", 1:ncol(tmp8))
    tmp9 <- as.data.frame(tmp8)
    
    return(tmp9)
  }
  
  lll <- reactive({purrr::map(1:enrol_days(), 
                              ~f1(i = .x,
                                  study_d = study_days(),
                                  vists = visits(),
                                  enrol_per_d = enrol_per_day()))})
  
   

  # visit_days <- reactive({as.matrix(dplyr::bind_rows(lll()))})
  
  visit_days <- reactive({unlist(lll())})
  
  # visit_days1 <- reactive({pivot_longer(as.data.frame(visit_days()),
  #              cols = everything(),
  #              names_to = "visit_nr",
  #              values_to = "study_day")})
  
  # xxx <- reactive({which(is.na(visit_days1()[,2]))})
  # output$visit_days1 <- renderPrint({
  # 
  #   xxx()
  # 
  # })

# x2 <- reactive({which(visit_days() > max(study_days()))})
# 
# f2 <- function(pos, vect){
#   vect[pos] <- NA
#   return(vect)
# }
#   visit_days2 <- reactive({f2(pos = x2(), vect = visit_days())})
  
  # visit_days2 <- reactive({visit_days()})

  #these visits all do not fall on study days
  
  f3 <- function(ii, visit_d, study_d){

      if(visit_d[ii] %in% study_d == FALSE){
        
          x <- as.numeric(visit_d[ii])
          x <- study_d - x
          xx <- min(x[x > 0])
          x <- which(x == xx)
          ii_x <- ii
          x <- study_d[x]
        
      }else{
        x <- NA
        ii_x <- NA
      }
    return(list(ii_x = ii_x, x = x))
    }

  llll <- reactive({purrr::map(1:length(visit_days()),
             ~f3(ii = .x,
                 visit_d = visit_days(),
                 study_d = study_days()))})
  
  f2.1 <- function(vect){
      vect <- vect[!is.na(vect)]
    }
  
  x4 <- reactive({purrr::map_dbl(1:length(llll()), 
                                 ~llll()[[.x]]$ii_x)})
  
  x4.1 <- reactive(f2.1(x4()))
  
  x5 <- reactive({purrr::map_dbl(1:length(llll()), 
                                 ~llll()[[.x]]$x)})
  
  x5.1 <- reactive(f2.1(x5()))
  
  f4 <- function(pos, vect, new_values){
   
    vect[pos] <- new_values
    return(vect)
  }
  
  
  visit_days3 <- reactive({f4(pos = x5.1(), vect = visit_days(),
                              new_values = x4.1())})
  
  # output$visit_days3 <- renderPrint({
  # 
  #   visit_days3()
  # 
  # })
  
    # f2.1 <- function(vect){
  #   vect <- vect[!is.na(vect)]
  # }
  # 
  # visit_days4 <- reactive({f2.1(vect = visit_days3())})
  

  # for(i in 1:length(visit_days)){
  # 
  #   if(visit_days[i] %in% study_days == FALSE){
  #     if(!is.na(visit_days[i])){
  #       x <- study_days - visit_days[i]
  #       xx <- min(x[x > 0])
  #       x <- which(x == xx)
  #       visit_days[i] <- study_days[x]
  #     }
  #   }
  # }

  # 
  visit_days5 <- reactive({as.matrix(dplyr::bind_rows(lll()))})
  
  visit_days6 <- reactive({pivot_longer(as.data.frame(visit_days5()),
                                                     cols = everything(),
                                                     names_to = "visit_nr",
                                                     values_to = "study_day")})
  # visit_days5 <- reactive({
  #   data.frame(visit_nr = 1:length(visit_days3()),
  #                 study_day = visit_days3())

    # })
  #
  #
  #
  #
  output$visit_days6 <- renderTable({
    visit_days6()})

}

###########################################################
# Run the application 
shinyApp(ui = ui, server = server)
