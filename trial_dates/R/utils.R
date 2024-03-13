
days_to_num <- function(days_of_w) {
  sub <- tibble::tibble(day = c("Monday", "Tuesday", "Wednesday",
                                "Thursday", "Friday"),
                        nr = 1:5)
  tmp <- unlist(stringr::str_split(days_of_w, pattern = ","))
  orig <- tibble::tibble(day = days_of_w)
  sub <- dplyr::left_join(orig, sub)
  x <- dplyr::pull(sub, 2)
  return(x)
}




calc_total_study_days <- function(total_study_d, visits) {
  
  vst <- as.numeric(
    unlist(stringr::str_split(visits, pattern = ","))) - 1
  total_study_days <- as.numeric(total_study_d) + max(vst)
  return(total_study_days)
  
}


make_study_days <- function(days_of_w, total_study_d, visits){
  
  tmp <- days_to_num(days_of_w = days_of_w)
  days_of_w <- tmp[-1] - tmp[1]
  
  total_study_d <- calc_total_study_days(total_study_d = total_study_d,
                                         visits = visits)
  
  x <- seq(1,total_study_d, by = 7)
  ll <- purrr::map(x, 
                   ~c(.x, .x+days_of_w))
  study_days <- unlist(ll)
  return(study_days)
}


calc_enrol_days <- function(enrol_per_d, enrol_f, enrol_t) {
  
  x1 <- enrol_per_d - enrol_f/100*enrol_per_d  # screen failure
  enrol_t <- enrol_t # input is %
  enrol_days <- ceiling(enrol_t/x1)
  return(enrol_days)
  
}


df_structure <- function(i, study_d,
                         visits,
                         enrol_per_d) {
  
  vst <- as.numeric(
    unlist(stringr::str_split(visits, pattern = ","))) - 1
  
  tmp7 <- rep(c(study_d[i], study_d[i] + vst[-1]),
              enrol_per_d)
  
  tmp8 <- t(matrix(tmp7, ncol = enrol_per_d))
  colnames(tmp8) <- paste0("v", 1:ncol(tmp8))
  tmp9 <- as.data.frame(tmp8)
  
  return(tmp9)
}

vdays_not_in_study_days <- function(ii, visit_d, study_d){
  
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

get_study_dates <- function(date, x){
  
  xx <- lubridate::ymd(date) +lubridate::days(x)
  xx <- as.character(xx)
  return(xx)
  
}


make_visit_days <- function(enrol_ds = enrol_days(),
                            study_ds = study_days(),
                            vsts = input$visits,
                            enrol_per_d = input$enrol_per_day,
                            total_study_ds = input$total_study_days,
                            study_s = input$study_start,
                            visit_t = input$visit_type){
  
  tmp1 <- purrr::map(1:enrol_ds, 
                     ~df_structure(i = .x,
                                   study_d = study_ds,
                                   visits = vsts,
                                   enrol_per_d = enrol_per_d))
  
  visit_days1 <- unlist(tmp1)
  
  #these visits all do not fall on study days
  
  tmp <- purrr::map(1:length(visit_days1),
                    ~vdays_not_in_study_days(ii = .x,
                                             visit_d = visit_days1,
                                             study_d = study_ds))
                    
                    
                    
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
                    
                    total_study_d <- calc_total_study_days(total_study_d = total_study_ds,
                                                           visits = vsts)
                    
                    lllll <- purrr::map(1:total_study_d,
                                        ~get_study_dates(date = study_s, x = .x))
                    
                    tmp1 <- as.vector(visit_days1$study_day)
                    tmp2 <- unlist(lllll[tmp1])
                    
                    # add date and id
                    
                    xid <- purrr::map(1:enrol_per_d,
                                      ~seq(.x, nrow(visit_days1), 
                                           by = enrol_per_d)) %>% 
                      unlist() %>% 
                      sort()
                    
                    # START HERE - infinite loop from somewhere??  join gives error
                    xxtp <- unlist(stringr::str_split(visit_t, pattern = ","))
                    
                    
                    xxx <- visit_days1
                    
                    
                    visit_types <- tibble::tibble(visit_nr = xxx$visit_nr) %>% 
                      dplyr::distinct() %>% 
                      dplyr::mutate(visit_type = as.vector(xxtp))
                    
                    
                    
                    
                    visit_days1 <- visit_days1 %>% 
                      dplyr::mutate(date = tmp2,
                                    id = xid) %>% 
                      dplyr::inner_join(visit_types, by = visit_nr) %>% 
                      dplyr::select(id, visit_nr, visit_type, study_day, date)
                    
                    return(visit_days1)
                    
}






