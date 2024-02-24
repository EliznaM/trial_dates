
library(tidyverse)
# M1, T2, W3, T4, F5, S6, S7
# M8, 

x <- seq(1,511, by = 7)
ll <- vector("list", length(x))

for(i in x){
  ll[[i]] <- c(i, i+1, i+2, i+3)
}
#screening left out
study_days <- unlist(ll)
# study_days <- c(-6, study_days)

# number of necessary enrolment days to reach 450
# nr_enrol <- 4
x <- nr_enrol - 0.1*nr_enrol  # screen failure
enrol_days <- ceiling(450/x)


ll <- vector("list", enrol_days)


for(i in 1:length(ll)){
  tmp <- rep(c(study_days[i], study_days[i] + 7, 
               study_days[i] + 28, 
    study_days[i] + 35, study_days[i] + 56, study_days[i] + 106, 
    study_days[i] + 156, study_days[i] + 209, 
    study_days[i] + 269, study_days[i] + 329,
    study_days[i] + 389, study_days[i] + 449, study_days[i] + 509),
    nr_enrol)
  
  tmp <- t(matrix(tmp, ncol = nr_enrol))
  colnames(tmp) <- paste0("v", 1:ncol(tmp))
  ll[[i]] <- as.data.frame(tmp)
}

visit_days <- ll %>% 
  bind_rows() %>% 
  as.matrix()


visit_days[visit_days > max(study_days)] <- NA

unique(visit_days[!visit_days %in% study_days])
#these visits all do not fall on study days

for(i in 1:length(visit_days)){

  if(visit_days[i] %in% study_days == FALSE){
    if(!is.na(visit_days[i])){
x <- study_days - visit_days[i]
xx <- min(x[x > 0])
x <- which(x == xx)
visit_days[i] <- study_days[x]
}
  }
}

unique(visit_days[!visit_days %in% study_days &
                    !is.na(visit_days)])  # 0


sort(table(visit_days))

visit_days <- visit_days %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(),
  names_to = "key", values_to = "value") %>% 
  mutate(type_visit = ifelse(key %in% c("v6", "v7", "v9", "v10"), "phone", "f2f"))



