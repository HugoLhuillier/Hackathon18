####################################
# TABLE 1: based on sample B
####################################

# NOTE: ignore the first two columns

doTab1 <- function(data){
  # add a factor to distinguish the four time periods
  data <- data %>% 
    mutate(true_year = year - 1, 
           year_group = ifelse(true_year %in% 1967:1979, 1, ifelse(true_year %in% 1980:1989, 2, ifelse(true_year %in% 1990:1999, 3, 4))), 
           year_group = factor(year_group)) 
  
  # compute average age
  table1_age <- data %>%
    group_by(year_group, sex) %>% 
    dplyr::summarize(avg_age = mean(age, na.rm = T))
  
  # compute percentage of male / female with more than 16 years of education
  table1_edu_perc <- data %>% 
    mutate(educ_16 = ifelse(edu >= 16, 1, 0)) %>% 
    group_by(year_group, sex, educ_16) %>%
    dplyr::summarize(n16 = n()) %>% 
    group_by(year_group, sex) %>%
    mutate(freq = n16 / sum(n16)) %>% 
    filter(educ_16 == 1)
  
  # combine the results in a dataframe
  table1 <- data.frame("1967-1979" = rep(NA, 4), 
                       "1980-1989" = rep(NA, 4), 
                       "1990-1999" = rep(NA, 4), 
                       ">= 2000" = rep(NA, 4), 
                       stringsAsFactors=FALSE) 
  table1[1,] <- round(filter(table1_age, sex == 1)$avg_age, 1)
  table1[2,] <- round(filter(table1_age, sex == 2)$avg_age, 1)
  table1[3,] <- round(filter(table1_edu_perc, sex == 1)$freq * 100, 1)
  table1[4,] <- round(filter(table1_edu_perc, sex == 2)$freq * 100, 1)
  
  return(table1)
}