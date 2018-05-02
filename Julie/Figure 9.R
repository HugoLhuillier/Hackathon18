quant = sampC %>% select(year, hh_earnings_equiv, hh_wgt)

sampgender = function(data){

  wage1 = data %>% 
    filter(!is.na(hh_earnings_equiv)) %>%
    group_by(year) %>%
    summarise(wage = mean(hh_earnings_equiv)) %>%
  
  
  B = wage2 %>% summarise(bias = mean(bias))
  b = B$bias
  
  final1 = wage1 %>%
    mutate(wage1 = wage - b) %>%
    select(year, wage1)
  
  final2 = wage2 %>%
    select(year, new_wage) %>%
    filter(year>1975)
  
  data = bind_rows(final1, final2) %>% 
    mutate(wage = ifelse(year<1976, wage1, new_wage))
  
  return(data)
}
