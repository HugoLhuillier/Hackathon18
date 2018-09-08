data3 = sampA %>% select(year, wage, new_wage, sex, wgt)

variance = function(data, g){
  
    wage1 = data %>% 
    filter(!is.na(wage), year < 1976) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    mutate(log = log(wage)) %>%
    dplyr::summarise(wage = wtd.var(log, wgt)) %>%
    mutate(new_wage = NA) 
  
  wage2 = data %>% 
    filter(!is.na(wage), year > 1975) %>%
    filter(!is.na(new_wage)) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    mutate(log = log(wage), 
           logn = log(new_wage)) %>%
    dplyr::summarise(wage = wtd.var(log, wgt), 
              new_wage = wtd.var(logn, wgt)) %>%
    mutate(bias = wage - new_wage)
  
  B = wage2 %>% dplyr::summarise(bias = mean(bias))
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

var1 = variance(data3, 1) %>% mutate(gndr = "Male")
var2 = variance(data3, 2) %>% mutate(gndr = "Female")
var = bind_rows(var1, var2)

gvar = ggplot(var, aes(x = year, y = wage)) + 
  geom_line(aes(linetype = gndr, colour = gndr)) + 
  labs(title = "Variance of log hourly wages",
       x = "Year",
       y = "")

quant = function(data, g, q, t){
  
  wage1 = data %>% 
    filter(!is.na(wage), year < 1976) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    dplyr::summarise(wage50 = wtd.quantile(wage, probs = c(q)), 
              wage10 = wtd.quantile(wage, probs = c(t))) %>%
    mutate(wage = wage50/wage10, 
           w_wage = NA) 
  
  wage2 = data %>% 
    filter(!is.na(wage), year > 1975) %>%
    filter(!is.na(new_wage)) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    dplyr::summarise(wage50 = wtd.quantile(wage, wgt, probs = c(q)), 
              wage10 = wtd.quantile(wage, wgt, probs = c(t)), 
              nwage50 = wtd.quantile(new_wage, wgt, probs = c(q)), 
              nwage10 = wtd.quantile(new_wage, wgt, probs = c(t))) %>%
    mutate(wage = wage50/wage10,
           nwage = wage50/wage10, 
           bias = wage-nwage)
  
  B = wage2 %>% dplyr::summarise(bias = mean(bias))
  b = B$bias
  
  final1 = wage1 %>%
    mutate(wage1 = wage - b) %>%
    select(year, wage1)
  
  final2 = wage2 %>%
    select(year, nwage) %>%
    filter(year>1975)
  
  data = bind_rows(final1, final2) %>% 
    mutate(wage = ifelse(year<1976, wage1, nwage))
  
  return(data)
}

quant1 = quant(data3, 1, 0.5, 0.1) %>% mutate(gndr = "Male")
quant2 = quant(data3, 2 ,0.5, 0.1) %>% mutate(gndr = "Female")
quant = bind_rows(quant1, quant2)

gquant1 = ggplot(quant, aes(x = year, y = wage)) + 
  geom_line(aes(linetype = gndr, colour = gndr)) + 
  labs(title = "P50 - P10 ratio of hourly wages",
       x = "Year",
       y = "")

quant11 = quant(data3, 1, 0.9, 0.5) %>% mutate(gndr = "Male")
quant21 = quant(data3, 2 ,0.9, 0.5) %>% mutate(gndr = "Female")
QUant = bind_rows(quant11, quant21)

gquant2 = ggplot(QUant, aes(x = year, y = wage)) + 
  geom_line(aes(linetype = gndr, colour = gndr)) + 
  labs(title = "P90 - 510 ratio of hourly wages",
       x = "Year",
       y = "")


gini = function(data, g){
  
  wage1 = data %>% 
    filter(!is.na(wage), year < 1976) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    dplyr::summarise(wage = reldist::gini(wage, wgt)) %>%
    mutate(new_wage = NA) 
  
  wage2 = data %>% 
    filter(!is.na(wage), year > 1975) %>%
    filter(!is.na(new_wage)) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    dplyr::summarise(wage = reldist::gini(wage, wgt), 
              new_wage = reldist::gini(new_wage, wgt)) %>%
    mutate(bias = wage - new_wage)
  
  B = wage2 %>% dplyr::summarise(bias = mean(bias))
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

gini1 = gini(data3, 1) %>% mutate(gndr = "Male")
gini2 = gini(data3, 2) %>% mutate(gndr = "Female")
gini = bind_rows(gini1, gini2)

gini = ggplot(gini, aes(x = year, y = wage)) + 
  geom_line(aes(linetype = gndr, colour = gndr)) + 
  labs(title = "Gini Coefficient of hourly wages",
       x = "Year",
       y = "")

grid.arrange(gvar, gini,gquant1, gquant2, ncol = 2)





