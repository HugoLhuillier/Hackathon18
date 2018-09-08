sampgender = function(g){

data2 = sampA %>% filter(between(age, 25, 60)) %>% 
  select(sex, year, wage, new_wage)

wage1 = data2 %>% 
  select(year, wage, new_wage, sex) %>%
  filter(!is.na(wage), year < 1976) %>%
  filter(sex == g) %>%
  group_by(year) %>%
  summarise(wage = mean(wage)) %>%
  mutate(new_wage = NA) 

wage2 = data2 %>% 
  select(year, wage, new_wage, sex) %>%
  filter(!is.na(wage), year > 1975) %>%
  filter(!is.na(new_wage)) %>%
  filter(sex == g) %>%
  group_by(year) %>%
  summarise(wage = mean(wage), 
            new_wage = mean(new_wage)) %>%
  mutate(bias = wage - new_wage)

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

wagem = sampgender(1)
wagef = sampgender(2)

wm = ggplot(wagem, aes(x = year, y = wage)) + geom_line() + 
  labs(title = "Average Male Wage", 
       x = "Year", 
       y = "")

wf = ggplot(wagef, aes(x = year, y = wage)) + geom_line() + 
  labs(title = "Average Female Wage", 
       x = "Year", 
       y = "")

sampgender2 = function(g){
  
  data2 = sampA %>% filter(between(age, 25, 60)) %>% 
    select(sex, year, hrs, new_hrs)
  
  hrs1 = data2 %>% 
    select(year, hrs, new_hrs, sex) %>%
    filter(!is.na(hrs), year < 1976) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    summarise(hrs = mean(hrs)) %>%
    mutate(new_hrs = NA) 
  
  hrs2 = data2 %>% 
    select(year, hrs, new_hrs, sex) %>%
    filter(!is.na(hrs), year > 1975) %>%
    filter(!is.na(new_hrs)) %>%
    filter(sex == g) %>%
    group_by(year) %>%
    summarise(hrs = mean(hrs), 
              new_hrs = mean(new_hrs)) %>%
    mutate(bias = hrs - new_hrs)
  
  B = hrs2 %>% summarise(bias = mean(bias))
  b = B$bias
  
  final1 = hrs1 %>%
    mutate(hrs1 = hrs - b) %>%
    select(year, hrs1)
  
  final2 = hrs2 %>%
    select(year, new_hrs) %>%
    filter(year>1975)
  
  data = bind_rows(final1, final2) %>% 
    mutate(hrs = ifelse(year<1976, hrs1, new_hrs))
  
  return(data)
}

hours1 = sampgender2(1)
hours2 = sampgender2(2)

hm = ggplot(hours1, aes(x = year, y = hrs)) + geom_line() + 
  labs(title = "Average Male Annual hours", 
       x = "Year", 
       y = "")

hf = ggplot(hours2, aes(x = year, y = hrs)) + geom_line() + 
  labs(title = "Average Female Annual hours", 
       x = "Year", 
       y = "")

grid.arrange(wm, wf, hm, hf, ncol = 2)







  