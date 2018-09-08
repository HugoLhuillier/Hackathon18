
sampB = sampB %>% mutate(yind = "PROB")



sampB$yind = ifelse(between(sampB$year, 1968, 1980), "1967-1979", 
                               ifelse(between(sampB$year, 1981, 1990), "1980-1989", 
                               ifelse(between(sampB$year, 1991, 2000), "1990-1999", 
                               ifelse(sampB$year > 2000, "from 2000", sampB$yind))))

age = sampB %>% group_by(yind, sex) %>%
  summarise(av = weighted.mean(age,wgt)) %>%
  spread(key = yind, value = av) %>%
  mutate(name = c("Avg male age", "Avg female age")) %>%
  select(-sex)

edu16m = sampB %>% select(yind, edu, sex) %>%
  filter(sex == 1 & edu >= 16) %>%
  group_by(yind) %>%
  summarise(n = n()) %>%
  mutate(eff = c(nrow(filter(sampB, yind == "1967-1979" & sex == 1)), 
                 nrow(filter(sampB, yind == "1980-1989" & sex == 1)), 
                 nrow(filter(sampB, yind == "1990-1999" & sex == 1)), 
                 nrow(filter(sampB, yind == "from 2000" & sex == 1)))) %>%
  mutate(edu16 = n * 100 / eff) %>%
  select(yind, edu16) %>%
  spread(key = yind, value = edu16) %>%
  mutate(name = c("% male > 16 years edu"))

edu16f = sampB %>% select(yind, edu, sex) %>%
  filter(sex == 2 & edu >= 16) %>%
  group_by(yind) %>%
  summarise(n = n()) %>%
  mutate(eff = c(nrow(filter(sampB, yind == "1967-1979" & sex == 2)), 
                 nrow(filter(sampB, yind == "1980-1989" & sex == 2)), 
                 nrow(filter(sampB, yind == "1990-1999" & sex == 2)), 
                 nrow(filter(sampB, yind == "from 2000" & sex == 2)))) %>%
  mutate(edu16 = n * 100 / eff) %>%
  select(yind, edu16) %>%
  spread(key = yind, value = edu16) %>%
  mutate(name = c("% female > 16 years edu"))

table2 = bind_rows(age, edu16m, edu16f) %>% select(name,"1967-1979", "1980-1989", 
                                                   "1990-1999", "from 2000")
table2








