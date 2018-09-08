inc = sampA %>% select(year, labor_income, pre_tax_income, cpi, wgt) %>%
  mutate(true_year = year - 1, 
         real_income = labor_income/cpi * 100, 
         real_pre_tax = pre_tax_income/cpi * 100) %>%
  group_by(true_year) %>%
  summarise(real_income = log(weighted.mean(real_income, wgt)), 
            real_pre_tax = log(weighted.mean(real_pre_tax, wgt)), 
            cpi = mean(cpi)) %>%
  mutate(type = "CPS") %>%
  select(true_year, real_income, real_pre_tax, type)

nipa = nipa %>% 
  mutate(real_income = log(salary_nipa), 
         real_pre_tax = log(total_income_nipa), 
         type = "NIPA") %>%
  select(true_year, real_income, real_pre_tax, type)

g1 = ggplot(data = nipa, aes(x = true_year, y = real_income)) + 
  geom_point(color = "red") + 
  geom_line(data = inc, color = "blue") + 
  theme_bw() + 
  labs(title = "Labor Income Per Capita", 
       x = "Year", 
       y = "")

g2 = ggplot(data = nipa, aes(x = true_year, y = real_pre_tax)) + 
  geom_point(color = "red") + 
  geom_line(data = inc, color = "blue", ) + 
  theme_bw() + 
  labs(title = "Pre-Tax Income Per Capita", 
       x = "Year", 
       y = "")

grid.arrange(g1, g2, ncol = 2)
    
