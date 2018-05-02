sampleb<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/sample_B.csv",
                  col_types = cols(new_hrs=col_double(),new_wage=col_double(),wgt=col_double(),hh_wgt=col_double(),hhid=col_double()))
sampleb$year=sampleb$year-1
sub97_79=  filter(sampleb,year %in% 1967:1979)
column67_79avg_age_sex <- sub97_79 %>%
                          group_by(sex) %>%
                          summarise(mean(age))
column67_79denominator <-  sub97_79 %>%
                    group_by(sex) %>%
                     summarise(count=n()) 

column67_79numerator <- sub97_79 %>%
                     filter(edu>=16) %>%
                     group_by(sex) %>%
                     summarise(count=n())
column67_79edu16=column67_79numerator/column67_79denominator




          