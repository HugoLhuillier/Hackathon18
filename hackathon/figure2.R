samplea<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/sample_A.csv",
                  col_types = cols(new_hrs=col_double(),new_wage=col_double(),wgt=col_double(),hh_wgt=col_double(),hhid=col_double()))
nipa<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/nipa_income.csv")
ggplot(nipa)+geom_line(aes(x=true_year,y=salary_nipa))+geom_line(aes(x=true_year,y=total_income_nipa))
workingage = filter(samplea,working_age==1)
fig2 <- workingage %>%
        group_by(year,sex) %>%
        filter(!is.na(hrs),!is.na(wage)) %>%
        dplyr::summarise(
                        mean_wage = Hmisc::wtd.mean(wage),
                        mean_hour = Hmisc::wtd.mean(hrs)
        )
ggplot(fig2,aes(x=year,y=mean_wage,color=factor(sex)))+geom_line()
ggplot(fig2,aes(x=year,y=mean_hour,color=factor(sex)))+geom_line()
