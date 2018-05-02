sampleb<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/sample_B.csv",
                  col_types = cols(new_hrs=col_double(),new_wage=col_double(),wgt=col_double(),hh_wgt=col_double(),hhid=col_double()))
sampleb$year=sampleb$year-1
variancelogwages <- sampleb %>%
                    group_by(year,sex) %>%
                    mutate(logwage=log(wage)) %>%
                    dplyr::summarise(
                    var_wage = Hmisc::wtd.var(logwage))
ggplot(variancelogwages,aes(x=year,y=var_wage,color=factor(sex)))+geom_line()  

giniwage <- sampleb %>%
  filter(!is.na(wage))  %>%
  group_by(year,sex) %>%
  dplyr::summarise(
    gini_wage = reldist::gini(wage,wgt))
ggplot(giniwage,aes(x=year,y=gini_wage,color=factor(sex)))+geom_line()

quantile5010 <- sampleb %>%
  group_by(year,sex) %>%
  dplyr::summarise(
    quantile_wage50 = reldist::wtd.quantile(x=wage,q=0.5,weight=wgt,na.rm=T),
    quantile_wage10 = reldist::wtd.quantile(x=wage,q=0.1,weight=wgt,na.rm=T),
    quantilewage5010=quantile_wage50/quantile_wage10
    )
ggplot(quantile5010,aes(x=year,y=quantilewage5010,color=factor(sex)))+geom_line()
