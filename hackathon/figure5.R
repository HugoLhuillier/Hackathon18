sampleb<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/sample_B.csv",
                  col_types = cols(new_hrs=col_double(),new_wage=col_double(),wgt=col_double(),hh_wgt=col_double(),hhid=col_double()))
sampleb$year=sampleb$year-1

premium <- sampleb %>%
  mutate(educated=ifelse(edu>=16,1,0)) %>%
  group_by(year,sex,educated) %>%
  dplyr::summarise(
    mean_wage = Hmisc::wtd.mean(wage))
premium<- spread(premium,key=educated,value=mean_wage)
premium<- mutate(premium,premiumwage=`1`/`0`)
ggplot(premium,aes(x=year,y=premiumwage,color=factor(sex)))+geom_line() 

experiencepremium <- sampleb %>%
  mutate(experience=ifelse(age>=25&age<=35,1,ifelse(age>=45&age<=55,2,0))) %>%
  group_by(year,sex,experience) %>%
  dplyr::summarise(
    mean_wage = Hmisc::wtd.mean(wage))
experiencepremium<- spread(experiencepremium,key=experience,value=mean_wage)
experiencepremium<- mutate(experiencepremium,premiumwage=`2`/`1`)
ggplot(experiencepremium,aes(x=year,y=premiumwage,color=factor(sex)))+geom_line() 

genderpremium <- sampleb %>%
  group_by(year,sex) %>%
  dplyr::summarise(
    mean_wage = Hmisc::wtd.mean(wage))
genderpremium<- spread(genderpremium,key=sex,value=mean_wage)
genderpremium<- mutate(genderpremium,premiumwage=`1`/`2`)
ggplot(genderpremium,aes(x=year,y=premiumwage))+geom_line() 
