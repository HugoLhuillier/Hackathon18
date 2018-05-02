samplec<-read_csv("C:/Users/JIEJIE/Desktop/Rfinal/sample_C.csv")
quantiles <- samplec %>%
  filter(!is.na(wage)) %>%
  group_by(year) %>%
  dplyr::summarise(
    quantile_wage95 = reldist::wtd.quantile(x=wage,q=0.95,weight=hh_wgt,na.rm=T),
    quantile_wage90 = reldist::wtd.quantile(x=wage,q=0.9,weight=hh_wgt,na.rm=T),
    quantile_wage75 = reldist::wtd.quantile(x=wage,q=0.75,weight=hh_wgt,na.rm=T),
    quantile_wage50 = reldist::wtd.quantile(x=wage,q=0.5,weight=hh_wgt,na.rm=T),
    quantile_wage25 = reldist::wtd.quantile(x=wage,q=0.25,weight=hh_wgt,na.rm=T),
    quantile_wage10 = reldist::wtd.quantile(x=wage,q=0.1,weight=hh_wgt,na.rm=T),
    quantile_wage5  = reldist::wtd.quantile(x=wage,q=0.05,weight=hh_wgt,na.rm=T)
  )
ggplot(quantiles)+geom_line(aes(x=year,y= quantile_wage95))
