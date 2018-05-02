###################################################################################
# FIGURE 1: Comparison between averages in CPS and NIPA: labor and pre-tax income
###################################################################################

doFig1 <- function(data, toSave = F){
  # compute the weighted mean of labor income and pre-tax income for each year of the sample, 
  # excluding all entries where either labor income or pre-tax income is missing.
  # note that we deflate both variables by dividing by the CPI
  fig1 <- data %>% 
    group_by(year) %>% 
    filter(!is.na(labor_income), !is.na(pre_tax_income)) %>%
    dplyr::summarize(salary_cps       = Hmisc::wtd.mean(labor_income, wgt), 
                     total_income_cps = Hmisc::wtd.mean(pre_tax_income, wgt), 
                     # note: do this to keep the CPI in our summarized tibble
                     # since the cpi is the same for all the household, mean(cpi) = cpi
                     cpi              = mean(cpi)) %>%
     mutate(salary_cps       = salary_cps / cpi * 100, 
            total_income_cps = total_income_cps / cpi * 100)
  
  # load the NIPA data
  fig1_temp <- read_csv("./data-to-be-merge/nipa_income.csv")
  # merge the two dataset based on true_year
  fig1 <- left_join(fig1, fig1_temp)
  
  # produces the plot, respecting the theme of HPI (2009)
  p1 <- ggplot(fig1) + 
    geom_line(aes(true_year, log(salary_cps), color = "CPS"), size = 1.2) +
    geom_point(aes(true_year, log(salary_nipa), color = "NIPA"), size = 1.2) + 
    labs( title = "Labor Income Per Capita", 
          subtitle="Log - 2000$",
          x="Year",
          y="") + 
    scale_colour_manual(name="",values=c("blue","red"))
  
  p2 <- ggplot(fig1) + 
    geom_line(aes(true_year, log(total_income_cps), color = "CPS"), size = 1.2) +
    geom_point(aes(true_year, log(total_income_nipa), color = "NIPA"), size = 1.2) + 
    labs( title = "Pre-Tax Income Per Capita", 
          subtitle="Log - 2000$",
          x="Year",
          y="") + 
    scale_colour_manual(name="",values=c("blue","red"))
  
  # if toSave = TRUE, save the resulting plots
  if(toSave){
    pdf('Fig1.pdf')
    grid.arrange(p1,p2,ncol=2,nrow=1)
    dev.off()
  }
  # use grid.arrange() from the gridExtra package to combine plot 1 & 2 
  return(grid.arrange(p1,p2))
}