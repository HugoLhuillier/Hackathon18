#############################################################
# FIGURE 2: Average wages and hours worked for men and women
#############################################################

doFig2 <- function(data, toSave = F){
  # for each year, each sex, compute the weighted mean of hours (old & new definition) and wage (old & new)
  # note that we do not remove ex ante all the entries where hrs, new_hrs, wage and new_wage are NAs
  # otherwise, we would only keep the entries after 1975. instead, we remove the entries with missing values 
  # only when they are relevent for the computation (i.e. within wtd.mean with the switch na.rm = T)
  fig2 <- data %>% 
    group_by(year, sex) %>% 
    dplyr::summarize(hrs      = Hmisc::wtd.mean(hrs, wgt, na.rm = T), 
                     new_hrs  = Hmisc::wtd.mean(new_hrs, wgt, na.rm = T),
                     wage     = Hmisc::wtd.mean(wage, wgt, na.rm = T), 
                     new_wage = Hmisc::wtd.mean(new_wage, wgt, na.rm = T)) 
  
  # compute the bias of the old measure of hours. to do that, we consider the period where both the old
  # and the new definitions are available (from 1975 ownard of true_year, so 1976 for year). then, for each 
  # sex, we compute the mean difference between the new and the old definition
  mean_effects <- fig2 %>%
    filter(year > 1975) %>%
    group_by(sex) %>%
    dplyr::summarize(bias_hrs  = mean(new_hrs - hrs),
                     bias_wage = mean(new_wage - wage))
  
  # the final hours and wages that we plot are the new hours and new wage for the 1975-2005 period, and the 
  # old hours and old wages corrected for their bias before. 
  fig2 <- full_join(fig2,mean_effects) %>%
          mutate(hours_comb = ifelse(year < 1976, hrs + bias_hrs, new_hrs),
                 wage_comb  = ifelse(year < 1976, wage + bias_wage, new_wage)) %>%
          select(year, sex, hours_comb, wage_comb)
  
  # function that generates the desired plot. this allows us to not repeat four times the almost exact same code
  doPlot <- function(isWage, sexe, title, ylim){
    if(isWage)
      p <- ggplot(filter(fig2, sex == sexe)) + 
            geom_line(aes(x = year - 1, y = wage_comb), color = "blue", size = 1.2) 
    else 
      p <- ggplot(filter(fig2, sex == sexe)) + 
            geom_line(aes(x = year - 1, y = hours_comb), color = "blue", size = 1.2) 
    p <- p + labs( title = title, 
                   x="Year",
                   y="") + ylim(ylim[1],ylim[2])
    return(p)
  }
  
  p1 <- doPlot(T,"1","Average Male Wage (2000$)",c(18.5,22.5))
  p2 <- doPlot(T,2,"Average Female Wage (2000$)",c(11.5,17))
  p3 <- doPlot(F,1,"Average Male Annual Hours",c(1650,2300))
  p4 <- doPlot(F,2,"Average Female Annual Hours",c(800,1450))
  
  # same as before. see fig1.R
  if(toSave){
    pdf('Fig3.pdf')
    grid.arrange(p1,p2,p3,p4)
    dev.off()
  }
  return(grid.arrange(p1,p2,p3,p4))
}