#############################################################
# FIGURE 9:Percentiles of the household earnings distribution
#############################################################

doFig15 <- function(data, toSave = F){
  
  # function that computes the age effect for the variable v
  # again, again and again: need to supply v as a string; therefore, need eval(parse(text=v)) for R to understand that v is actually a variable
  # the switch isNew is used to filter the data when we are using new_wage and new_hrs: keep only the year > 1975
  doReg <- function(v, isNew = F){
    
    # if computing the age effect for new_hrs and new_wage (i.e. isNew = T), keep only the data after 1975
    if(isNew)
      t <- data %>% filter(year > 1975)
    else
      t <- data
    
    # compute, for each year, and each age, the variance of the log earnings 
    t <- t %>%
      group_by(year, head_age) %>%
      dplyr::summarise(variance = var(log(eval(parse(text=v))), na.rm = T)) %>%
      group_by() %>%
      # transform head_age and year into factors so that they are directly turned into dummy variables in our linear model
      mutate(head_age = factor(head_age), 
             year     = factor(year))
    
    # change the reference year and age in our dataset (not really needed since we are taking the difference wrt age 27 eventually)
    if(isNew){
      t <- t %>% mutate(head_age = relevel(head_age, ref = "0"), 
             year     = relevel(year, ref = "1976"))
    } else {
      t <- t %>% mutate(head_age = relevel(head_age, ref = "0"), 
             year     = relevel(year, ref = "1968"))
    }
    
    # linear model: regresse the variance on a bunch of dummy, the head age and the year, without intercept
    # the output will be an OLS model. two types of coefficients: age effect and year effect. that's how we control for year effect
    ols <- lm(variance ~ t$head_age + t$year - 1, data =t)
    
    # the 2nd to 36 coefficients are the age effect (the OLS estimates for age 1, 2, 3 etc.)
    return(coef(ols)[2:36])
  }
  
  # create a new tibble, with as variable: the age of the household, and the corresponding age effect (from doReg) for each variable of interest
  fig15 <- tibble(age = 25:59, 
                  `Head Wage` = doReg("`new_wage`", isNew = T),
                  `Head Hours` = doReg("`new_hrs`", isNew = T), 
                  `HH Earnings` = doReg("`hh_earnings`"), 
                  `HH Earnings Equiv` = doReg("`hh_earnings_equiv`"))
  
  # load the external data
  cex_15 <- read_delim("./data/cex_fig15.csv", skip = 1, delim = ";")
  psid_15 <- read_delim("./data/psid_fig15.csv", skip = 1, delim = ";")
  
  # combine the two external datasets, with an additional factor data_source that indicates the source of the data (CEX or PSID)
  cex_15 <- cex_15 %>%
    mutate(data_source = factor("CEX"))
  psid_15 <- psid_15 %>%
    mutate(data_source = factor("PSID"))
  fig15_others <- full_join(cex_15,psid_15) %>%
    mutate(data_source = factor(data_source)) %>%
    arrange(Age)
  
  # function that plots the variable of interest
  #   1. need to supply var as a string; therefore, need eval(parse(text=var)) for R to understand that var is actually a variable
  #   2. the other arguments are only to affect the plot
  doPlot <- function(var, title, ylim, noLegend = T){
    g <- ggplot(fig15_others, aes(Age, eval(parse(text=var)))) + 
      labs( title = title, 
            subtitle="Control for Year Effects",
            x="Age",
            y="") + 
      geom_line(aes(color = data_source, linetype = data_source), size = 1.2) + 
      geom_line(data = filter(fig15, age <= 57), aes(x = age, y = eval(parse(text=var)) - eval(parse(text=var))[3], color = "CPS", linetype = "CPS"), size = 1.2) + 
      geom_point(data = filter(fig15_others, data_source == "CEX"), color = "red") +
      ylim(ylim[1],ylim[2]) + xlim(25,57) +  
      scale_colour_manual(name="",values=c("red","blue","darkgreen")) + 
      scale_linetype_manual(name="",values=c("dotted","solid","longdash")) + 
      theme(legend.position="bottom")
    
    if(noLegend){
      g <- g + theme(legend.position="none") 
    }
    return(g)
  }
  
  p1 <- doPlot("`Head Wage`", "Var. of Log Head Wages", c(-0.05,0.35), noLegend = F)
  p2 <- doPlot("`Head Hours`", "Var. of Log Head Hours", c(-0.05,0.04))
  p3 <- doPlot("`HH Earnings`", "Var. of Log Household Earnings", c(-0.01,0.35))
  p4 <- doPlot("`HH Earnings Equiv`", "Var. of Log Equiv. Household Earnings", c(-0.05,0.35))
  
  if(toSave){
    pdf('Fig15.pdf')
    grid.arrange(p1,p2,p3,p4)
    dev.off()
  }
  return(grid.arrange(p1,p2,p3,p4))
}