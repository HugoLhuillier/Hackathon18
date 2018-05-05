##################################################
# FIGURE 7: Understanding male earnings inequality
##################################################

doFig7 <- function(data, toSave = F){
  
  data <- data %>% mutate(true_year = year - 1)
  # step 1: clean the dat (simply follows the authors). remove all the entries where hrs or wage is missing (before 1975), 
  # or new_wage / new_hrs after 1975
  fig7_pre <- data %>%
        filter(sex == 1) %>%
        mutate(earnings = ifelse((is.na(hrs) | is.na(wage)) & true_year < 1975, NA, earnings), 
               earnings = ifelse((is.na(new_hrs) | is.na(new_wage)) & true_year >= 1975, NA, earnings),
               hrs      = ifelse((is.na(hrs) | is.na(wage)) & true_year < 1975, NA, hrs),
               hrs      = ifelse((is.na(new_hrs) | is.na(new_wage)) & true_year >= 1975, NA, hrs),
               wage     = ifelse((is.na(hrs) | is.na(wage)) & true_year < 1975, NA, wage),
               wage     = ifelse((is.na(new_hrs) | is.na(new_wage)) & true_year >= 1975, NA, wage),
               new_wage = ifelse((is.na(hrs) | is.na(wage)) & true_year < 1975, NA, new_wage),
               new_wage = ifelse((is.na(new_hrs) | is.na(new_wage)) & true_year >= 1975, NA, new_wage),
               new_hrs  = ifelse((is.na(hrs) | is.na(wage)) & true_year < 1975, NA, new_hrs),
               new_hrs  = ifelse((is.na(new_hrs) | is.na(new_wage)) & true_year >= 1975, NA, new_hrs)) %>%
        filter(!is.na(earnings)) 
  
  # step 2: for each year, compute the 10th, 45th, 55th and 90th percentile (clearly, 0th percentile = min, and 100th percentile = max)
  fig7_perc <- fig7_pre %>%
        group_by(true_year) %>%
        dplyr::summarize(p10 = reldist::wtd.quantile(earnings, 0.1, weight = wgt),
                        p45 = reldist::wtd.quantile(earnings, 0.45, weight = wgt),
                        p55 = reldist::wtd.quantile(earnings, 0.55, weight = wgt),
                        p90 = reldist::wtd.quantile(earnings, 0.9, weight = wgt)) 
      
  # join (by year) the cleaned dataset and the table containing the percentiles. note: care only about the male, therefore remove all sex == 0  
  fig7_pre <- left_join(filter(data, sex == 1), fig7_perc)

  # function that filters for a decile group (based on cond), and then compute the statistics 
  perc_comp <- function(cond){
    # note: same as before, cond will be a string. therefore, need eval(parse(text=cond)) to transform this string into an actual condition
    ti <- fig7_pre %>%
      # group by year, then keep only the decile of interest (e.g. earnings between the 0th and 10th percentile) 
      group_by(true_year) %>%
      filter(eval(parse(text=cond))) %>%
      # compute the statistics for the old and new hours, same for wage, and earnings
      dplyr::summarize(avg_hrs                = Hmisc::wtd.mean(hrs, wgt),
                       avg_new_hrs            = Hmisc::wtd.mean(new_hrs, wgt),
                       avg_earnings           = Hmisc::wtd.mean(earnings, wgt),
                       avg_wage               = Hmisc::wtd.mean(wage, wgt),
                       avg_new_wage           = Hmisc::wtd.mean(new_wage, wgt)) 
    
    # compute the mean bias
    mean_effect <- ti %>%
      filter(true_year > 1974) %>%
      dplyr::summarize(effect_avg_hrs  = mean(avg_new_hrs - avg_hrs),
                       effect_avg_wage = mean(avg_new_wage - avg_wage))
    
    # correct the old measure with the corresponding mean bias
    ti <- ti %>%
      mutate(plt_avg_hrs  = ifelse(true_year < 1975, avg_hrs + mean_effect$effect_avg_hrs, avg_new_hrs), 
             plt_avg_wage = ifelse(true_year < 1975, avg_wage + mean_effect$effect_avg_wage, avg_new_wage),
             plt_avg_hrs  = log(plt_avg_hrs) - log(plt_avg_hrs)[1], 
             plt_avg_earnings = log(avg_earnings) - log(avg_earnings)[1], 
             plt_avg_wage = log(plt_avg_wage) - log(plt_avg_wage)[1]) 

    return(ti)
  }
  
  # keep the 0th-10th decile group
  p10  <- perc_comp("earnings <= p10")
  # keep the 45th-55th decile group
  p55  <- perc_comp("earnings > p45 & earnings < p55")
  # keep the 90th-100th decile group
  p100 <- perc_comp("earnings > p90")
  
  # create a generic function to plot
  #    1. var is the variable to be plotted (e.g. averge wage, average hours etc.)
  #    2. then, info on the plot
  perc_plot <- function(var, title, addLegend = F){
    # as before, need to provide var under the form of a string. therefore, need to use eval(parse(text=var)) so that R understand its an actual variable
    min <- min(select(p10, var))
    max <- max(select(p100, var))
    if(min > -0.8)
      min <- -0.8
    if(max < 0.8)
      max <- 0.55
    g <- ggplot()
    if (addLegend){
      g <- g + 
        geom_line(data = p10, aes(x = true_year, y = eval(parse(text=var)), color = "P0-P10"), size = 1.2) + 
        geom_line(data = p55, aes(x = true_year, y = eval(parse(text=var)), color = "P45-P55"), size = 1.2) + 
        geom_line(data = p100, aes(x = true_year, y = eval(parse(text=var)), color = "P90-P100"), size = 1.2) + 
        scale_color_manual(name="",values=c("blue","darkgreen","red")) + 
        theme(legend.position="bottom")
    } else {
      g <- g + 
        geom_line(data = p10, aes(x = true_year, y = eval(parse(text=var))), color = "blue", size = 1.2) + 
        geom_line(data = p55, aes(x = true_year, y = eval(parse(text=var))), color = "darkgreen", size = 1.2) + 
        geom_line(data = p100, aes(x = true_year, y = eval(parse(text=var))), color = "red", size = 1.2)
    }
    g <- g + ylim(min,max) + 
      labs( title = title, 
            subtitle = "Ranked by Earnings Decile",
            x="Year",
            y="Percentage Change")
    
    return(g)
  }
  
  p1 <- perc_plot("plt_avg_earnings","Male Annual Earnings")
  p2 <- perc_plot("plt_avg_wage","Male Hourly Wages")
  p3 <- perc_plot("plt_avg_hrs","Male Hours Worked", addLegend = T)

  # define a grid to arrange our plot (use the package gridExtra)
  lay <- rbind(c(1,2),
               c(3,3))
  if(toSave){
    pdf('Fig7.pdf')
    grid.arrange(p1, p2, p3, layout_matrix = lay)
    dev.off()
  }
  return(grid.arrange(p1, p2, p3, layout_matrix = lay))
}
