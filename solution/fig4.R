#############################################
# FIGURE 4: Wage inequality for men and women
#############################################

doFig4 <- function(data, toSave = F){
  
  # we compute the variance of log wage, and the 10th, 50th and 90th percentile (old hour definition) for each year, each sex.
  # then, compute the 90th to 50th ratio, and 50th to 10th ratio
  # NOTE: the reldist package is not very clever. if, in each group, there is only NAs, it fails to compute the quantile.
  # that's why we need to compute two tibbles, one for the old hour measure, and one for the new measure.
  fig4_o <- data %>% 
    group_by(true_year, sex) %>%
    dplyr::summarize(p90       = reldist::wtd.quantile(wage, 0.9, weight = wgt, na.rm = T), 
                     p50       = reldist::wtd.quantile(wage, 0.5, weight = wgt, na.rm = T), 
                     p10       = reldist::wtd.quantile(wage, 0.1, weight = wgt, na.rm = T), 
                     var_lwage = Hmisc::wtd.var(log(wage), weights = wgt, na.rm = T)) %>%
    group_by(true_year, sex) %>% 
    mutate(p90_p50 = p90 / p50, 
           p50_p10 = p50 / p10)
  
  # we compute the variance of log wage, and the 10th, 50th and 90th percentile (new hour definition) for each year, each sex
  # then, compute the 90th to 50th ratio, and 50th to 10th ratio
  fig4_n <- data %>% 
    filter(true_year > 1974) %>%
    group_by(true_year, sex) %>%
    dplyr::summarize(p90_n       = reldist::wtd.quantile(new_wage, 0.9, weight = wgt, na.rm = T), 
                     p50_n       = reldist::wtd.quantile(new_wage, 0.5, weight = wgt, na.rm = T), 
                     p10_n       = reldist::wtd.quantile(new_wage, 0.1, weight = wgt, na.rm = T), 
                     var_lwage_n = Hmisc::wtd.var(log(new_wage), weights = wgt, na.rm = T)) %>%
    group_by(true_year, sex) %>% 
    mutate(p90_p50_n = p90_n / p50_n, 
           p50_p10_n = p50_n / p10_n)
  
  # compute the gini coefficient (old measure), for each year, each sex
  # NOTE #1: same remark as before for the NAs
  # NOTE #2: the gini function in reldist does not have a na.rm option; therefore have to remove ex ante all the NAs
  fig4_ginio <- data %>%
    filter(!is.na(wage)) %>%
    group_by(true_year, sex) %>%
    dplyr::summarize(gini = reldist::gini(wage, weight = wgt))
  
  # compute the gini coefficient (new measure), for each year, each sex
  fig4_ginin <- data %>%
    filter(true_year > 1974) %>%
    filter(!is.na(new_wage)) %>%
    group_by(true_year, sex) %>%
    dplyr::summarize(gini_n = reldist::gini(new_wage, weight = wgt))
  
  # join all our summarized datasets by true_year and sex
  fig4 <- full_join(left_join(fig4_o, fig4_ginio), left_join(fig4_n, fig4_ginin)) %>% 
    select(-p90_n, -p90, -p50_n, -p50, -p10_n, -p10)
  
  # compute the bias between the old and the new measures, by sex
  mean_effect <- fig4 %>%
    filter(true_year > 1974) %>%
    group_by(sex) %>%
    dplyr::summarize(eff_var_lwage = mean(var_lwage_n - var_lwage), 
                     eff_gini      = mean(gini_n - gini), 
                     eff_p90_p50   = mean(p90_p50_n - p90_p50), 
                     eff_p50_p10   = mean(p50_p10_n - p50_p10))
  
  # combine the summarized dataset with the table containing all the biases (easier to then compute the bias-corrected statistics)
  # if before 1975: bias corrected statistics. after: statistics based on the new measure
  fig4 <- full_join(fig4,mean_effect) %>%
    mutate(plt_var_lwage = ifelse(true_year < 1975, var_lwage + eff_var_lwage, var_lwage_n), 
           plt_gini      = ifelse(true_year < 1975, gini + eff_gini, gini_n),
           plt_p90_p50   = ifelse(true_year < 1975, p90_p50 + eff_p90_p50, p90_p50_n),
           plt_p50_p10   = ifelse(true_year < 1975, p50_p10 + eff_p50_p10, p50_p10_n))

  # plot the final statistics, distinguishing by sex
  p1 <- ggplot(fig4, aes(x = true_year, y = plt_var_lwage, group = factor(sex))) + 
    geom_line(aes(color = factor(sex), linetype = factor(sex)), size = 1.2) + 
    labs( title = "Variance of Log Hourly Wages", 
          x="Year",
          y="") + guides(linetype = FALSE) + 
    scale_colour_manual(name = "", values=c("blue","purple"), labels = c("Men","Women")) +
    theme(legend.position="bottom")
  
  p2 <- ggplot(fig4, aes(x = true_year, y = plt_gini, group = factor(sex))) + 
    geom_line(aes(color = factor(sex), linetype = factor(sex)), size = 1.2) + 
    labs( title = "Gini Coefficient of Hourly Wages", 
          x="Year",
          y="") + 
    scale_colour_manual(name = "", values=c("blue","purple"), labels = c("Men","Women")) + 
    guides(linetype = FALSE, color = FALSE) 
  
  p3 <- ggplot(fig4, aes(x = true_year, y = p50_p10, group = factor(sex)), size = 1.2) + 
    geom_line(aes(color = factor(sex), linetype = factor(sex)), size = 1.2) + 
    labs( title = "P50-P10 Ratio of Hourly Wages", 
          x="Year",
          y="") + 
    scale_colour_manual(name = "", values=c("blue","purple"), labels = c("Men","Women")) + 
    guides(linetype = FALSE, color = FALSE) 
  
  p4 <- ggplot(fig4, aes(x = true_year, y = p90_p50, group = factor(sex)), size = 1.2) + 
    geom_line(aes(color = factor(sex), linetype = factor(sex)), size = 1.2) + 
    labs( title = "P90-P50 Ratio of Hourly Wages", 
          x="Year",
          y="") + 
    scale_colour_manual(name = "", values=c("blue","purple"), labels = c("Men","Women")) + 
    guides(linetype = FALSE, color = FALSE) 
  
  if(toSave){
    pdf('Fig4.pdf')
    grid.arrange(p1,p2,p3,p4)
    dev.off()
  }
  return(grid.arrange(p1,p2,p3,p4))
}
