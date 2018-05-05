####################################################################################
# FIGURE 5: Education, experience, gender wage premia, and residual wage inequality
####################################################################################

# NOTE: ignore plot of variance of log male wages

doFig5 <- function(data, toSave = F){
  
  # create first some dummy that are going to define our premiums
  # 1. educated: more than 16 years of education 
  # 2. experience: workers aged 45-55 vs. 25-35 
  # 3. gender: male vs. female (already defined)
  data <- data %>% 
  mutate(educated  = ifelse(edu >= 16 & !is.na(edu), 1, 0), 
         age_group = ifelse(age >= 45 & age <= 55, 1, ifelse(age >= 25 & age <= 35, 0, 10)),
         sex   = ifelse(sex == 2, 0, 1), 
         sex   = factor(sex)) %>%
         filter(!is.na(wage))
  
  # function that: 
  #     1. group by group_spe (e.g. educated vs. non-educated, experienced vs. non-experienced)
  #     2. compute the mean wage (old and new definition)
  #     3. correct the old definition with the mean bias 
  #     4. compute the premium: one group divided by the other (e.g. educated / non-educated)
  build_data <- function(group_spe, isSex = F){
    
    # step 1 and 2
    # NOTE: we are going to provide group_spe under the form of a string (e.g. "sex", or "educated"). R has no clue how to interpret this / this
    # is not a variable. by using eval(parse(text=group_spe)), R transforms our string in an actual variable.
    data_spe <- data %>%
      {if(isSex) group_by(., year, sex) else group_by(., year, sex, eval(parse(text=group_spe))) } %>%
      dplyr::summarize(wage     = Hmisc::wtd.mean(wage, weights = wgt),
                       new_wage = Hmisc::wtd.mean(new_wage, weights = wgt))
    
    # step 3: compute the mean bias
    mean_effect <- data_spe %>%
      filter(year > 1975) %>%
      {if(isSex) group_by(., sex) else group_by(., sex, `eval(parse(text = group_spe))`) }%>%
      dplyr::summarize(wage_bias = mean(new_wage - wage))
    
    # step 3: correct the old measure for the mean bias, and put the statistics in a nice format 
    data_spe <- left_join(data_spe, mean_effect) %>%
      {if(isSex) group_by(., year, sex) else group_by(., year, sex, `eval(parse(text = group_spe))`) }%>%
      mutate(plt_wage = ifelse(year < 1976, wage + wage_bias, new_wage)) %>%
      {if(isSex) select(., year, sex, plt_wage) else select(., year, sex, `eval(parse(text = group_spe))`, plt_wage) } %>%
      {if(isSex) spread(., sex, plt_wage) else spread(., `eval(parse(text = group_spe))`, plt_wage) }
      
    # step 4: compute the premium
    if(isSex){
      data_spe <- data_spe %>%
        group_by(year) %>%
        dplyr::summarize(premium = `1` / `0`) 
    } else {
      data_spe <- data_spe %>%
        group_by(year, sex) %>%
        dplyr::summarize(premium = `1` / `0`) 
    }
    return(data_spe)
  }
  
  # use our function to compute the three premiums of interest
  col <- build_data("educated")
  exp <- build_data("age_group")
  sex <- build_data("sex", isSex = T)
  
  # again very lazy: define a generic function that does the plot of each premium
  # data_plot: data that contains the premium; the remaining arguments are only relevant for the plot (e.g. title, the limits of the y-axis etc.)
  doPlot5 <- function(data_plot, title, ylim, isSex = F){
    if(isSex){
      p1 <- ggplot(data_plot, aes(x = year - 1, y = premium)) + 
        geom_line(color = "blue", size = 1.2)
    } else {
      p1 <- ggplot(data_plot, aes(x = year - 1, y = premium, group = factor(relevel(sex, ref = 2)))) + 
        geom_line(aes(color = factor(sex), linetype = factor(sex)), size = 1.2) + 
        scale_colour_manual(name="",values=c("purple","blue"),labels=c("Women","Men")) + 
        scale_linetype_manual(name="",values=c("dashed","solid")) + 
        theme(legend.position="bottom")
    }
     p1 <- p1 + ylim(ylim[1], ylim[2]) + 
      labs( title = title, 
            x="Year",
            y="") + guides(linetype = FALSE)
     return(p1)
  }
  
  p1 <- doPlot5(col, "College Wage Premium", c(1.3,2))
  p2 <- doPlot5(exp, "Experience Wage Premium",c(0.9,1.6))
  p3 <- doPlot5(sex, "Gender Wage Premium", c(1.1,1.8),isSex = T)
      
  # define a grid to arrange our plot (use the package gridExtra)
  lay <- rbind(c(1,2),
               c(3,3))
  if(toSave){
    pdf('Fig5.pdf')
    grid.arrange(p1, p2, p3, layout_matrix = lay)
    dev.off()
  }
  return(grid.arrange(p1, p2, p3, layout_matrix = lay))
}
