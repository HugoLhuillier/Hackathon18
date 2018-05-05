#############################################################
# FIGURE 9:Percentiles of the household earnings distribution
#############################################################

doFig9 <- function(data, toSave = F){
  
  # group by year, then compute the different percentile (therefore for each year)
  # NOTE: there is no NAs in hh_earnings_equiv, so no need to filter(!is.na(hh_earnings_equiv)) before
  fig9_2d <- data %>% 
    mutate(true_year = year - 1) %>%
    group_by(true_year) %>%
    dplyr::summarize(
      p5 = reldist::wtd.quantile(hh_earnings_equiv, 0.05, weight = hh_wgt),
      p10 = reldist::wtd.quantile(hh_earnings_equiv, 0.1, weight = hh_wgt),
      p25 = reldist::wtd.quantile(hh_earnings_equiv, 0.25, weight = hh_wgt),
      p50 = reldist::wtd.quantile(hh_earnings_equiv, 0.5, weight = hh_wgt),
      p75 = reldist::wtd.quantile(hh_earnings_equiv, 0.75, weight = hh_wgt),
      p90 = reldist::wtd.quantile(hh_earnings_equiv, 0.9, weight = hh_wgt),
      p95 = reldist::wtd.quantile(hh_earnings_equiv, 0.95, weight = hh_wgt) ) 
  
  # directly compute in the plot functions the log deviations from the first year
  g <- ggplot(fig9_2d) + 
    geom_line(aes(x = true_year, y = log(p5) - log(p5)[1], color = "P5"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p10) - log(p10)[1], color = "P10"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p25) - log(p25)[1], color = "P25"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p50) - log(p50)[1], color = "P50"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p75) - log(p75)[1], color = "P75"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p90) - log(p90)[1], color = "P90"), size = 1.2) + 
    geom_line(aes(x = true_year, y = log(p95) - log(p95)[1], color = "P95"), size = 1.2) + 
    scale_color_manual(name="",values=c("darkgreen","red","blue","turquoise3","magenta4","yellow3","black")) + 
    labs( title = "Equivalized Household Earnings", 
          x="Year",
          y="Log(normalized to 0 in 1967)")
  
  if(toSave){
    ggsave("Fig9.pdf",g)
  }
  return(g)
}