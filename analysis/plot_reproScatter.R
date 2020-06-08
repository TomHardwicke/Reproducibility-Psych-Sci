scatterData_p <- data_values %>%
  filter(valueType == 'p') %>%
  filter(reportedValue >= 0.001, # select only p-values above 0.001 for display purposes
         obtainedValue >= 0.001)

numberRemoved <- data_values %>%
  filter(valueType == 'p') %>%
  filter(reportedValue < 0.001 |
         obtainedValue < 0.001) %>%
  nrow()

# scatterData_d <- data_values %>%
#   filter(valueType == 'd')

makeScatter <- function(thisData){
  ggplot(data = thisData, aes(colour = comparisonOutcome, x = reportedValue, y = obtainedValue)) +
    geom_abline(intercept = 0, colour = 'darkgrey') +
    geom_point(alpha = 1, size = 2, shape = 4) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    scale_colour_manual(name = 'reproducibility\noutcome', 
                        values = c(green, blue, purple, 'red'),
                        labels = c("Match", "Minor discrepancy", "Major discrepancy", "Decision error")) + 
    ylab('reanalysis p-value') +
    xlab('reported p-value') +
    coord_fixed()
}

scatter_p <- makeScatter(scatterData_p) +
  scale_x_continuous(trans='log',breaks=c(0.001,0.01,0.05,0.10,0.40,0.70,1),labels=c("0.001","0.01","0.05","0.10","0.40","0.70","1"),limits = c(0.00079, 1),expand=c(0,0)) +
  scale_y_continuous(trans='log',breaks=c(0.001,0.01,0.05,0.10,0.40,0.70,1),labels=c("0.001","0.01","0.05","0.10","0.40","0.70","1"),limits = c(0.00079, 1),expand=c(0,0))

scatterPlot <- scatter_p

# scatter_d <- makeScatter(scatterData_d) +
#   scale_x_continuous(trans='log',breaks=c(0,0.01,0.2, 0.5,1,3,8),labels=c("0","0.01", "0.2","0.5","1","3","8"),limits = c(0.01, 8.2),expand=c(0,0)) +
#   scale_y_continuous(trans='log',breaks=c(0,0.01,0.2, 0.5,1,3,8),labels=c("0","0.01", "0.2","0.5","1","3","8"),limits = c(0.01, 8.2),expand=c(0,0))

# combine plots
#scatterPlots <- ggarrange(scatter_p, scatter_d, labels = c("P-values", "Cohen's d"), hjust = -0.1,
#                       common.legend = TRUE, legend = "bottom")