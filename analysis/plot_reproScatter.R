# This script will build a scatter plot for original vs reanalysis values

## extract p-value data
scatterData_p <- data_values %>%
  filter(valueType == 'p') %>%
  filter(reportedValue >= 0.001, # select only p-values above 0.001 for display purposes
         obtainedValue >= 0.001)

## identify the number of values below 0.001 that are not being shown for display purposes
numberRemoved <- data_values %>%
  filter(valueType == 'p') %>%
  filter(reportedValue < 0.001 |
         obtainedValue < 0.001) %>%
  nrow()

## function containing ggplot code for the scatter plot
makeScatter <- function(thisData){
  ggplot(data = thisData, aes(x = reportedValue, y = obtainedValue, shape = comparisonOutcome)) +
    geom_abline(intercept = 0, colour = 'black') + # plot reference line illustrating perfect match
    geom_point(alpha = 1, size = 2.5) + # plot data points
    geom_vline(data = NULL, aes(xintercept = 0.05), colour = "black", linetype = "dashed") + # plot vertical reference line for .05 threshold
    geom_hline(data = NULL, aes(yintercept = 0.05), colour = "black", linetype = "dashed") + # plot horizontal reference line for .05 threshold
    theme_bw(base_size = 12) + # apply aesthetic theme
    theme(axis.text = element_text(colour = 'black'), # further aesthetic adjustments to theme
          panel.grid.minor = element_blank(), # remove major gridlines
          panel.grid.major = element_blank(), # remove minor gridlines
          panel.background = element_rect(fill = "white", colour = "black", size = 1)) + # plot background (white) and border (black)
    scale_shape_manual(name = 'reproducibility\noutcome', # set name in legend
                       values = c(4,1,2,8), # choose shapes 
                       labels = c("Match", "Minor discrepancy", "Major discrepancy", "Decision error")) + # shape labels
    ylab('reanalysis p-value') + # add y axis label
    xlab('original p-value') + # add x axis label
    coord_fixed() # set aspect ratio of plot
}

scatterPlot <- makeScatter(scatterData_p) + # build the scatter plot
  scale_x_continuous(trans='log',breaks=c(0.001,0.01,0.05,0.10,0.40,0.70,1),labels=c("0.001","0.01","0.05","0.10","0.40","0.70","1"),limits = c(0.00079, 1),expand=c(0,0)) + # make x axis log scale
  scale_y_continuous(trans='log',breaks=c(0.001,0.01,0.05,0.10,0.40,0.70,1),labels=c("0.001","0.01","0.05","0.10","0.40","0.70","1"),limits = c(0.00079, 1),expand=c(0,0)) # make y axis log scale

# 
# scatterData_d <- data_values %>%
#   filter(valueType %in% c('d', 'pes', 'phi'))
# 
# scatter_d <- makeScatter(scatterData_d) +
#   scale_x_continuous(trans='log',breaks=c(0,0.01,0.2,0.5,1,3,8),labels=c("0","0.01", "0.2","0.5","1","3","8"),limits = c(0.01, 8.2),expand=c(0,0)) +
#   scale_y_continuous(trans='log',breaks=c(0,0.01,0.2,0.5,1,3,8),labels=c("0","0.01", "0.2","0.5","1","3","8"),limits = c(0.01, 8.2),expand=c(0,0))

# combine plots
#scatterPlots <- ggarrange(scatter_p, scatter_d, labels = c("P-values", "Cohen's d"), hjust = -0.1,
#                       common.legend = TRUE, legend = "bottom")