library(readxl)
library(meta)


downward_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 2)

#A.PR
#1.Downward mobility------------------------------------------------------------------
downward_data <- downward_data[7:12, ]
downward_data$DPN <- formatC(downward_data$DPN, format = "d", big.mark = ",")
downward_data$N <- formatC(downward_data$N, format = "d", big.mark = ",")
downward_data$`DPN (Stably Low)` <- formatC(downward_data$`DPN (Stably Low)`, format = "d", big.mark = ",")
downward_data$`Total (Stably Low)` <- formatC(downward_data$`Total (Stably Low)`, format = "d", big.mark = ",")
downward_data$logOR <- log(downward_data$OR)
downward_data$SE <- (log(downward_data$CI_upper) - log(downward_data$CI_lower)) / (2 * qnorm(0.975))
num_studies <- nrow(downward_data)
colors <- c("darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey")

library(meta)
meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = downward_data,
  common = TRUE,    
  random = TRUE,
  sm = "OR",
  backtransf = TRUE
)

meta_result$fixed <- meta_result$common
meta_result$common <- NULL

jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\down_pr.jpg",
  res = 500,
  width = 14,
  height = 6,
  units = "in"
)

plot.new()

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"),
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.35, 1.35),
  at = c(0.35, 0.50, 0.8, 1.35),
  fixed = TRUE,
  random = TRUE
)

legend(
  x = 1.5, y = 7,
  legend = c("Fixed Effect Model", "Random Effects Model"),
  col = c("darkgrey", "darkgrey"),
  lty = 1,
  lwd = 2,
  bty = "n",
  cex = 0.8
)

dev.off()


#2.Stable middle------------------------------------------------------------------
middle_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 3)
middle_data <- middle_data[7:12, ]
middle_data$DPN <- formatC(middle_data$DPN, format = "d", big.mark = ",")
middle_data$N <- formatC(middle_data$N, format = "d", big.mark = ",")
middle_data$`DPN (Stably Low)` <- formatC(middle_data$`DPN (Stably Low)`, format = "d", big.mark = ",")
middle_data$`Total (Stably Low)` <- formatC(middle_data$`Total (Stably Low)`, format = "d", big.mark = ",")
middle_data$logOR <- log(middle_data$OR)
middle_data$SE <- (log(middle_data$CI_upper) - log(middle_data$CI_lower)) / (2 * qnorm(0.975))

library(meta)
meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = middle_data,
  common = TRUE,   
  random = TRUE,   
  sm = "OR",
  backtransf = TRUE
)

jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\middle_pr.jpg",
  res = 500,
  width = 14,  
  height = 6,
  units = "in"
)

plot.new()

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"), 
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.35, 1.35),
  at = c(0.35, 0.50, 0.8, 1.35),
  common = TRUE,
  random = TRUE
)

legend(
  x = 1.5, y = 7,  
  legend = c("Fixed Effect Model", "Random Effects Model"),
  col = c("darkgrey", "darkgrey"),
  lty = 1,
  lwd = 2,
  bty = "n",
  cex = 0.8
)

dev.off()

#3.upward mobility------------------------------------------------------------------
upward_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 4)
upward_data <- upward_data[7:12, ]
upward_data$DPN <- formatC(upward_data$DPN, format = "d", big.mark = ",")
upward_data$N <- formatC(upward_data$N, format = "d", big.mark = ",")
upward_data$`DPN (Stably Low)` <- formatC(upward_data$`DPN (Stably Low)`, format = "d", big.mark = ",")
upward_data$`Total (Stably Low)` <- formatC(upward_data$`Total (Stably Low)`, format = "d", big.mark = ",")
upward_data$logOR <- log(upward_data$OR)
upward_data$SE <- (log(upward_data$CI_upper) - log(upward_data$CI_lower)) / (2 * qnorm(0.975))

library(meta)
meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = upward_data,
  common = TRUE,   
  random = TRUE,   
  sm = "OR",
  backtransf = TRUE
)

jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\upward_pr.jpg",
  res = 500,
  width = 14,  
  height = 6,
  units = "in"
)

plot.new()

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"), 
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.35, 1.35),
  at = c(0.35, 0.50, 0.8, 1.35),
  common = TRUE,
  random = TRUE
)

legend(
  x = 1.5, y = 7,  
  legend = c("Fixed Effect Model", "Random Effects Model"),
  col = c("darkgrey", "black"),
  lty = 1,
  lwd = 2,
  bty = "n",
  cex = 0.8
)

dev.off()

#4.stably high------------------------------------------------------------------
high_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 5)
high_data <- high_data[7:12, ]
high_data$DPN <- formatC(high_data$DPN, format = "d", big.mark = ",")
high_data$N <- formatC(high_data$N, format = "d", big.mark = ",")
high_data$`DPN (Stably Low)` <- formatC(high_data$`DPN (Stably Low)`, format = "d", big.mark = ",")
high_data$`Total (Stably Low)` <- formatC(high_data$`Total (Stably Low)`, format = "d", big.mark = ",")
high_data$logOR <- log(high_data$OR)
high_data$SE <- (log(high_data$CI_upper) - log(high_data$CI_lower)) / (2 * qnorm(0.975))

library(meta)
meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = high_data,
  common = TRUE,   
  random = TRUE,   
  sm = "OR",
  backtransf = TRUE
)

jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\high_pr.jpg",
  res = 500,
  width = 14,  
  height = 6,
  units = "in"
)

plot.new()

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"), 
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.35, 1.35),
  at = c(0.35, 0.50, 0.8, 1.35),
  common = TRUE,
  random = TRUE
)

legend(
  x = 1.5, y = 7,  
  legend = c("Fixed Effect Model", "Random Effects Model"),
  col = c("darkgrey", "darkgrey"),
  lty = 1,
  lwd = 2,
  bty = "n",
  cex = 0.8
)

dev.off()

#B.Harmonized
#1.Downward mobility------------------------------------------------------------------
downward_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 2)
downward_data <- downward_data[1:6, ]
downward_data$logOR <- (downward_data$OR)
downward_data$SE <- (log(downward_data$CI_upper) - log(downward_data$CI_lower)) / (2 * qnorm(0.975))

downward_data$DPN <- formatC(downward_data$DPN, format = "d", big.mark = ",")
downward_data$N <- formatC(downward_data$N, format = "d", big.mark = ",")
downward_data$`DPN (Stably Low)` <- formatC(downward_data$`DPN (Stably Low)`, format = "d", big.mark = ",")
downward_data$`Total (Stably Low)` <- formatC(downward_data$`Total (Stably Low)`, format = "d", big.mark = ",")
downward_data$logOR <- log(downward_data$OR)
downward_data$SE <- (log(downward_data$CI_upper) - log(downward_data$CI_lower)) / (2 * qnorm(0.975))
num_studies <- nrow(downward_data)

meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = downward_data,
  common = TRUE,
  random = TRUE,
  sm = "OR",
  backtransf = TRUE
)


jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\downward_H.jpg",
  res = 500,
  width = 14,
  height = 6,
  units = "in"
)

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"),
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.20, 2.50),
  at = c(0.20, 0.50, 0.8, 2.50),
  fontsize = 10
)


dev.off()


#2.Stable middle------------------------------------------------------------------
middle_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 3)
middle_data <- middle_data[1:6, ]
middle_data$logOR <- log(middle_data$OR)
middle_data$SE <- (log(middle_data$CI_upper) - log(middle_data$CI_lower)) / (2 * qnorm(0.975))

meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = middle_data,
  common = TRUE,
  random = TRUE,
  sm = "OR",
  backtransf = TRUE
)


jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\middle_H.jpg",
  res = 500,
  width = 14,
  height = 6,
  units = "in"
)

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"),
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.20,2.50),
  at = c(0.20, 0.50, 0.8, 2.50),
  fontsize = 10
)


dev.off()

#3.upward mobility------------------------------------------------------------------
upward_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 4)
upward_data <- upward_data[1:6, ]
upward_data$logOR <- log(upward_data$OR)
upward_data$SE <- (log(upward_data$CI_upper) - log(upward_data$CI_lower)) / (2 * qnorm(0.975))

meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = upward_data,
  common = TRUE,
  random = TRUE,
  sm = "OR",
  backtransf = TRUE
)


jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\upward_H.jpg",
  res = 500,
  width = 14,
  height = 6,
  units = "in"
)

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"),
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.20, 2.50),
  at = c(0.20, 0.50, 0.8, 2.50),
  fontsize = 10,
  main = "Meta-Analysis of Downward Educational Mobility and Depression",
  cex.main = 1.2
)

dev.off()

#4.stably high------------------------------------------------------------------
high_data <- read_xlsx("D:/R project/Educational mobility and depression/meta_analysis/Model_results.xlsx", sheet = 5)
high_data <- high_data[1:6, ]
high_data$logOR <- log(high_data$OR)
high_data$SE <- (log(high_data$CI_upper) - log(high_data$CI_lower)) / (2 * qnorm(0.975))


meta_result <- metagen(
  TE = logOR,
  seTE = SE,
  studlab = Study,
  data = high_data,
  common = TRUE,
  random = TRUE,
  sm = "OR",
  backtransf = TRUE
)


jpeg(
  filename = "D:\\R project\\Educational mobility and depression\\meta_analysis\\high_H.jpg",
  res = 500,
  width = 14,
  height = 6,
  units = "in"
)

forest(
  meta_result,
  xlab = "Odds Ratio (95% CI)",
  leftcols = c("studlab", "DPN", "N", "DPN (Stably Low)", "Total (Stably Low)"),
  leftlabs = c("Study", "DPN", "Total", "DPN(ref)", "Total(ref)"),
  col.square = colors,
  col.square.lines = "black",
  col.diamond = "darkgrey",
  col.diamond.lines = "black",
  col.inside = "black",
  xlim = c(0.20, 2.50),
  at = c(0.20, 0.50, 0.8, 2.50),
  fontsize = 10
)


dev.off()

