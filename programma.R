library(dplyr)

kordat <- read.table("variants8.txt",
                      header = TRUE,
                      sep = ";",            
                      strip.white = TRUE,   
                      row.names = 1,        
                      stringsAsFactors = FALSE)

kordat[ ,9:ncol(kordat)] <- lapply(kordat[ ,9:ncol(kordat)], as.factor)

factor_summary <- lapply(kordat[ ,9:ncol(kordat)], function(x) table(x))

writeLines("Kopsavilkums par faktoru līmeņiem", "results.txt")
for (factor_summary_item in factor_summary) {
  writeLines(capture.output(factor_summary_item), "results.txt", append = TRUE)
}

sl.by.b <- tapply(kordat$Slope, kordat$b, mean, na.rm = TRUE)

writeLines("Slope grupējot pēc b faktora vērtībām", "results.txt", append = TRUE)
writeLines(capture.output(sl.by.b), "results.txt", append = TRUE)

kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)

sd_by_f <- tapply(kordat$Slope, kordat$f, sd, na.rm = TRUE)

writeLines("Standarta novirze pēc f faktora līmeņiem", "results.txt", append = TRUE)
writeLines(capture.output(sd_by_f), "results.txt", append = TRUE)

prockordat <- subset(kordat, (adj.r.squared > 0.7 & adj.r.squared > 0) | (adj.r.squared < -0.3 & adj.r.squared < 0))

mean_slope <- tapply(prockordat$Slope, prockordat$f, mean, na.rm = TRUE)
mean_intercept <- tapply(prockordat$Intercept, prockordat$f, mean, na.rm = TRUE)
mean_adj_r_squared <- tapply(prockordat$adj.r.squared, prockordat$f, mean, na.rm = TRUE)

writeLines("Vidējie aprēķini pēc f faktora līmeņiem", "results.txt", append = TRUE)
writeLines(capture.output(mean_slope), "results.txt", append = TRUE)
writeLines(capture.output(mean_intercept), "results.txt", append = TRUE)
writeLines(capture.output(mean_adj_r_squared), "results.txt", append = TRUE)

prockordat$Slope <- 1 - 1 / prockordat$Slope

writeLines("Prockordat datu satvars", "results.txt", append = TRUE)
writeLines(capture.output(prockordat), "results.txt", append = TRUE)

library(ggplot2)
ggplot(kordat, aes(x = MAD, y = Average)) + 
  geom_point() + 
  labs(title = "MAD vs Average", x = "MAD", y = "Average") +
  ggsave("scatter.svg")

ggplot(kordat, aes(x = f, y = Intercept)) + 
  geom_boxplot(aes(color = f)) + 
  labs(title = "Intercept pēc f faktora vērtībām") +
  ggsave("boxplot.svg")