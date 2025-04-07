install.packages(c("dplyr", "ggplot2"))
install.packages("devtools")

kordat <- read.table("variants8.txt",
                     header = TRUE,
                      sep = ";",       
                      dec = ",",       
                      strip.white = TRUE,
                      row.names = 1,
                      stringsAsFactors = FALSE)
 kordat[] <- lapply(kordat, function(x) if(is.character(x)) trimws(x) else x)

  kordat[ , 9:ncol(kordat)] <- lapply(kordat[ , 9:ncol(kordat)], as.factor)

  sink("results.txt")
 
  cat("Līmeņu sadalījums pa kolonnām no 9. un uz augšu:\n")
  for (col in names(kordat)[9:ncol(kordat)]) {
      cat("\nKolonna:", col, "\n")
      print(summary(kordat[[col]]))
  }
  sl.by.b <- split(kordat$Slope, kordat$b)
  cat("\nSadalītās 'Slope' vērtības pēc b faktora:\n")
  print(sl.by.b)
  kordat$Slope <- as.numeric(as.character(kordat$Slope))
  kordat$Intercept <- as.numeric(as.character(kordat$Intercept))
  kordat$adj.r.squared <- as.numeric(as.character(kordat$adj.r.squared))
  kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)
  cat("\nStandartnovirze pa f faktora līmeņiem:\n")
  library(dplyr)
  std_by_f <- kordat %>%
  group_by(f) %>%
  numeric_cols <- sapply(kordat, is.numeric)
  std_by_f <- kordat %>%
  group_by(f) %>%
  summarise(across(names(kordat)[numeric_cols], sd, na.rm = TRUE))
  print(std_by_f)
  adj_vals <- kordat$adj.r.squared
  if (all(adj_vals >= 0, na.rm = TRUE)) {
      prockordat <- kordat[kordat$adj.r.squared > 0.7, ]
  } else {
      prockordat <- kordat[kordat$adj.r.squared > -0.3, ]
  }
  prockordat$Slope <- 1 - 1 / prockordat$Slope
  cat("\nDatu satvars prockordat (filtrēts un pārrēķināts):\n")
  print(prockordat)
  sink()
  library(ggplot2)
  svg("scatter.svg")
  ggplot(kordat, aes(x = MAD, y = Average)) +
      geom_point() +
      theme_minimal() +
      labs(title = "Izkliedes grafiks: MAD vs Average", x = "MAD", y = "Average")
  dev.off()
  svg("boxplot.svg")
  ggplot(kordat, aes(x = f, y = Intercept, fill = f)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Kastu grafiks: Intercept pēc f faktora", x = "f faktors", y = "Intercept")
