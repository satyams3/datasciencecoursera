complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names = TRUE)
  frame <- data.frame()
  for (i in id) {
    dat <- read.csv(files[i])
    complete_cases <- sum(complete.cases(dat))
    entry <- c(i, complete_cases)
    frame <- rbind(frame, entry)
  }
  colnames(frame)<- c("id","nobs")
  frame
}