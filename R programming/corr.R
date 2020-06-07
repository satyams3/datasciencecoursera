corr2 <- function(directory, threshold = 0) {
  # get the list of files in the directory
  files <- list.files(directory, full.names = TRUE)
  
  # create an empty numeric vector to store the correlations
  corr_vector <- numeric()
  
  for (i in 1:332) { # for all observations
    dat <- read.csv(files[i]) # read the observation file
    complete_cases <- sum(complete.cases(dat)) # get the number of complete cases
    if (complete_cases > threshold) { # if complete cases exceed the threshold value
      # then calculate the correlation for that observation
      corr_indv <- cor(dat$sulfate, dat$nitrate, use = "pairwise.complete.obs")
      corr_vector <- c(corr_vector, corr_indv) # add the correlation to the vector
    } 
  }
  # return the vector with correlations for observations exceeding the threshold value
  corr_vector
}