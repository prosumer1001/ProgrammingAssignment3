# Load Function and Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# Install and Load Packages
packages <- c("purrr",
              "rio", 
              "tidyverse")
ipak(packages)

is.sorted <- function(x = NULL) {
  if(all(sort(x, decreasing = FALSE) == x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

setwd('~/Documents/gitrepos/DataAnalysis/RProgramming_Project/ProgrammingAssignment3/rprog-data-ProgAssignment3-data')
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
colClasses = "character"

best <- function(state = as.character(), outcome = as.character()){
  
  # Start at section 2 in the assignment instructions
}