# Load some important functions and packages
## Set it up the script
if(T){  ## Set it up the script
clear <- function(x){
  x = cat("\f")
}
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
helpCon <- function(topic, format=c("text", "html", "latex", "Rd"),lines=NULL, before=NULL, after=NULL) {  
  format=match.arg(format)
  if (!is.character(topic)) topic <- deparse(substitute(topic))
  helpfile = utils:::.getHelpFile(help(topic))
  
  hs <- capture.output(switch(format, 
                              text=tools:::Rd2txt(helpfile),
                              html=tools:::Rd2HTML(helpfile),
                              latex=tools:::Rd2latex(helpfile),
                              Rd=tools:::prepare_Rd(helpfile)
  )
  )
  if(!is.null(lines)) hs <- hs[lines]
  hs <- c(before, hs, after)
  cat(hs, sep="\n")
  invisible(hs)
}
is.sorted <- function(x = NULL) {
  if(all(sort(x, decreasing = FALSE) == x)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
# Install and Load Packages
packages <- c("rio", "tidyverse")
ipak(packages)
} ## Set it up the script

setwd('~/Documents/gitrepos/DataAnalysis/RProgramming_Projects/ProgrammingAssignment3/')


outcome <- import("./data/raw/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
colClasses = "character"

best <- function(state = as.character(), outcome = as.character()){
  
  # Start at section 2 in the assignment instructions
}





## Clean up the working environment
## # Total Environment Wipe
rm(list = ls()) 

## Keep unique functions defined in this script
rm(list=setdiff(ls(), c("clear", "ipak", "is.sorted", "packages", "helpCon")))

## Clear the Console
clear()

## Call Help in the Console, not the `Help` pane.
helpCon()

