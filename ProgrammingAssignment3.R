# Load some important functions and packages
## Set it up the script
if(T){  ## Set it up the script
## Create a function to clear the console easily
  clear <- function(x){
  x = cat("\f")
}

## Package names
packages <- c("dplyr", 
              "here", 
              "janitor", 
              "rio", 
              "tidyr")

## Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
## Packages Loading
invisible(lapply(packages, library, character.only = TRUE))

## Create a function to read package help in the console
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
} ## Set it up the script

setwd('~/Documents/gitrepos/DataAnalysis/RProgramming_Projects/ProgrammingAssignment3/')

outcome_df <- import("./data/raw/outcome-of-care-measures.csv", colClasses = "character")

list.of.packages <- c("here", "rio", "dplyr", "tidyr", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
library("here")
library("rio")
library("dplyr")
library("tidyr")
library("janitor")

head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
colClasses = "character"

### FUNCTION LOOK HERE ###
###                    ###
### FUNCTION LOOK HERE ###

best <- function(state = as.character(), outcome = as.character()) {
  ## Set up the script

  ## Read outcome data
  if(T){ ## Read outcome data and reshape
    outcome_df <- import("./data/raw/outcome-of-care-measures.csv", 
                         colClasses = "character")
    ## Select and variables 
    outcome_df <- outcome_df %>%
      select("Hospital Name", 
             "City", 
             "State", 
             "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
             "Hospital 30-Day Death (Mortality) Rates from Heart Failure",
             "Hospital 30-Day Death (Mortality) Rates from Pneumonia")
    
    ## Rename variables to be easier to work with
    names(outcome_df)[4] <- 'heartAttack'
    names(outcome_df)[5] <- 'heartFailure'
    names(outcome_df)[6] <- 'pneumonia'
    names(outcome_df)[1] <- 'Hospital'
    names(outcome_df)[2] <- 'City'
    names(outcome_df)[3] <- 'State'
    
    ## Make the DF a TIBBLE for better analysis with TIDYVERSE
    outcome_df <- as_tibble(outcome_df)
    
    ## Remove all "Not Available" characters with NAs
    outcome_df$heartAttack <- na_if(outcome_df$heartAttack, "Not Available")
    outcome_df$heartFailure <- na_if(outcome_df$heartFailure, "Not Available")
    outcome_df$pneumonia <- na_if(outcome_df$pneumonia, "Not Available")
    
    ## Make each vector with numbers a numeric
    outcome_df$heartAttack <- as.numeric(outcome_df$heartAttack)
    outcome_df$heartFailure <- as.numeric(outcome_df$heartFailure)
    outcome_df$pneumonia <- as.numeric(outcome_df$pneumonia)
    
    ## Reshape TIBBLE to be long
    outcome_df <- outcome_df %>% 
      pivot_longer(
        cols = heartAttack:pneumonia,
        names_to = 'deathCause', 
        values_to = 'deathRate'
      )
    
    ## Rename Factors for ease of reading
    level_key <- c(heartAttack = "heart attack", 
                   heartFailure = "heart failure", 
                   pneumonia = "pneumonia")
    outcome_df$deathCause <- recode(outcome_df$deathCause, 
                                    !!!level_key, 
                                    .default = NA_character_)
    outcome_df <- as.list(outcome_df)
    
    #str(outcome_df)
    
  } ## Read outcome data and reshape
  
  ## Check that state and outcome are valid
  if(T){ ## Check that state is valid
  checkState <- if(state %in% outcome_df$State){
    as_tibble(outcome_df) %>%
      filter(State == state)
  } else{
    stop("invalid state")
  }
  }  ## Check that state is valid
  

  
  if(T){  ## Check that outcome is valid
  checkCause <- if(outcome %in% outcome_df$deathCause){
    as_tibble(outcome_df) %>%
      filter(deathCause == outcome) %>%
      filter(!is.na(deathRate)) %>%
      filter(State == state) %>%
      arrange(deathRate) %>%
      select(Hospital)
  } else{
    stop("invalid cause")
  }
  }  ## Check that outcome is valid
  
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  print(head(checkCause, 1))
}

### FUNCTION LOOK UP ###
###                    ###
### FUNCTION LOOK UP ###

##### WHITE BOARD #####

best("TX", "heart failure")

##### END WHITE BOARD #####


##### SOME GOOD DESCRIPTIVE STATS CODE #####
## If You need NA count of all
table(is.na(outcome)) 

## If you need NA count Column wise 
sapply(outcome, function(outcome) sum(is.na(outcome))) 

## If you need NA count Row wise
rowSums(is.na(outcome))

## WITH TIDYVERSE


##### END GOOD DS CODE #####

## Clean up the working environment
## # WIPE GLOBAL ENVIRONMENT
# rm(list = ls())  # DO NOT USE UNLESS YOU WANT TO WIPE THE GLOBAL ENVIRONMENT

## Keep unique functions defined in this script
rm(list=setdiff(ls(), c("clear", "is.sorted", 
                        "installed_packages", 
                        "packages", 
                        "helpCon",
                        "is.sorted")))

## Clear the Console
clear()

## Call Help in the Console, not the `Help` pane.
helpCon()

## Debug


