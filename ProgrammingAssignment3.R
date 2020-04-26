## todo:  start with line 165 to create a function that will
## check to see if the outcome is valid (in other words, !is.na())
## right now the error is that I am only looking at the first
## element because the length is > 1.  Need to figure out how to overcome this.


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
packages <- c("here", "rio", "tidyverse")
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

### FUNCTION LOOK HERE ###
###                    ###
### FUNCTION LOOK HERE ###

best <- function(state = as.character(), outcome = as.character()) {
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
    packages <- c("here", "rio", "tidyverse")
    ipak(packages)
  } ## Set it up the script
  
  ## Read outcome data
  if(T){ ## Read outcome data and reshape
  outcome <- import("./data/raw/outcome-of-care-measures.csv", 
                    colClasses = "character")
  ## Select and variables 
  outcome <- outcome %>%
    select("Hospital Name", 
           "City", 
           "State", 
           "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
           "Hospital 30-Day Death (Mortality) Rates from Heart Failure",
           "Hospital 30-Day Death (Mortality) Rates from Pneumonia")
  
  ## Rename variables to be easier to work with
  names(outcome)[4] <- 'heartAttack'
  names(outcome)[5] <- 'heartFailure'
  names(outcome)[6] <- 'pneumonia'
  names(outcome)[1] <- 'hospital'
  names(outcome)[2] <- 'city'
  names(outcome)[3] <- 'state'

  ## Make the DF a TIBBLE for better analysis with TIDYVERSE
  outcome <- as_tibble(outcome)
  
  ## Remove all "Not Available" characters with NAs
  outcome$heartAttack <- na_if(outcome$heartAttack, "Not Available")
  outcome$heartFailure <- na_if(outcome$heartFailure, "Not Available")
  outcome$pneumonia <- na_if(outcome$pneumonia, "Not Available")
  
  ## Make each vector with numbers a numeric
  outcome$heartAttack <- as.numeric(outcome$heartAttack)
  outcome$heartFailure <- as.numeric(outcome$heartFailure)
  outcome$pneumonia <- as.numeric(outcome$pneumonia)
  
  ## Reshape TIBBLE to be long
  outcome <- outcome %>% 
    pivot_longer(
    cols = heartAttack:pneumonia,
    names_to = 'deathCause', 
    values_to = 'deathRate'
  )
  
  ## Rename Factors for ease of reading
  level_key <- c(heartAttack = "Heart Attack", 
                 heartFailure = "Heart Failure", 
                 pneumonia = "Pneumonia")
  outcome$deathCause <- recode(outcome$deathCause, 
                               !!!level_key, 
                               .default = NA_character_)
  outcome <- as.list(outcome)

  #str(outcome)
  
  } ## Read outcome data and reshape
  
  ## Check that state and outcome are valid
  if(T){ ## Check that state is valid
  checkstate = if(state %in% outcome$state){
    print(state)
  } else{
    stop("invalid state")
  }
  }  ## Check that state is valid
  
  if(T){  ## Check that outcome is valid
    
  checkOutcome = if(outcome %in% outcome$deathRate){
    print(deathCause)
  } else{
    stop("invalid cause")
  }
   
  }  ## Check that outcome is valid
  
  
  ## Return hospital name in that state with lowest 30-day death ## rate
  
}

### FUNCTION LOOK UP ###
###                    ###
### FUNCTION LOOK UP ###

##### WHITE BOARD #####
summary(state)
str(outcome)

best("NY", "Heart Failure")


outcome %>% pivot_longer(
  cols = heartAttack:pneumonia,
  names_to = 'deathCause', 
  values_to = 'deathRate'
)


checkOutcome = if(deathRate %in% outcome$deathRate
  
}


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
rm(list=setdiff(ls(), c("clear", "ipak", "is.sorted", "packages", "helpCon")))

## Clear the Console
clear()

## Call Help in the Console, not the `Help` pane.
helpCon()

