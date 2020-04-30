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




### FUNCTION LOOK DOWN ###
###                    ###
### FUNCTION LOOK DOWN ###

rankhospital <- function(state, outcome, num = "best"){
## Read outcome data and reshape
outcome_df <- import("~/Desktop/outcome.RData")
## Read outcome data and reshape       

checkState <- if(state %in% outcome_df$State){
        as_tibble(outcome_df) %>%
                filter(State == state)
} else{
        stop("invalid state")
}

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

rankReturn <- if(num == ){
        outcome_df %>%
                filter(!is.na(deathRate)) %>%
                filter(State == state) %>%
                filter(deathCause == outcome) %>%
                arrange(deathRate, Hospital) %>%
                slice(num)
} else if(num == as.character("best")){
                outcome_df %>%
                filter(!is.na(deathRate)) %>%
                filter(State == state) %>%
                filter(deathCause == outcome) %>%
                arrange(deathRate) %>%
                select(Hospital) %>%
                head(1)
} else if(num == as.character("worst")){
        as_tibble(outcome_df) %>%
                filter(!is.na(deathRate)) %>%
                filter(State == state) %>%
                filter(deathCause == outcome) %>%
                arrange(deathRate) %>%
                select(Hospital) %>%
                tail(1)
} else{
        stop("ifelse does not work")
}


print(rankReturn)
        
} ## Function END

### FUNCTION LOOK ^UP^ ###
###                    ###
### FUNCTION LOOK ^UP^ ###


##### WHITE BOARD #####

rankhospital("MD", "heart attack", "worst")

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
rm(list=setdiff(ls(), c("clear", "is.sorted", "packages", "helpCon")))
rm(list = ls())
## Clear the Console
clear()





