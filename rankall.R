## rankall.R
## Load some important functions and packages
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

rankall <- function(outcome, num = "best"){
## Read outcome data
        outcome_df <- import("~/Desktop/outcome.RData")
        ## str(outcome_df)

## Check state and outcome are valid
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
                        arrange(deathRate, State, Hospital) %>%
                        select(Hospital)
        } else{
                stop("invalid cause")
        }

## For each state, find the hospital of the given rank
        rankHospital <- outcome_df %>%
                filter(State == state) %>%
                filter(deathCause == outcome) %>%
                arrange(deathRate, State, deathCause, Hospital) %>%
                select(Hospital, State)
        
        if(num == "best"){
                head(rankHospital, 1)
        } else if(num == "worst"){
                tail(rankHospital, 1)
        } else if(num >= 0){
                slice(rankHospital, num)
        }
        

## Return a data frame with the hospital names and the 
## (abbreviated) state name.
}

### FUNCTION LOOK ^UP^ ###
###                    ###
### FUNCTION LOOK ^UP^ ###


##### WHITE BOARD #####

rankall("heart attack", 15)

##### END WHITE BOARD #####
