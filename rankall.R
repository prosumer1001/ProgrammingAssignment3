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
        
        globalClear <- function(x){
                rm(list=ls()[! ls() %in% c("clear", "helpCon")])
        }
        
} ## Set it up the script
outcome_df <- import("~/Documents/gitrepos/DataAnalysis/RProgramming_Projects/ProgrammingAssignment3/outcome.RData")
outcome_df <- outcome_df %>% select(-City)
outcome_df$State <- outcome_df$State
outcome = "heart attack"
num = 20

### FUNCTION LOOK DOWN ###
###                    ###
### FUNCTION LOOK DOWN ###

rankall <- function(outcome, num = "best"){
## Read outcome data
        outcome_df <- import("~/Documents/gitrepos/DataAnalysis/RProgramming_Projects/ProgrammingAssignment3/outcome.RData")
        outcome_df <- outcome_df %>% select(-City)
        outcome_df$State <- outcome_df$State
        ## str(outcome_df)
        
## Check that outcome is valid
        checkCause <- if(outcome %in% outcome_df$deathCause){
                as_tibble(outcome_df) %>%
                        filter(deathCause == outcome)
        } else{
                stop("invalid cause")
        }

## For each state, find the hospital of the given rank
        rankHospital <- outcome_df %>%
                filter(deathCause == outcome) %>% # Filter only Outcome call
                filter(!is.na(deathRate)) %>% # Filter remove NAs from deathRate
                group_by(State) %>% # Group each by the State
                mutate(rank = row_number(deathRate)) %>% # generate a ranking variable
                filter(rank == num)%>% # filter by choosing the rank number
                select(Hospital, State); rankHospital
        
## Return a data frame with the hospital names and the 
        if(num == "best"){
                print(rankHospital)
        } else if(num == "worst"){
                print(rankHospital)
        } else if(num >= 0){
                slice(rankHospital, num)
        }
## (abbreviated) state name.
}

### FUNCTION LOOK ^UP^ ###
###                    ###
### FUNCTION LOOK ^UP^ ###


##### WHITE BOARD #####

rankall("heart attack", 20)


##### END WHITE BOARD #####

globalClear()
