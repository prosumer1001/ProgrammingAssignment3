## install.packages(c("dplyr", "rio", "purrr"))
library(dplyr)
library(tidyr)
library(rio)
library(purrr)
#outcome_df <- import("~/Desktop/outcome.RData")
#export(outcome_df, file = "~/Desktop/outcome.csv")

gitHubURL <- "https://raw.githubusercontent.com/prosumer1001/ProgrammingAssignment3/master/outcome.csv"

outcome_df <- import(gitHubURL)

str(outcome_df)

outcome = "heart attack"
num = 20

## Make a vector of state abbreviations
stateAbbreviations <- tibble(State = state.abb); str(stateAbbreviations)
stateAbbreviations <- as_tibble(stateAbbreviations); str(stateAbbreviations)
outcome_df02 <- outcome_df %>% expand(Hospital, 
                                      State, 
                                      deathCause, 
                                      deathRate)
completeOutcome_df <- outcome_df02
export(completeOutcome_df, file = "~/Desktop/completeOutcome_df.RData")
export(completeOutcome_df, file = "~/Desktop/completeOutcome_df.csv")
reportLog <- completeOutcome_df %>% anti_join(outcome_df)
outcome_df03 <- outcome_df02 %>% 
        right_join(right_join(x = arrangeStates,
                              y = stateAbbreviations,
                              by = c("State" = "State"),
                              keep = FALSE)); outcome_df03
## First filter of outcome data to remove NA Causes, filter by outcome call
## then group by state, then arrange in ascending order by deathRate first
## then by Hospital
groupStates <- outcome_df %>%
        filter(!is.na(deathCause)) %>% # Must keep for assignment
        filter(deathCause == outcome) %>% # Filter all Outcome calls
        distinct(.keep_all = TRUE) %>% # Keep all observations
        group_by(State) %>% #
        arrange(deathRate, Hospital); groupStates

## Tell me how many NAs in each vector.
map(rankStates, ~sum(is.na(rankStates$State)))

groupStates %>%
        summarise(n())

rankStates <- groupStates %>%
        mutate(rank = min_rank(deathRate))%>%
        filter(rank == num) %>%
        distinct(.keep_all = TRUE) %>%
        filter(rank(rank) == 1); rankStates

rankStates <- rankStates %>% expand(deathRate,nesting(Hospital, State))
rankStates %>%
        summarise(n())

stateAbbreviations <- tibble(State = state.abb); str(stateAbbreviations)
stateAbbreviations <- as_tibble(stateAbbreviations); str(stateAbbreviations)

rankStates %>% complete(nesting(State, Hospital), deathRate)
completeStates <- rankStates %>%
        right_join(x = arrangeStates,
                  y = stateAbbreviations,
                  by = c("State" = "State"),
                  keep = TRUE); completeStates

completeStates %>%
        summarise(n())       
        
##### HOW TO TOP #####

##  df %>% complete(group, nesting(item_id, item_name))
#> # A tibble: 4 x 5
#>   group item_id item_name value1 value2
#>   <dbl>   <dbl> <chr>      <int>  <int>
#> 1     1       1 a              1      4
#> 2     1       2 b              3      6
#> 3     2       1 a             NA     NA
#> 4     2       2 b              2      5



##### HOW TO BOT #####

arrangeStates <- completeStates %>%
        distinct(.keep_all = TRUE) %>%
        arrange(State) %>%
        select(Hospital, State, deathCause, deathRate); arrangeStates

## Make a vector of state abbreviations
stateAbbreviations <- tibble(State = state.abb); str(stateAbbreviations)
stateAbbreviations <- as_tibble(stateAbbreviations); str(stateAbbreviations)
outcome_df02 <- arrangeStates %>% expand(Hospital, 
                                      State, 
                                      deathCause, 
                                      deathRate)

## Add the NAs in...now I have to figure out how to report these
outcome_df03 <- outcome_df02 %>% 
        right_join(x = arrangeStates,
                              y = stateAbbreviations,
                              by = c("State" = "State"),
                              keep = FALSE); outcome_df03

## Use complete() to make NA values for all states (experiment 1)
completeStates01 <- arrangeStates %>%
        complete(Hospital, nesting(State)); completeStates01

## Use complete() to make NA values for all states (experiment 2)
## experiment == arrangeStates
## all == completeStates
## all <- experiment %>% expand(nesting(name, trt), rep)
completeStates02 <- arrangeStates %>% 
        expand(State = state.abb, 
               Hospital, 
               deathCause, 
               deathRate); completeStates02

## all %>% anti_join(experiment)
## all == completeStates
## experiment == arrangeStates
completeStates02 <- completeStates02 %>%
        anti_join(arrangeStates)

## experiment %>% right_join(all)
## all == completeStates
## experiment == arangeStates
completeStates03 <- completeStates02 %>% 
        right_join(right_join(x = arrangeStates,
                              y = stateAbbre,
                              by = c("State" = "State"),
                              keep = FALSE)); completeStates03

stateArrange <- completeStates03 %>%
        arrange(State, Hospital); stateArrange
                
groupStates02 <- stateArrange %>%
        filter(deathCause == outcome) %>%
        group_by(State) %>%
        select(Hospital, State) %>%
        arrange(State); groupStates02            
                
stateBind <- right_join(x = arrangeStates, 
                        y = state.abb, 
                        by = c("State" = "State"), 
                        keep = FALSE)                
                
     