## install.packages(c("dplyr", "rio"))
library(dplyr)
library(tidyr)
library(rio)
#outcome_df <- import("~/Desktop/outcome.RData")
#export(outcome_df, file = "~/Desktop/outcome.csv")

gitHubURL <- "https://raw.githubusercontent.com/prosumer1001/ProgrammingAssignment3/master/outcome.csv"

outcome_df <- import(gitHubURL)

str(outcome_df)

outcome = "heart attack"
num = 20

## Make a vector of state abbreviations
stateAbbreviations <- tibble(State = state.abb); str(stateAbbreviations)

## First filter of outcome data to remove NA Causes, filter by outcome call
## then group by state, then arrange in ascending order by deathRate first
## then by Hospital
groupStates <- outcome_df %>%
        filter(!is.na(deathCause)) %>% # Must keep for assignment
        filter(deathCause == outcome) %>%
        group_by(State) %>%
        arrange(deathRate, Hospital); groupStates

## 
expand(groupStates, Hospitals, State)

stateComplete <- arrangeStates %>%
        complete(Hospital, nesting(State)); stateComplete

## DELETE LATER
#filterStates <- groupStates %>%
#        filter(State == "AL"); filterStates

rankStates <- groupStates %>%
        mutate(rank = min_rank(deathRate))%>%
        filter(rank == num) %>%
        filter(rank(rank) == 1); rankStates

arrangeStates <- rankStates %>%
        arrange(State) %>%
        select(Hospital, State); arrangeStates

stateBind <- right_join(x = arrangeStates, 
                       y = stateAbbreviations, 
                       by = c("State" = "State"), 
                       keep = FALSE) 

stateArrange <- stateBind %>%
        arrange(State, Hospital); stateArrange


groupStates %>% anti_join(Hospital)     


                arrange(deathRate, State, Hospital) %>%
        select(Hospital, State, deathCause, deathRate); head(rankHospital, 20)
