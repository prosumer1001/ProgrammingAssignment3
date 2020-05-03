## Whiteboard #2

## install.packages(c("dplyr", "rio", "purrr"))
library(dplyr)
library(tidyr)
library(rio)
library(purrr)

gitHubURL <- "https://raw.githubusercontent.com/prosumer1001/ProgrammingAssignment3/master/outcome.csv"

outcome_df <- import(gitHubURL)
outcome_df <- outcome_df %>% select(-City)
str(outcome_df)

outcome = "heart attack"
num = 20

rankState <- outcome_df %>%
        filter(!is.na(deathCause)) %>% # Remove all NA in outcome
        filter(deathCause == outcome) %>% # Include only outcome
        group_by(State) %>% # Group each by the State
        mutate(rank = row_number(deathRate)) %>% # generate a ranking variable
        #filter(rank == num)%>% # filter by choosing the rank number
        select(Hospital, State, deathCause, deathRate, rank); rankState

rankState %>%
        group_by(State) %>%
        filter(rank == num) %>%
        filter(rank(rank) == 1); rankState
# rep(1:4, each = 2)

50*370
State <- rep(state.abb, each = 370)
rank <- rep(1:50, each = 370)
rank <- as.numeric(rank)
deathCause <- rep(NA, each = 18500)
deathRate <- rep(99, each = 18500)
Hospital <- rep(99, each = 18500)
Hospital <- as.numeric(Hospital); str(Hospital)
full_df <- cbind.data.frame(Hospital, State, deathCause, deathRate, rank)
full_df$State
str(rankState)
full_df <- rbind.data.frame(rankState, full_df)
str(full_df)
summary(full_df)
# full_df <- new_df[match(unique(new_df$id),new_df$id),]

full_df %>%
        group_by(State, Hospital) %>%
        filter(Hospital >= 0) %>%
        select(Hospital, State, deathCause, deathRate, rank) %>%
        summarise(n())

rankState %>%
        group_by(State, Hospital) %>%
        filter(Hospital >= 0) %>%
        select(Hospital, State, deathCause, deathRate, rank) %>%
        summarise(n())

map(full_df, ~sum(is.na(.)))

full_df %>% 
        filter(State == "MD") %>% # Include the rank values
        select(Hospital, State, deathCause, deathRate, rank)

length(unique(full_df$State))
full_df$State

full_df <- as.data.frame(full_df)
subset(full_df, rank == 20, select = c(Hospital, State, deathCause, deathRate, rank))


if(F){
## How many hospitals does each state have? (Top)
window <- rankState %>%
        group_by(State) %>%
        summarise(rank_n = n())
View(window)

## Texas has the most hospitals at 370.
## How many hospitals does each state have? (Top)
}
